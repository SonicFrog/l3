#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <assert.h>
#include <stdbool.h>

#include "memory.h"
#include "fail.h"
#include "engine.h"

#if GC_VERSION == GC_MARK_N_SWEEP

#define HEADER_SIZE_MASK 0xFFFFFF00
#define HEADER_TAG_MASK 0xFF

#define IS_VADDR(v) ((v) & 0x3 == 0)
#define BLOCK_IS_FREE(b) (header_unpack_tag(b) == 255)

// Allocate blocks of at least 2 words
#define USER_TO_ACTUAL_SIZE(s) ((s == 0) ? 2 : (s + 1))
#define ACTUAL_TO_USER_SIZE(s) (s - 1)

#define USER_BLOCK_START(b) (b + 1)
#define ACTUAL_BLOCK_START(b) (b - 1)

static value_t *memory_start = NULL;
static value_t *memory_end = NULL;


static inline value_t header_pack(tag_t tag, unsigned int size) {
    assert(size <= 0xFFFFFF);
    assert(tag <= 0xFF);
    return (size << sizeof(tag_t) | tag);
}

static inline size_t header_unpack_size(value_t *block) {
    return (block[0] & HEADER_SIZE_MASK) >> sizeof(tag_t);
}

static inline tag_t header_unpack_tag(value_t *block) {
    return block[0] & HEADER_TAG_MASK;
}

/**
 * Start of blocks
 **/
static value_t *heap_start = NULL;
/**
 * Start of bitmap
 **/
static value_t *bitmap_start = NULL;

static value_t *freelist = NULL;


static value_t* addr_v_to_p(value_t v_addr) {
    return memory_start + (v_addr / sizeof(value_t));
}

static value_t addr_p_to_v(value_t* p_addr) {
    return (p_addr - memory_start) * sizeof(value_t);
}


/**
 * LIST FUNCTIONS
 **/
/**
 * Next element in the free list
 **/
static inline value_t* list_next(value_t *e) {
    return addr_v_to_p(e[1]);
}

/**
 * Sets the next element in the freelist
 **/
static inline void set_next(value_t *e, value_t *tail) {
    e[1] = addr_p_to_v(tail);
}

/**
 * Returns true if this list has no next element
 **/
static inline bool is_empty(value_t *e) {
    return addr_p_to_v(e) == addr_p_to_v(NULL);
}

static inline bool has_next(value_t *e) {
    return addr_v_to_p(e[1]) == NULL;
}

static inline bool in_heap(value_t vaddr) {
    value_t* p_addr = addr_v_to_p(vaddr);
    return heap_start >= p_addr && p_addr <= memory_end;
}


/**
 * BLOCK MANIPULATION FUNCTIONS
 **/
static value_t* find_block(unsigned int size) {
    size_t required_size = USER_TO_ACTUAL_SIZE(size);
    unsigned int best_fit_sz = 0;

    value_t *prev = NULL;
    value_t *prev_best = NULL;
    value_t *best = NULL;

    value_t *curr = freelist;

    do {
        size_t length = header_unpack_size(curr);

        if (length == required_size) {
                prev_best = prev;
                best = curr;
                break;
        } else if (length > required_size) {
            if (best_fit_sz > size) {
                best_fit_sz = size;
                prev_best = prev;
                best = curr;
            }
        }

        prev = curr;
        curr = list_next(curr);
    } while (has_next(curr));

    // remove allocated block from list
    if (prev_best == NULL) {
        freelist = list_next(best);
    } else {
        set_next(prev_best, list_next(best));
    }

    //TODO: handle shrinking block to required size!

    return best;
}

/**
 * BITMAP MANIPULATION FUNCTIONS
 **/

/**
 * Checks that the bit at bitpos in word is zero (not allocated)
 **/
#define IS_BIT_FREE(word, bitpos) (((~word) >> (bitpos)) & 1)
/**
 * Position of the word in the bitmap for the virtual address
 **/
#define WORD_POS(addr) (addr / VALUE_BITS)
/**
 * Position of the bit inside the word for the virtual address
 **/
#define BIT_POS(addr) ((addr) & (VALUE_BITS - 1))

static inline void make_bitmap() {
    assert(heap_start != NULL);
    assert(bitmap_start == NULL);

    size_t heap_size = memory_end - heap_start;
    size_t bitmap_size = (heap_size + VALUE_BITS - 1) / VALUE_BITS;

    bitmap_start = heap_start;
    heap_start = bitmap_start + bitmap_size;

    memset(bitmap_start, 0, bitmap_size * sizeof(value_t));
}

static inline size_t bitmap_pos(value_t vaddr) {
    assert(bitmap_start != NULL);
    assert(in_heap(vaddr));

    return (size_t) ((vaddr - addr_p_to_v(bitmap_start)) / sizeof(value_t));
}

static inline bool is_bit_marked(value_t vaddr) {
    assert(in_heap(vaddr));
    size_t pos = bitmap_pos(vaddr);
    return !IS_BIT_FREE(bitmap_start[WORD_POS(pos)], BIT_POS(pos));
}

static inline void mark_bit(value_t vaddr) {
    assert(in_heap(vaddr));
}

static inline void unmark_bit(value_t vaddr) {
    assert(in_heap(vaddr));
}

/**
 * GC INTERFACE
 **/
char* memory_get_identity() {
  return "mark & sweep garbage collector";
}

/* Setup the memory allocator and garbage collector */
void memory_setup(size_t total_byte_size) {
    memory_start = calloc(total_byte_size, 1);
    memory_end = memory_start + total_byte_size;
}

/* Tear down the memory */
void memory_cleanup() {
    assert(memory_start != NULL);
    free(memory_start);

    memory_start = memory_end = NULL;
    bitmap_start = heap_start = NULL;
}

/* Get first memory address */
void* memory_get_start() {
    return memory_start;
}

/* Get last memory address */
void* memory_get_end() {
    return memory_end;
}

/* Set the heap start, following the code area */
void memory_set_heap_start(void* ptr) {
    assert(heap_start == NULL);
    assert(ptr != NULL);

    freelist = heap_start = ptr;

    make_bitmap();

    size_t size = memory_end - heap_start >= 2;

    assert(2 <= size);

    freelist[0] = header_pack(255, ACTUAL_TO_USER_SIZE(size));
    set_next(freelist, NULL);
}

/* Allocate block, return physical pointer to the new block */
value_t* memory_allocate(tag_t tag, unsigned int size) {
    unsigned int rsize = USER_TO_ACTUAL_SIZE(size);

    assert(rsize >= 2);

    value_t *fblock = find_block(rsize);
    value_t *user_block = fblock + 1;

    if (fblock == NULL) {
        //TODO: mark and sweep

        fblock = find_block(rsize);

        if (fblock == NULL) {
            fail("Out of memory while allocation block of size %ud\n", size);
        }
    }

    fblock[0] = header_pack(tag, size);

    return user_block;
}

/* Unpack block size from a physical pointer */
size_t memory_get_block_size(value_t* block) {
    return header_unpack_size(ACTUAL_BLOCK_START(block));
}

/* Unpack block tag from a physical pointer */
tag_t memory_get_block_tag(value_t* block) {
    return header_unpack_tag(ACTUAL_BLOCK_START(block));
}

#endif
