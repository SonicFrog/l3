#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <assert.h>

#include "memory.h"
#include "fail.h"
#include "engine.h"

#if GC_VERSION == GC_MARK_N_SWEEP

/* Returns a string identifying the memory system */
char* memory_get_identity() {
  return "mark & sweep garbage collector";
}

/* Setup the memory allocator and garbage collector */
void memory_setup(size_t total_byte_size) {
  // TODO
}

/* Tear down the memory */
void memory_cleanup() {
  // TODO
}

/* Get first memory address */
void* memory_get_start() {
  return NULL;                  /* TODO */
}

/* Get last memory address */
void* memory_get_end() {
  return NULL;                  /* TODO */
}

/* Set the heap start, following the code area */
void memory_set_heap_start(void* ptr) {
  // TODO
}

/* Allocate block, return physical pointer to the new block */
value_t* memory_allocate(tag_t tag, unsigned int size) {
  return NULL;                  /* TODO */
}

/* Unpack block size from a physical pointer */
size_t memory_get_block_size(value_t* block) {
  return 0;                     /* TODO */
}

/* Unpack block tag from a physical pointer */
tag_t memory_get_block_tag(value_t* block) {
  return 0;                     /* TODO */
}

#endif
