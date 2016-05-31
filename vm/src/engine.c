#include <assert.h>
#include <stdio.h>

#include "vmtypes.h"
#include "engine.h"
#include "opcode.h"
#include "memory.h"
#include "fail.h"

static instr_t* instr_memory_start;
static instr_t* instr_memory_end;

static value_t* value_memory_start;

static instr_t* curr_code_ptr;
static value_t* R[8];           /* (pseudo)base registers */

/* Returns a string identifying the memory system */
char* engine_get_identity(void) {
  return "threaded";
}

/* Setup the interpreter */
void engine_setup(void) {
  instr_memory_start = memory_get_start();
  instr_memory_end = memory_get_end();

  value_memory_start = memory_get_start();

  curr_code_ptr = instr_memory_start;
}

/* Tear down the interpreter */
void engine_cleanup(void) {
  // nothing to do
}

/* Add an instruction to the code area of the memory */
void engine_emit(instr_t instr) {
  if (curr_code_ptr + 1 > instr_memory_end)
    fail("not enough memory to load code");
  *curr_code_ptr = instr;
  curr_code_ptr += 1;
}

/* Get the next address in the code area of the memory */
void* engine_get_next_address(void) {
  return curr_code_ptr;
}

/* Convert an integer value that we stored on the heap (virtual pointer) to a memory address (physical pointer) */
static value_t* addr_v_to_p(value_t v_addr) {
  // TODO: When copying to the GC code, add your own assertions
  // specific to the heap invariants you are using, such as:
  // assert(value_t % size_of(value_t) == 0, "Invalid virtual pointer");
  return value_memory_start + (v_addr / sizeof(value_t));
}

/* Convert a heap memory address (physical pointer) to an integer that can be stored on the heap (virtual pointer) */
static value_t addr_p_to_v(value_t* p_addr) {
  // TODO: When copying to the GC code, add your own assertions
  return (p_addr - value_memory_start) * sizeof(value_t);
}

/* Instruction decoding method */
static reg_bank_t reg_bank(reg_id_t r) {
  return r >> 5;
}

/* Instruction decoding method */
static unsigned int reg_index(reg_id_t r) {
  return r & 0x1F;
}

/* Instruction decoding method */
static unsigned int instr_extract_u(instr_t instr, int start, int len) {
  return (instr >> start) & ((1 << len) - 1);
}

/* Instruction decoding method */
static int instr_extract_s(instr_t instr, int start, int len) {
  int bits = instr_extract_u(instr, start, len);
  int m = 1 << (len - 1);
  return (bits ^ m) - m;
}

/* Instruction decoding method */
static opcode_t instr_opcode(instr_t instr) {
  return (opcode_t)instr_extract_u(instr, 26, 6);
}

/* Instruction decoding method */
static reg_id_t instr_ra(instr_t instr) {
  return (reg_id_t)instr_extract_u(instr, 18, 8);
}

/* Instruction decoding method */
static reg_id_t instr_rb(instr_t instr) {
  return (reg_id_t)instr_extract_u(instr, 10, 8);
}

/* Instruction decoding method */
static reg_id_t instr_rc(instr_t instr) {
  return (reg_id_t)instr_extract_u(instr, 2, 8);
}

/* Instruction decoding method */
static int instr_d(instr_t instr) {
  return instr_extract_s(instr, 0, 10);
}

static int signum(int x) {
  return (x > 0) - (x < 0);
}

/* floor division and modulus
 * (see "Division and Modulus for Computer Scientists" by Daan Leijen)
 */
static int floorDiv(int x, int y) {
  int rt = x % y;
  int i = signum(rt) == -signum(y);
  return (x / y) - i;
}

static int floorMod(int x, int y) {
  int rt = x % y;
  int i = signum(rt) == -signum(y);
  return rt + i * y;
}

/* Return the heap address of the register bank */
value_t* engine_get_base_register(reg_bank_t bank) {
  assert(bank == Lb || bank == Ib || bank == Ob);
  return R[bank];
}

/* Set the heap address of the register bank */
void engine_set_base_register(reg_bank_t bank, value_t* new_value) {
  assert(bank == Lb || bank == Ib || bank == Ob);
  if (bank == Lb) {
    for (int pseudo_bank = Lb; pseudo_bank <= Lb5; ++pseudo_bank)
      R[pseudo_bank] = new_value + (pseudo_bank - Lb) * 32;
  } else
    R[bank] = new_value;
}

#define Ra(instr) (R[reg_bank(instr_ra(instr))][reg_index(instr_ra(instr))])
#define Rb(instr) (R[reg_bank(instr_rb(instr))][reg_index(instr_rb(instr))])
#define Rc(instr) (R[reg_bank(instr_rc(instr))][reg_index(instr_rc(instr))])

#define GOTO_NEXT goto *labels[instr_opcode(*pc)]

/* Interpret the program in the code area of the memory */
value_t engine_run() {
  instr_t* pc = (instr_t*)addr_v_to_p(0);
  engine_set_base_register(Lb, addr_v_to_p(0));
  engine_set_base_register(Ib, addr_v_to_p(0));
  engine_set_base_register(Ob, addr_v_to_p(0));

  void** labels[OPCODE_COUNT];
  labels[opcode_ADD] = &&l_ADD;
  labels[opcode_SUB] = &&l_SUB;
  labels[opcode_MUL] = &&l_MUL;
  labels[opcode_DIV] = &&l_DIV;
  labels[opcode_MOD] = &&l_MOD;
  labels[opcode_ASL] = &&l_ASL;
  labels[opcode_ASR] = &&l_ASR;
  labels[opcode_AND] = &&l_AND;
  labels[opcode_OR] = &&l_OR;
  labels[opcode_XOR] = &&l_XOR;
  labels[opcode_JLT] = &&l_JLT;
  labels[opcode_JLE] = &&l_JLE;
  labels[opcode_JEQ] = &&l_JEQ;
  labels[opcode_JNE] = &&l_JNE;
  labels[opcode_JGE] = &&l_JGE;
  labels[opcode_JGT] = &&l_JGT;
  labels[opcode_JI] = &&l_JI;
  labels[opcode_TCAL] = &&l_TCAL;
  labels[opcode_CALL] = &&l_CALL;
  labels[opcode_RET] = &&l_RET;
  labels[opcode_HALT] = &&l_HALT;
  labels[opcode_LDLO] = &&l_LDLO;
  labels[opcode_LDHI] = &&l_LDHI;
  labels[opcode_MOVE] = &&l_MOVE;
  labels[opcode_RALO] = &&l_RALO;
  labels[opcode_BALO] = &&l_BALO;
  labels[opcode_BSIZ] = &&l_BSIZ;
  labels[opcode_BTAG] = &&l_BTAG;
  labels[opcode_BGET] = &&l_BGET;
  labels[opcode_BSET] = &&l_BSET;
  labels[opcode_BREA] = &&l_BREA;
  labels[opcode_BWRI] = &&l_BWRI;

  GOTO_NEXT;

 l_ADD: {
    Ra(*pc) = Rb(*pc) + Rc(*pc);
    pc += 1;
  } GOTO_NEXT;

 l_SUB: {
    Ra(*pc) = Rb(*pc) - Rc(*pc);
    pc += 1;
  } GOTO_NEXT;

 l_MUL: {
    Ra(*pc) = Rb(*pc) * Rc(*pc);
    pc += 1;
  } GOTO_NEXT;

 l_DIV: {
    Ra(*pc) = floorDiv(Rb(*pc), Rc(*pc));
    pc += 1;
  } GOTO_NEXT;

 l_MOD: {
    Ra(*pc) = floorMod(Rb(*pc), Rc(*pc));
    pc += 1;
  } GOTO_NEXT;

 l_ASL: {
    Ra(*pc) = Rb(*pc) << Rc(*pc);
    pc += 1;
  } GOTO_NEXT;

 l_ASR: {
    Ra(*pc) = Rb(*pc) >> Rc(*pc);
    pc += 1;
  } GOTO_NEXT;

 l_AND: {
    Ra(*pc) = Rb(*pc) & Rc(*pc);
    pc += 1;
  } GOTO_NEXT;

 l_OR: {
    Ra(*pc) = Rb(*pc) | Rc(*pc);
    pc += 1;
  } GOTO_NEXT;

 l_XOR: {
    Ra(*pc) = Rb(*pc) ^ Rc(*pc);
    pc += 1;
  } GOTO_NEXT;

 l_JLT: {
    pc += (Ra(*pc) < Rb(*pc) ? instr_d(*pc) : 1);
  } GOTO_NEXT;

 l_JLE: {
    pc += (Ra(*pc) <= Rb(*pc) ? instr_d(*pc) : 1);
  } GOTO_NEXT;

 l_JEQ: {
    pc += (Ra(*pc) == Rb(*pc) ? instr_d(*pc) : 1);
  } GOTO_NEXT;

 l_JNE: {
    pc += (Ra(*pc) != Rb(*pc) ? instr_d(*pc) : 1);
  } GOTO_NEXT;

 l_JGE: {
    pc += (Ra(*pc) >= Rb(*pc) ? instr_d(*pc) : 1);
  } GOTO_NEXT;

 l_JGT: {
    pc += (Ra(*pc) > Rb(*pc) ? instr_d(*pc) : 1);
  } GOTO_NEXT;

 l_JI: {
    pc += instr_extract_s(*pc, 0, 26);
  } GOTO_NEXT;

 l_TCAL: {
    instr_t* target_pc = (instr_t*)addr_v_to_p(Ra(*pc));
    R[Ob][0] = R[Ib][0];
    R[Ob][1] = R[Ib][1];
    R[Ob][2] = R[Ib][2];
    R[Ob][3] = R[Ib][3];
    engine_set_base_register(Ib, R[Ob]);
    engine_set_base_register(Lb, addr_v_to_p(0));
    engine_set_base_register(Ob, addr_v_to_p(0));
    pc = target_pc;
  } GOTO_NEXT;

 l_CALL: {
    instr_t* target_pc = (instr_t*)addr_v_to_p(Ra(*pc));
    R[Ob][0] = addr_p_to_v(R[Ib]);
    R[Ob][1] = addr_p_to_v(R[Lb]);
    R[Ob][2] = addr_p_to_v(R[Ob]);
    R[Ob][3] = addr_p_to_v((value_t*)(pc + 1));
    engine_set_base_register(Ib, R[Ob]);
    engine_set_base_register(Lb, addr_v_to_p(0));
    engine_set_base_register(Ob, addr_v_to_p(0));
    pc = target_pc;
  } GOTO_NEXT;

 l_RET: {
    value_t ret_value = R[Ib][4];
    instr_t* target_pc = (instr_t*)addr_v_to_p(R[Ib][3]);
    engine_set_base_register(Ob, addr_v_to_p(R[Ib][2]));
    engine_set_base_register(Lb, addr_v_to_p(R[Ib][1]));
    engine_set_base_register(Ib, addr_v_to_p(R[Ob][0]));
    R[Ob][0] = ret_value;
    pc = target_pc;
  } GOTO_NEXT;

 l_HALT: {
    return Ra(*pc);
  }

 l_LDLO: {
    Ra(*pc) = instr_extract_s(*pc, 0, 18);
    pc += 1;
  } GOTO_NEXT;

 l_LDHI: {
    Ra(*pc) = (instr_extract_u(*pc, 0, 16) << 16) | (Ra(*pc) & 0xFFFF);
    pc += 1;
  } GOTO_NEXT;

 l_MOVE: {
    Ra(*pc) = Rb(*pc);
    pc += 1;
  } GOTO_NEXT;

 l_RALO: {
    value_t* block = memory_allocate(tag_RegisterFrame,
                                     instr_extract_u(*pc, 16, 8));
    reg_bank_t banks[] = { Lb, Ib, Ob };
    engine_set_base_register(banks[instr_extract_u(*pc, 24, 2)], block);
    pc += 1;
  } GOTO_NEXT;

 l_BALO: {
    value_t* block = memory_allocate(instr_extract_u(*pc, 2, 8), Rb(*pc));
    Ra(*pc) = addr_p_to_v(block);
    pc += 1;
  } GOTO_NEXT;

 l_BSIZ: {
    Ra(*pc) = memory_get_block_size(addr_v_to_p(Rb(*pc)));
    pc += 1;
  } GOTO_NEXT;

 l_BTAG: {
    Ra(*pc) = memory_get_block_tag(addr_v_to_p(Rb(*pc)));
    pc += 1;
  } GOTO_NEXT;

 l_BGET: {
    Ra(*pc) = addr_v_to_p(Rb(*pc))[Rc(*pc)];
    pc += 1;
  } GOTO_NEXT;

 l_BSET: {
    addr_v_to_p(Rb(*pc))[Rc(*pc)] = Ra(*pc);
    pc += 1;
  } GOTO_NEXT;

 l_BREA: {
    uint8_t byte;
    size_t read = fread(&byte, sizeof(byte), 1, stdin);
    Ra(*pc) = (read == sizeof(byte) ? byte : -1);
    pc += 1;
  } GOTO_NEXT;

 l_BWRI: {
    uint8_t byte = Ra(*pc);
    fwrite(&byte, sizeof(byte), 1, stdout);
    fflush(stdout);
    pc += 1;
  } GOTO_NEXT;
}
