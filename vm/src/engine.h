#ifndef __ENGINE__H
#define __ENGINE__H

#include "vmtypes.h"

typedef enum {
  Lb, Lb1, Lb2, Lb3, Lb4, Lb5,
  Ib, Ob
} reg_bank_t;

/* Returns a string identifying the memory system */
char* engine_get_identity(void);

/* Setup the interpreter */
void engine_setup(void);

/* Tear down the interpreter */
void engine_cleanup(void);

/* Add an instruction to the code area of the memory */
void engine_emit(instr_t instr);

/* Get the next address in the code area of the memory */
void* engine_get_next_address();

/* Return the heap address of the register bank */
value_t* engine_get_base_register(reg_bank_t bank);

/* Set the heap address of the register bank */
void engine_set_base_register(reg_bank_t bank, value_t* new_value);

/* Interpret the program in the code area of the memory */
value_t engine_run();

#endif // __ENGINE__H
