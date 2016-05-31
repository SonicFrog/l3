#ifndef __FAIL_H
#define __FAIL_H

/* A method to indicate vm failure */
extern void fail(char* msg, ...) __attribute__ ((noreturn));

#endif // __FAIL_H
