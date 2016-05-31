# Makefile to compile 

SRCS=src/engine.c \
     src/fail.c \
     src/loader.c \
     src/main.c \
     src/memory*.c

CFLAGS=-std=c11 -fwrapv -g -Wall -O3

default:
	@echo "Use the following targets:"
	@echo " - 'make vm' to use your mark-and-sweep GC"
	@echo " - 'make test' to test the VM"
	@echo " - 'make clean' to clean the VM"
	@echo ""
	@echo "Note: the GC_VERSION preprocessor variable controls which"
	@echo "      garbage collector is used in the virtual machine."

bin:
	mkdir -p bin

vm: clean bin/vm

bin/vm: bin ${SRCS}
	${CC} ${CFLAGS} ${LDFLAGS} ${SRCS} -o bin/vm

test: vm 
	@((echo 8   | bin/vm ../examples/asm/queens.asm  2>&1) >/dev/null && echo Queens test passed!) || echo Queens test failed!
	@((echo 110 | bin/vm ../examples/asm/bignums.asm 2>&1) >/dev/null && echo Bignums test passed!) || echo Bignums test failed!
	@((echo 30  | bin/vm ../examples/asm/pascal.asm  2>&1) >/dev/null && echo Pascal test passed!) || echo Pascal test failed!
	@((echo 10  | bin/vm ../examples/asm/maze.asm    2>&1) >/dev/null && echo Maze test passed!) || echo Maze test failed!

clean:
	rm -rf bin

all: clean vm
