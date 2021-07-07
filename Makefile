CFLAGS = -Iinclude -g -Wall -pedantic -std=c99

SOURCES = $(wildcard src/*.c)
INCLUDE = $(wildcard include/*.h)
OBJECTS = $(patsubst src/%.c, build/%.o, $(SOURCES))

ARFLAGS = -crU

libssm.a : libssm.a($(OBJECTS))

$(OBJECTS) : build

build :
	mkdir build

(%) : %
	$(AR) $(ARFLAGS) $@ $%

build/%.o : src/%.c
	$(CC) $(CFLAGS) -c -o $@ $< 


documentation : doc/html/index.html

doc/html/index.html : doc/Doxyfile
	rm -rf doc/html doc/latex
	cd doc && doxygen


EXE = fib fib2 fib3 counter counter2 clock onetwo

all : $(EXE)

scheduler.o fib.o counter.o counter2.o clock.o : ssm.h

fib : fib.o scheduler.o
	$(CC) $(CFLAGS) -o $@ $^

fib2 : fib2.o scheduler.o
	$(CC) $(CFLAGS) -o $@ $^

fib3 : fib3.o scheduler.o
	$(CC) $(CFLAGS) -o $@ $^

counter : counter.o scheduler.o
	$(CC) $(CFLAGS) -o $@ $^

counter2 : counter2.o scheduler.o
	$(CC) $(CFLAGS) -o $@ $^

clock : clock.o scheduler.o
	$(CC) $(CFLAGS) -o $@ $^

onetwo : onetwo.o scheduler.o
	$(CC) $(CFLAGS) -o $@ $^

.PHONY : clean
clean :
	rm -rf *.o *.gch $(EXE) build
