CFLAGS = -g -Wall -pedantic -std=c99

EXE = fib fib2 fib3 counter counter2 clock

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

.PHONY : clean
clean :
	rm -rf *.o *.gch $(EXE)
