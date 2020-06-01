CFLAGS = -g -Wall -pedantic -std=c99

EXE = fib counter counter2 clock

all : $(EXE)

scheduler.o fib.o counter.o counter2.o clock.o : ssm.h

fib : fib.o scheduler.o
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
