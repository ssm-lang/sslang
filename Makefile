CFLAGS = -Iinclude -g -Wall -pedantic -std=c99 -DSSM_DEBUG

SOURCES = $(wildcard src/*.c)
INCLUDES = $(wildcard include/*.h)
OBJECTS = $(patsubst src/%.c, build/%.o, $(SOURCES))

EXAMPLES = $(wildcard examples/*.c)
EXAMPLEEXES = $(patsubst examples/%.c, build/%, $(EXAMPLES))

ARFLAGS = -crU

test-examples : examples
	./runexamples > build/examples.out
	(diff test/examples.out build/examples.out && echo "EXAMPLES PASSED") || echo "EXAMPLE OUTPUT DIFFERS"

build/test_main : test/test_main.c build/libssm.a
	$(CC) $(CFLAGS) -o $@ test/test_main.c -Lbuild -lssm

build/libssm.a : $(INCLUDES) $(OBJECTS)
	rm -f build/libssm.a
	$(AR) $(ARFLAGS) build/libssm.a $(OBJECTS)

$(OBJECTS) : $(INCLUDES) $(SOURCES)

build/%.o : src/%.c
	$(CC) $(CFLAGS) -c -o $@ $<

examples : $(EXAMPLEEXES)

build/% : examples/%.c build/libssm.a
	$(CC) $(CFLAGS) -o $@ $< -Lbuild -lssm



documentation : doc/html/index.html

doc/html/index.html : doc/Doxyfile $(SOURCES) $(INCLUDES)
	cd doc && doxygen


.PHONY : clean
clean :
	rm -rf *.gch build/* libssm.a
