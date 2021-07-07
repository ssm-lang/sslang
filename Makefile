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


.PHONY : clean
clean :
	rm -rf *.gch build doc/html doc/latexq libssm.a
