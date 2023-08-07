CC = gcc
LEXER = lexer.l
PARSER = parser.y
BIN = compiler.bin
CFLAGS = -Wall
LFLAGS = -lm -lfl

all: $(BIN)

hashtbl.o: hashtbl.c hashtbl.h
	gcc -o hashtbl.o -c hashtbl.c

lex.yy.c: $(LEXER) constants.h
	flex $<

parser.tab.c: $(PARSER) constants.h
	bison -vd $< 
#-Wno-other -Wcounterexamples

compiler.bin: parser.tab.c lex.yy.c  hashtbl.o
	gcc $(CFLAGS) $^ -o $@ $(LFLAGS)

run: all
	./$(BIN) tests/test1.f

clean:
	rm *.bin *.yy.c *.tab.* parser.output
