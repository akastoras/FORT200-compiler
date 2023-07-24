CC = gcc
LEXER = lexer.l
PARSER = parser.y
BIN = compiler.bin
CFLAGS = -Wall
LFLAGS = -lm -lfl

all: $(BIN)

lex.yy.c: $(LEXER)
	flex $^

parser.tab.c: $(PARSER)
	bison -vd $^

compiler.bin: parser.tab.c lex.yy.c
	gcc $(CFLAGS) $^ -o $@ $(LFLAGS)

run: all
	./$(BIN) tests/test1.f

clean:
	rm *.bin *.yy.c *.tab.* parser.output
