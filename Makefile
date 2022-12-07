CC = gcc
LEXER = lexer.l
BIN = lexer.lexer.bin
CFLAGS = -Wall
LFLAGS = -lm -lfl

all: $(BIN)

lex.yy.c: $(LEXER)
	flex $^

%.lexer.bin: lex.yy.c
	gcc $(CFLAGS) $^ -o $@ $(LFLAGS)

run: all
	./$(BIN) tests/test1.f

clean:
	rm *.lexer.bin *.yy.c
