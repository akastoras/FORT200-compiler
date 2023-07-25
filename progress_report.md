# Compilers

## Syntactic Analysis

### Session 1 (Spiros & Ioanna)


### Session 2 (Thanos & Tolis)

#### Done

1. Added associativities in `parser.y` T_NOTOP (at the end of the first part after token declerations)
2. Updated Makefile (with `make all` it now generates compiler.bin which runs the syntax analysis)
3. Wrote a new print function (print_token) for `lexer.l` to simplify suppressing error messages. To enable verbose lectical analysis uncomment the define in `constants.h`
4. Setting yylval (the union defined in `parser.y` associated with each token) from `lexer.l` for ID(strval), STRING(strval), ICONST(intval), RCONST(rval), CCONST(charval), LCONST(lval)
5. Saved `parser.output` in `parser_output_before_solving_conflicts.txt` that will be needed for the report
6. Declared the types of the nonterminal variables in `parser.y` as strval. This produces some warnings which we suppressed in bison with -Wno-other but should be checked in the near future

#### TODO
* Catch lexical and syntax errors and continue the analysis (2nd video tutorial 50:00 - 55:00). Left that out to continue with more useful things. (DONE)
* Explain how the syntax conflicts were resolved in the `parser_output_before_solving_conflicts.txt` as requested by the homework description (Tolis & Thanos)

### Session 3 (Thanos & Tolis)

#### Done

1. Added two new regexes for matching common mistakes in ids and print specialized error messages in `lexer.l`. Could be done for other common mistakes in the future.
2. Added error cases in grammar in `parser.y` for matching common syntax mistakes.
	* Missing seperator ',' between labels, expressions and variable definitions
	* Missing operator between expressions
3. Needed to give error token the lowest priority since it can introduce conflicts with other rules.

#### TODO
* Add more error cases (in the long term)
* Implement the symbol table as shown in the last 40 minutes of video tutorial 2 (Spiros & Ioanna)