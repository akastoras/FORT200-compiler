# Compilers

## Syntactic Analysis

### Session 2 (Thanos & Tolis)

1. Added associativities in `parser.y` T_NOTOP (at the end of the first part after token declerations)
2. Updated Makefile (with `make all` it now generates compiler.bin which runs the syntax analysis)
3. Wrote a new print function (print_token) for `lexer.l` to simplify suppressing error messages. To enable verbose lectical analysis uncomment the define in `constants.h`
4. Setting yylval (the union defined in `parser.y` associated with each token) from `lexer.l` for ID(strval), STRING(strval), ICONST(intval), RCONST(rval), CCONST(charval), LCONST(lval)
5. Saved `parser.output` in `parser_output_before_solving_conflicts.txt` that will be needed for the report
6. Declared the types of the nonterminal variables in `parser.y` as strval. This produces some warnings which we suppressed in bison with -Wno-other but should be checked in the near future
7. Added the function yyless(1) inside yyerror() in `lexer.l` which discards the next character from input during an error (may not be necessary)
8. TODO: Catch syntax errors and continue the analysis (2nd video tutorial 50:00 - 55:00). Left that out to continue with more useful things.