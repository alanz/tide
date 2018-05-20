
.PHONY: all
all :: src/Lexer.hs src/Play.hs

src/Lexer.hs: grammar/Lexer.x
	alex --ghc  --debug  --info=Lexer.info \
    -o src/Lexer.hs \
    grammar/Lexer.x

src/Play.hs: grammar/Play.y
	happy --ghc --incremental --debug  --info=Play.info \
    -o src/Play.hs \
    grammar/Play.y
