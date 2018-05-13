

src/Play.hs: grammar/Play.y
	happy --ghc --incremental --debug  --info=Play.info \
    -o src/Play.hs \
    grammar/Play.y
