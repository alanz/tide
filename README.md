This repo is initially an implementation of
http://blog.haskell-exists.com/yuras/posts/incremental-lexer.html

The @alanz fork is being used to experiment with incremental parsing via
`happy`, to explore ways of managing eventual integration into GHC, using the
haskell-ide-engine virtual file system.

This already does all the heavy lifting, in that it has the source code in a
fingertree representation.

The incremental parser generator is at https://github.com/alanz/happy

