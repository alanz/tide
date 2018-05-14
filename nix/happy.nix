{ mkDerivation, array, base, Cabal, containers, directory, filepath
, mtl, process, stdenv
}:
mkDerivation {
  pname = "happy";
  version = "1.20.0";
  src = ../submodules/happy;
  isLibrary = false;
  isExecutable = true;
  setupHaskellDepends = [ base Cabal directory filepath ];
  executableHaskellDepends = [ array base containers mtl ];
  testHaskellDepends = [ base process ];
  homepage = "https://www.haskell.org/happy/";
  description = "Happy is a parser generator for Haskell";
  license = stdenv.lib.licenses.bsd2;
}
