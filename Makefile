lock: Hex.lhs Intricacy.hs
	ghc --make -rtsopts -prof -auto-all Intricacy.hs
# DO NOT DELETE: Beginning of Haskell dependencies
Hex.o : Hex.lhs
Intricacy.o : Intricacy.hs
Intricacy.o : Hex.hi
# DO NOT DELETE: End of Haskell dependencies
