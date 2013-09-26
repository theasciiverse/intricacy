intricacy: *.lhs *.hs
	ghc -o intricacy --make -auto-all Intricacy.hs
intricacy-checker: *.lhs *.hs
	ghc -o intricacy-checker --make -auto-all Checker.hs
intricacy-prof: *.lhs *.hs
	ghc -o intricacy --make -rtsopts -prof -auto-all Intricacy.hs
# DO NOT DELETE: Beginning of Haskell dependencies
Hex.o : Hex.lhs
Intricacy.o : Intricacy.hs
Intricacy.o : Hex.hi
# DO NOT DELETE: End of Haskell dependencies
