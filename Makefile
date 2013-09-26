intricacy: *.lhs *.hs
	ghc -o intricacy --make -auto-all MainSDL.hs
intricacy-curses: *.lhs *.hs
	ghc -o intricacy-curses --make -auto-all MainCurses.hs
intricacy-checker: *.lhs *.hs
	ghc -o intricacy-checker --make -auto-all Checker.hs
intricacy-prof: *.lhs *.hs
	ghc -o intricacy --make -rtsopts -prof -auto-all MainSDL.hs
# DO NOT DELETE: Beginning of Haskell dependencies
Hex.o : Hex.lhs
Intricacy.o : Intricacy.hs
Intricacy.o : Hex.hi
# DO NOT DELETE: End of Haskell dependencies
