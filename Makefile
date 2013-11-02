VERSION=0.2
all: *.lhs *.hs
	cabas install -f Server --bindir .
intricacy: *.lhs *.hs
	cabal install --bindir .
intricacy-curses: *.lhs *.hs
	cabal configure -f -SDL --bindir .
	cabal build
	mv dist/build/intricacy/intricacy intricacy-curses
intricacy-checker: *.lhs *.hs
	ghc -o intricacy-checker --make -auto-all Checker.hs
intricacy-server: *.lhs *.hs
	cabal configure -f Server -f -Game --bindir .
	cabal build
	mv dist/build/intricacy-server/intricacy-server .
intricacy-prof: *.lhs *.hs
	ghc -o intricacy-prof --make -rtsopts -prof -auto-all MainBoth.hs
intricacy.exe: *.lhs *.hs
	wine cabal configure
	# I wish I were joking:
	sudo mv /usr/i386-mingw32 /usr/i386-mingw32.t
	wine cabal build || true
	sudo mv /usr/i386-mingw32.t /usr/i386-mingw32
	mv dist/build/intricacy/intricacy.exe .
intricacy-${VERSION}.zip: intricacy.exe tutorial/*.lock
	mkdir intricacy-${VERSION} || true
	mkdir intricacy-${VERSION}/tutorial || true
	cp intricacy.exe VeraMono.ttf SDL.dll intricacy-${VERSION}
	cp tutorial/*.lock intricacy-${VERSION}/tutorial
	zip intricacy-${VERSION}.zip intricacy-${VERSION}
# DO NOT DELETE: Beginning of Haskell dependencies
Hex.o : Hex.lhs
Intricacy.o : Intricacy.hs
Intricacy.o : Hex.hi
# DO NOT DELETE: End of Haskell dependencies
