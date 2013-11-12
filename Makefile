VERSION=0.2
all: *.lhs *.hs
	cabal install -O0 -f Server --bindir .
intricacy: *.lhs *.hs
	#cabal install -O0 --bindir .
	ghc -o intricacy --make MainBoth.hs
intricacy-O2: *.lhs *.hs
	ghc -o intricacy-O2 -O2 --make MainBoth.hs
intricacy-curses: *.lhs *.hs
	cabal configure -f -SDL --bindir .
	cabal build
	mv dist/build/intricacy/intricacy intricacy-curses
intricacy-checker: *.lhs *.hs
	ghc -o intricacy-checker --make -O2 -rtsopts -prof -auto-all Checker.hs
intricacy-server: *.lhs *.hs
	cabal configure -f Server -f -Game --bindir .
	cabal build
	mv dist/build/intricacy-server/intricacy-server .
intricacy-server-O2: *.lhs *.hs
	ghc -o intricacy-server-O2 --make Server.hs
intricacy-prof: *.lhs *.hs
	ghc -o intricacy-prof --make -rtsopts -prof -auto-all MainBoth.hs
intricacy-static: *.lhs *.hs
	cabal configure --ghc-options="-static" --bindir=. --datadir=.
	cabal build
	mv dist/build/intricacy/intricacy ./intricacy-static
intricacy-server-static: *.lhs *.hs
	cabal configure -f Server -f -Game --ghc-options="-static" --bindir=. --datadir=.
	cabal build
	mv dist/build/intricacy-server/intricacy-server ./intricacy-server-static
intricacy-${VERSION}-linux-i686-glibc-2.16.tgz: intricacy-static intricacy-server-static tutorial/*.lock
	mkdir linuxbintmp
	cd linuxbintmp &&\
	mkdir intricacy-${VERSION} &&\
	mkdirhier intricacy-${VERSION}/intricacy-${VERSION}/tutorial &&\
	mkdir intricacy-${VERSION}/lib &&\
	cp ../{README,VeraMono.ttf} intricacy-${VERSION}/intricacy-${VERSION}/ &&\
	cp ../intricacy-static intricacy-${VERSION}/intricacy.bin &&\
	cp ../intricacy-server-static intricacy-${VERSION}/intricacy-server.bin &&\
	cp ../intricacy-lib-script intricacy-${VERSION}/intricacy &&\
	cp ../intricacy-server-lib-script intricacy-${VERSION}/intricacy-server &&\
	cp ../tutorial/*.lock intricacy-${VERSION}/intricacy-${VERSION}/tutorial &&\
	cp -L ../lib/* intricacy-${VERSION}/lib &&\
	tar -czvf intricacy-${VERSION}-linux-i686-glibc-2.16.tgz intricacy-${VERSION}
	mv linuxbintmp/intricacy-${VERSION}-linux-i686-glibc-2.16.tgz .
	rm -rf linuxbintmp
intricacy.exe: *.lhs *.hs
	wine cabal configure
	# I wish I were joking:
	sudo mv /usr/i386-mingw32 /usr/i386-mingw32.t
	wine cabal build || true
	sudo mv /usr/i386-mingw32.t /usr/i386-mingw32
	mv dist/build/intricacy/intricacy.exe .
intricacy-${VERSION}-win32.zip: intricacy.exe tutorial/*.lock
	mkdir w32tmp
	cd w32tmp &&\
	mkdir intricacy-${VERSION} &&\
	mkdir intricacy-${VERSION}/tutorial &&\
	cp ../{README,intricacy.exe,VeraMono.ttf,SDL.dll} intricacy-${VERSION} &&\
	cp ../tutorial/*.lock intricacy-${VERSION}/tutorial &&\
	zip -r intricacy-${VERSION}-win32.zip "intricacy-${VERSION}"
	mv w32tmp/intricacy-${VERSION}-win32.zip .
	rm -rf w32tmp
modules.dot: *.lhs *.hs
	graphmod *.lhs *.hs > modules.dot
modules.png: modules.dot
	dot -Tpng -o modules.png modules.dot
# DO NOT DELETE: Beginning of Haskell dependencies
Hex.o : Hex.lhs
Intricacy.o : Intricacy.hs
Intricacy.o : Hex.hi
# DO NOT DELETE: End of Haskell dependencies
