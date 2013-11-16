VERSION=0.2
LINTARGET=linux-i686-glibc-2.16
sources=$(filter-out Paths_intricacy.hs,$(wildcard *.hs *.lhs))
all: $(sources)
	cabal install -O0 -f Server --bindir .
intricacy: $(sources)
	#cabal install -O0 --bindir .
	ghc -o intricacy --make MainBoth.hs
intricacy-O2: $(sources)
	ghc -o intricacy-O2 -O2 --make MainBoth.hs
intricacy-curses: $(sources)
	cabal configure -f -SDL --bindir .
	cabal build
	mv dist/build/intricacy/intricacy intricacy-curses
intricacy-checker: $(sources)
	ghc -o intricacy-checker --make -O2 -rtsopts -prof -auto-all Checker.hs
intricacy-server: $(sources)
	cabal configure -f Server -f -Game --bindir .
	cabal build
	mv dist/build/intricacy-server/intricacy-server .
intricacy-server-O2: $(sources)
	ghc -o intricacy-server-O2 --make Server.hs
intricacy-prof: $(sources)
	ghc -o intricacy-prof --make -rtsopts -prof -auto-all MainBoth.hs
intricacy-static: $(sources)
	cabal configure --ghc-options="-static" --bindir=. --datadir=.
	cabal build
	mv dist/build/intricacy/intricacy ./intricacy-static
intricacy-server-static: $(sources)
	cabal configure -f Server -f -Game --ghc-options="-static" --bindir=. --datadir=.
	cabal build
	mv dist/build/intricacy-server/intricacy-server ./intricacy-server-static
intricacy-latest-${LINTARGET}.tgz: intricacy-${VERSION}-${LINTARGET}.tgz
	ln -s $< $@
intricacy-${VERSION}-${LINTARGET}.tgz: intricacy-static intricacy-server-static tutorial/*.lock
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
	tar -czvf intricacy-${VERSION}-${LINTARGET}.tgz intricacy-${VERSION}
	mv linuxbintmp/intricacy-${VERSION}-${LINTARGET}.tgz .
	rm -rf linuxbintmp
intricacy.exe: $(sources)
	echo $^
	wine cabal configure
	# I wish I were joking:
	sudo mv /usr/i386-mingw32 /usr/i386-mingw32.t
	wine cabal build || true
	sudo mv /usr/i386-mingw32.t /usr/i386-mingw32
	mv dist/build/intricacy/intricacy.exe .
intricacy-latest-win32.zip: intricacy-${VERSION}-win32.zip
	ln -s $< $@
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
modules.dot: $(sources)
	graphmod *.lhs *.hs > modules.dot
modules.png: modules.dot
	dot -Tpng -o modules.png modules.dot
index.html: index.md
	markdown_py < index.md > index.html
publish: intricacy-${VERSION}-win32.zip intricacy-${VERSION}-${LINTARGET}.tgz
	rsync $^ sverige:html/intricacy/
	ssh sverige "cd html/intricacy && git pull && make index.html &&\
	    git update-server-info &&\
	    ln -sf intricacy-${VERSION}-win32.zip intricacy-latest-win32.zip &&\
	    ln -sf intricacy-${VERSION}-${LINTARGET}.tgz intricacy-latest-${LINTARGET}.tgz"
