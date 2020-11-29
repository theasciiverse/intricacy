VERSION=0.7.2

LINTARGET=linux-i686-glibc-2.16
sources=$(filter-out Paths_intricacy.hs,$(wildcard *.hs *.lhs))
all: $(sources)
	cabal install -O0 -f Server --bindir .
intricacy: $(sources)
	#cabal install -O0 --bindir .
	ghc -o intricacy -DMAIN_SDL -DMAIN_CURSES --make Intricacy.hs
intricacy-O2: $(sources)
	ghc -o intricacy-O2 -DMAIN_SDL -DMAIN_CURSES -O2 --make Intricacy.hs
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
	ghc -o intricacy-prof -DMAIN_SDL -DMAIN_CURSES --make -rtsopts -prof -auto-all Intricacy.hs
intricacy-static: $(sources)
	cabal configure --ghc-options="-static" --bindir=. --datadir=.
	cabal build
	mv dist/build/intricacy/intricacy ./intricacy-static
intricacy-server-static: $(sources)
	cabal configure -f Server -f -Game --ghc-options="-static" --bindir=. --datadir=.
	cabal build
	mv dist/build/intricacy-server/intricacy-server ./intricacy-server-static
modules.dot: $(sources)
	graphmod *.lhs *.hs > modules.dot
modules.png: modules.dot
	dot -Tpng -o modules.png modules.dot
index.html: index.md
	markdown_py < index.md > index.html
