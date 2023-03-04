VERSION=0.8.1.1

sources=$(filter-out Paths_intricacy.hs,$(wildcard *.hs *.lhs))

intricacy: $(sources)
	cabal install -O2 -f -server --installdir . --overwrite-policy=always
install: $(sources)
	cabal install -O2 -f server
intricacy-curses: $(sources)
	cabal install -O2 -f -server -f -sdl -f curses --installdir . --overwrite-policy=always &&\
	mv intricacy intricacy-curses
intricacy-server: $(sources)
	cabal install -O2 -f server -f -game --installdir . --overwrite-policy=always

modules.dot: $(sources)
	graphmod *.lhs *.hs > modules.dot
modules.png: modules.dot
	dot -Tpng -o modules.png modules.dot
index.html: index.md
	multimarkdown < index.md > index.html
