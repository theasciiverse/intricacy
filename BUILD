Compiling Intricacy
===================

Dependencies:
    EITHER
        sdl2
        sdl2-ttf
        sdl2-gfx
        sdl2-mixer
    OR
        curses

To compile the game on *nix with SDL graphics:
    Install ghc, cabal, and development packages for the SDL dependencies
    above, then run
	cabal update && cabal install
    the intricacy binary should then be installed, possibly to
	~/.cabal/bin/intricacy 
    Running cabal install as root should install it somewhere global.

To compile a curses-only (ascii graphics) build:
	cabal install -f curses

To compile the server:
	cabal install -f server -f -game

    The server will be installed as e.g.
	~/.cabal/bin/intricacy-server
    The server runs on port 27001 by default. It writes the game database to
    a directory 'intricacydb' under the directory from which it is run.

    Run 'intricacy-server -h' for various options.

To compile for windows:
    This should work as above once you have the dependencies installed
    properly. Good luck with that.

To compile for OSX, or android or whatever other wacky system:
    No idea, sorry. But please tell me if you manage!
