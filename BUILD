Compiling Intricacy
===================

Dependencies:
    EITHER
	sdl
	sdl-ttf
	sdl-gfx
    OR
	curses

To compile the game on *nix with SDL graphics:
    Install ghc, cabal, and development packages for the SDL dependencies
    above, then run
	cabal update && cabal install
    which should result in various haskell packages being compiled and
    installed, probably in ~/.cabal/ . The game can then be run as
	~/.cabal/bin/intricacy 
    Running cabal install as root should install it somewhere global.

To compile a curses-only (ascii graphics) build:
	cabal install -f -SDL

To compile the server:
	cabal install -f Server -f -Game

    The server will be installed in
	~/.cabal/bin/intricacy-server
    The server runs on port 27001 by default. It writes the game database to
    a directory 'intricacydb' under the directory from which it is run.

To compile for windows:
    This should work as above once you have the dependencies installed
    properly. Good luck with that. The mingw32-compiled libraries in winlibs
    may or may not help.

To compile for OSX, or android or whatever other wacky system:
    No idea, sorry. But please tell me if you manage!
