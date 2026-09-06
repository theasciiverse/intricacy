# Intricacy

<i>A game of competitive puzzle-design.</i>

By ruthlessly guarded secret arrangement, the council's agents can pick any
lock in the city. A secret guild produces the necessary locks - apparently
secure, but with fatal hidden flaws.

Coordinate a pair of lockpicks in a mechanistic turn-based puzzle game;
construct intricate locks of your own to protect the secrets you discover.

Free software, released under the GPLv3.

Written in Haskell, with SDL and Curses user interfaces.

[![Picking a lock](screenshots/playing.small.png)](screenshots/playing.png)
[![Initiation](screenshots/initiation.small.png)](screenshots/initiation.png)
[![Editing a lock](screenshots/editing.small.png)](screenshots/editing.png)
[![Metagame](screenshots/metagame.small.png)](screenshots/metagame.png)
[![Curses mode](screenshots/curses.small.png)](screenshots/curses.png)

## Download

Latest version is 0.9.1.1 ([NEWS](NEWS.md)).

* [Source tarball](intricacy-0.9.1.1.tar.gz)
* Source repository: <tt>git clone https://thegonz.net/~intricacy/intricacy.git</tt>
* Cabal: <tt>cabal update && cabal install intricacy</tt>
    <small>(requires the haskell package manager 'cabal', and SDL2 dependencies).</small>
* [Windows binary](intricacy-0.9.1.1-x86_64-mingw.zip)
* [Mac OS X binary](intricacy-0.5.7-mac32.zip)
    (intricacy version 0.5.7) - thanks to Kevin Eaves for providing this package.
* Gentoo: <tt>eselect repository enable haskell && emerge intricacy</tt>
<!--
No longer exists
* Arch Linux [AUR package](https://aur.archlinux.org/packages/intricacy/) 
-->
* [Old versions](old)

## Play by ssh
If you'd like to test out the game without installing anything, have access to
ssh, and are happy with a text-only interface, try:

* ssh intricacy@thegonz.net
* password: intricacy

## RSS Feed
Subscribe to this for updates on locks being set and solved:
[Feed for events on main 
server](https://intricacy.thegonz.net/mainServerFeed/rss).

## Discuss
<!--
* [Web forum](http://intricacy.thegonz.net/forum/)
(Broken due to PHP updates...)
-->
* Mailing list: mail mbays+intricacy@sdf.org with subject line '[intricacy-discuss] subscribe'.
* IRC channel: #intricacy on irc.libera.chat

<!--
##Introductory video
Download: [47MB ogg theora 15m](intricacy.ogv)

Stream: tell your video player to stream from
    <tt>http://mbays.freeshell.org/intricacy/intricacy.ogv</tt>,
or [use youtube](http://www.youtube.com/watch?v=lmuSNe4bGAM).
-->
