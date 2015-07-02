/* Workaround for SDL on OS X, based on the corresponding file in TimePiece-0.0.5
 *   http://hackage.haskell.org/package/TimePiece ,
 * placed in the public domin by by Audrey Tang <audreyt@audreyt.org>. */
#include <stdio.h>
#include "HsFFI.h"

#ifdef __APPLE__
#include <objc/objc.h>
#include <objc/objc-runtime.h>
#endif

#include <SDL.h>

#ifdef __GLASGOW_HASKELL__
#include "Main_stub.h"
extern void __stginit_Main ( void );
#endif

#ifdef main
int SDL_main(int argc, char *argv[])
#else
int main(int argc, char *argv[])
#endif
{
    int i;

#ifdef __APPLE__
    void * pool =
      objc_msgSend(objc_lookUpClass("NSAutoreleasePool"), sel_getUid("alloc"));
    objc_msgSend(pool, sel_getUid("init"));
#endif

    hs_init(&argc, &argv);
#ifdef __GLASGOW_HASKELL__
    hs_add_root(__stginit_Main);
#endif

    hs_MAIN();

    hs_exit();

#ifdef __APPLE__
    objc_msgSend(pool, sel_getUid("release"));
#endif
    return 0;
}
