# custom variables
_hkgname=intricacy
_licensefile=COPYING

# PKGBUILD options/directives
pkgname=intricacy
pkgver=0.4.2
pkgrel=1
pkgdesc="A game of competitive puzzle-design"
url="http://mbays.freeshell.org/intricacy"
license=("GPL-3")
arch=('i686' 'x86_64')
makedepends=('ghc')
depends=()
options=('strip')
source=("http://hackage.haskell.org/packages/archive/${_hkgname}/${pkgver}/${_hkgname}-${pkgver}.tar.gz")

sha256sums=("ffe62181ad55592a7fc07d3f08790f80a4683883b9eb0bdba7745636ad167cfb")

# PKGBUILD functions

prepare() {
    cd "${srcdir}/${_hkgname}-${pkgver}"
    
    # no cabal patch
    # no source patch
}

build() {
    cd "${srcdir}/${_hkgname}-${pkgver}"
    
    runhaskell Setup configure -O --prefix=/usr --docdir="/usr/share/doc/${pkgname}" \
        -f-server -fgame -fcurses -fsdl
    runhaskell Setup build
}

package() {
    cd "${srcdir}/${_hkgname}-${pkgver}"
    runhaskell Setup copy --destdir="${pkgdir}"
}
