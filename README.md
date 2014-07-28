detector-characterization
=========================

HasKAL : Tools for gravitational wave detector characterization.
HasKAL stands for "Haskell-based KAGRA Analysis Library"

Frameutil requires the Frame Library http://lappweb.in2p3.fr/virgo/FrameL/ .
You can install the Frame Library as follows.

```
> # goto http://lappweb.in2p3.fr/virgo/FrameL/ and download the recommended version
> tar xf libframe-8.20.tar.gz
> cd libframe-8.20/
> ./configure; make; make install
```

And you should set enviromental variable HASKALOPT to use HasKAL module easily.
You replace "somewhere" with the PATH to git pull.
```
setenv HASKALOPT /somewhere/detecotor-characterization/optFiles
```
