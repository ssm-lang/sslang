# sslang

A language built atop the Sparse Synchronous Model

This depends on the Haskell Stack and the SSM library

See <https://docs.haskellstack.org/en/stable/GUIDE/> for Haskell Stack
documentation

Build the compiler (sslc) with

````
stack build
````


Test with, e.g.,

````
cd regression-tests ; ./runtests.sh
stack test sslang:scanner-test
stack test sslang:parser-test
````

Build and view code documentation with

````
stack haddock --open
````

Haddock documentation is placed in, e.g., /mnt/sedwards/group/projects/bdl/sslang/.stack-work/install/x86_64-linux-tinfo6/b3ccde3c8441eeca22b0a903ee50d8515e74811fa5bed474a049e55591dcb5b4/8.10.7/doc/all/index.html

Build the language reference manual (sslang-lrm.pdf) with

````
cd doc
make
````
