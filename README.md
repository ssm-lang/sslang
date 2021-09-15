# sslang

A language built atop the Sparse Synchronous Model

This depends on the Haskell Stack and the SSM library

See <https://docs.haskellstack.org/en/stable/GUIDE/> for Haskell Stack
documentation

Install (but don't build) the ssm library in the top level directory with

git clone git@github.com:sedwards-lab/ssm.git


Build the compiler (sslc) with

````
stack build
````


Test with, e.g.,

````
cd regression-tests ; ./runtests.sh
stack test sslang:scanner-test
stack build sslang:parser-test
````

