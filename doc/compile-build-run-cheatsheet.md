# SSLANG Dev Workflow Notes

Navigate to your cloned sslang directory.

## Basic
```
stack build # builds the repo
stack test  # runs all tests
```
## Regression Tests

Navigate to the regression-tests directory

**To run only the regression tests:** 
```
./runtests.sh
```
```
# the output of passing test cases will be deleted
# the output of failing test cases will remain in the out directory
```

If a test case fails, you can look at runtests.log for more info, as well as investigate contents of the out directory.

**To run only the regression tests and keep all test case output:** 
```
./runtests.sh -k
```
```
# the -k option stands for "keep", because output from all test cases will be kept
# check the out directory for the output of both failing and passing test cases
```
**To run a single regression test:** 
```
./runtests.sh tests/<testFileName.ssl>
```
**To run a single regression test and keep its output, even if passing:**
```
 ./runtests.sh -k tests/<testFileName.ssl>
 ```
**To run a sslang executable directly:** 
First compile using the runtests.sh script using -k option.
```
out/<testFileName>
```
## Directly Invoking the Compiler

**Compiling a .ssl source file into c code:**

`stack exec sslc -- <optional-arguments>  <sourceFileName.ssl>`

**Running the Compiler on .ssl source file and dumping IR instead of output C code:**

`stack exec sslc -- --dump-ir <sourceFileName.ssl>`

Note: There are a bunch more options for dumping IR at different stages in the compiler.
```
--dump-ir Print the IR immediately after lowering

--dump-ir-annotated Print the fully-typed IR just before type inference

--dump-ir-typed Print the fully-typed IR after type inference

--dump-ir-typed-ugly  Ugly-Print the fully-typed IR after type inference

--dump-ir-lifted Print the IR after lambda lifting

--dump-ir-final Print the last IR representation before code generation
```
You can check them all by looking at the ssl usage with `stack exec sslc -- --help`