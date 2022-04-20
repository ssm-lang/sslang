###### Testing blink2
stack exec sslc -- tests/emily.ssl > emily.c
[]cc -O -g -Wall -Wno-unused-label -pedantic -std=c99 -I ../lib/ssm/include -I out -c -o emily.o emily.c
cc -O -g -Wall -Wno-unused-label -pedantic -std=c99 -I ../lib/ssm/include -I out -c -o out/emily-main.o tests/emily-main.c
cc -g -L ../lib/ssm/build -o emily emily.o emily-main.o -lssm
emily