CXXFLAGS=-O3 -Wall -I../include/
CXX=clang
CC=clang
CFLAGS=-O3 -Wall -I../include/

all: aes_test speed_test run_test

_build/%.o: %.cpp
	mkdir -p _build
	$(CXX) ${CXXFLAGS} -c `pwd`/$< -o $@ 

_build/%.o: %.c
	mkdir -p _build
	$(CC) ${CFLAGS} -c `pwd`/$< -o $@ 

aes_test: _build/aes_test.o ../_build/libtinfoil.a
	$(CXX) `pwd`/$< -o $@ -lstdc++ -ltinfoil -L../_build

KAT_AES.zip:
	wget -nd -c http://csrc.nist.gov/groups/STM/cavp/documents/aes/KAT_AES.zip

KAT_AES: KAT_AES.zip
	mkdir -p KAT_AES
	unzip -qq KAT_AES.zip -d KAT_AES

speed_test:
	@./aes_test aesspeedtest

run_test: KAT_AES speed_test
	@/usr/bin/env echo -n "[*] Testing AES-256 in ECB mode...."
	@./aes_test aesecbquiet KAT_AES/ECBVarKey256.rsp
	@./aes_test aesecbquiet KAT_AES/ECBVarTxt256.rsp
	@./aes_test aesecbquiet KAT_AES/ECBKeySbox256.rsp
	@./aes_test aesecbquiet KAT_AES/ECBGFSbox256.rsp
	@/usr/bin/env echo " passed."
	@/usr/bin/env echo -n "[*] Testing AES-192 in ECB mode...."
	@./aes_test aesecbquiet KAT_AES/ECBVarKey192.rsp
	@./aes_test aesecbquiet KAT_AES/ECBVarTxt192.rsp
	@./aes_test aesecbquiet KAT_AES/ECBKeySbox192.rsp
	@./aes_test aesecbquiet KAT_AES/ECBGFSbox192.rsp
	@/usr/bin/env echo " passed."
	@/usr/bin/env echo  -n "[*] Testing AES-128 in ECB mode...."
	@./aes_test aesecbquiet KAT_AES/ECBVarKey128.rsp
	@./aes_test aesecbquiet KAT_AES/ECBVarTxt128.rsp
	@./aes_test aesecbquiet KAT_AES/ECBKeySbox128.rsp
	@./aes_test aesecbquiet KAT_AES/ECBGFSbox128.rsp
	@/usr/bin/env echo " passed."

clean:
	rm -rf KAT_AES _build aes_test

# vim: set noexpandtab
