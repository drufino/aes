CC=clang
CXX=clang
CFLAGS=-O3 -Wall -mssse3 -mno-sse4 -Iinclude/
CXXFLAGS=-O3 -Wall -mssse3 -mno-sse4 -Iinclude/

all: run_tests tables/aes_tables.native tables/aes_test.native 

tarball:
	(cd ../; tar --exclude aes.sublime-project --exclude tests/aes_test --exclude tests/sbox_test --exclude aes.sublime-workspace --exclude tests/KAT_AES.zip --exclude _build --exclude tests/KAT_AES --exclude tests/_build --exclude tables/_build --exclude docs --exclude .svn --exclude .DS_Store -zcvf aes_algebra-`date +%Y%m%d`.tgz aes_algebra)

run_tests: _build/libtinfoil.a
	(cd tests; $(MAKE))

tables/aes_test.native: tables/aes_test.ml
	(cd tables; $(MAKE) aes_test.native)

tables/aes_tables.native: tables/aes_tables.ml tables/aes.ml
	(cd tables; $(MAKE) aes_tables.native)

_build/%.o: %.cpp
	mkdir -p _build
	$(CXX) $(CXXFLAGS) -c $< -o $@ 

_build/%.o: src/%.c
	mkdir -p _build
	$(CC) $(CFLAGS) -c $< -o $@ 

_build/libtinfoil.a: _build/aes.o _build/aes_simple.o _build/aes_ssse3.o _build/sse_helpers.o _build/cpuid.o
	ar -cr $@ $^

clean:
	(cd tables; $(MAKE) clean);
	(cd tests; $(MAKE) clean);
	rm -rf _build

# vim: set noexpandtab
