all: aes_tables.native aes_test.native 

aes_test.native: aes_test.ml aes.ml GF2n.ml GF2n.mli
	/usr/local/bin/ocamlbuild aes_test.native aes_tables.native

aes_tables.native: aes.ml aes_tables.ml GF2n.ml GF2n.mli
	/usr/local/bin/ocamlbuild aes_tables.native aes_test.native

clean:
	ocamlbuild -clean
