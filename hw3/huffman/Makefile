# PS3 makefile
#
# targets are:
#
# all -- rebuild the project (default)
# clean -- remove all objects and executables

# uncomment for MacOS X
#  export SHELL = /bin/bash
#  export PATH = /usr/bin:/bin:/usr/local/bin

.PHONY: all
all: huffman.exe

.PHONY: clean
clean:
	for Y in exe cmo cmi; do \
	  rm -f *.$$Y; \
	done

huffman.exe: util.ml huffman.ml
	ocamlc -o huffman.exe -g str.cma util.ml huffman.ml
