ocamlc -I bmp bmp.cma graphics.cma disjointSet.mli disjointSet.ml image.mli image.ml part1.ml segmentation.mli segmentation.ml trie.ml
ocamlmktop -I bmp bmp.cma graphics.cma disjointSet.cmo image.cmo part1.cmo segmentation.cmo trie.cmo -o toplevel.exe
