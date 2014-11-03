project: objects
	ocamlc -o snl ast.cmo parser.cmo scanner.cmo snl.cmo

objects: parser scanner snl
	ocamlc -c ast.ml parser.mli scanner.ml parser.ml snl.ml

snl: snl.ml

parser: parser.mly
	ocamlyacc -v parser.mly

scanner: scanner.mll
	ocamllex scanner.mll

.PHONY: clean
clean:
	rm -f parser.mli scanner.ml parser.ml parser.output *.cmo *.cmi snl
