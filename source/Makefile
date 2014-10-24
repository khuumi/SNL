objects: parser scanner
	ocamlc -c ast.ml parser.mli scanner.ml parser.ml

parser: parser.mly
	ocamlyacc -v parser.mly

scanner: scanner.mll
	ocamllex scanner.mll

.PHONY: clean
clean:
	rm -f parser.mli scanner.ml parser.ml parser.output *.cmo *.cmi
