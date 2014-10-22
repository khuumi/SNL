objects: parser scanner
	ocamlc -c ast.ml parser.mli scanner.ml parser.ml

parser: parser.mly
	ocamlyacc parser.mly

scanner: scanner.mll
	ocamllex scanner.mll

.PHONY: clean
clean:
	rm -f parser.mli scanner.ml parser.ml *.cmo *.cmi
