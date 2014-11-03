ast_test: objects ast
	ocamlc -o snl_ast ast.cmo parser.cmo scanner.cmo snl_ast.cmo

ast: snl_ast.ml
	ocamlc -c snl_ast.ml

objects: parser scanner
	ocamlc -c ast.ml parser.mli scanner.ml parser.ml

parser: parser.mly
	ocamlyacc -v parser.mly

scanner: scanner.mll
	ocamllex scanner.mll

.PHONY: clean
clean:
	rm -f parser.mli scanner.ml parser.ml parser.output *.cmo *.cmi snl_ast
