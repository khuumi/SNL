ast_test: objects java ast
	ocamlc -o snl_ast ast.cmo parser.cmo scanner.cmo codegen.cmo snl_ast.cmo

ast: snl_ast.ml
	ocamlc -c snl_ast.ml

java: codegen.ml
	ocamlc -c codegen.ml

objects: parser scanner
	ocamlc -c ast.ml parser.mli scanner.ml parser.ml

parser: parser.mly
	ocamlyacc -v parser.mly

scanner: scanner.mll
	ocamllex scanner.mll

.PHONY: test
test: ast_test
	./run_tests.py

.PHONY: clean
clean:
	rm -f parser.mli scanner.ml parser.ml parser.output *.cmo *.cmi snl_ast *~
