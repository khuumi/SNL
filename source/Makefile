ast_test: objects java compiler
	ocamlc -o snl ast.cmo parser.cmo scanner.cmo codegen.cmo analyzer.cmo snl.cmo

compiler: snl.ml
	ocamlc -c snl.ml

java: codegen.ml
	ocamlc -c codegen.ml

objects: parser scanner
	ocamlc -c ast.ml sast.ml parser.mli scanner.ml parser.ml analyzer.ml

parser: parser.mly
	ocamlyacc -v parser.mly

scanner: scanner.mll
	ocamllex scanner.mll

.PHONY: test
test: ast_test
	./run_tests.py

.PHONY: clean
clean:
	rm -f parser.mli scanner.ml parser.ml parser.output *.cmo *.cmi snl *~
