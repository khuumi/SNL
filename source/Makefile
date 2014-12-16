ast_test: touch objects java compiler
	ocamlc -o snlc ast.cmo parser.cmo scanner.cmo str.cma codegen.cmo analyzer.cmo snlc.cmo

touch:
	touch codegen.ml

compiler: snlc.ml
	ocamlc -c snlc.ml

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
	rm -f parser.mli scanner.ml parser.ml parser.output *.cmo *.cmi snlc *~
