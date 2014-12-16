compiler: snlc.ml objects
	ocamlc -c snlc.ml
	ocamlc -o snlc ast.cmo parser.cmo scanner.cmo str.cma codegen.cmo analyzer.cmo snlc.cmo

objects: scanner parser generator
	ocamlc -c ast.ml sast.ml parser.mli scanner.ml parser.ml analyzer.ml codegen.ml

generator: analyzer.ml codegen.ml

parser: parser.mly
	ocamlyacc -v parser.mly

scanner: scanner.mll
	ocamllex scanner.mll

.PHONY: test
test: compiler
	./run_tests.py

.PHONY: clean
clean:
	rm -f parser.mli scanner.ml parser.ml parser.output *.cmo *.cmi snlc *~
