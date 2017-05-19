
LEX=ocamllex
YACC=ocamlyacc
YACC_V=-v

TARGETS=scanner.ml

.PHONY: default clean
default: $(TARGETS)

scanner.ml: scanner.mll
	$(LEX) scanner.mll

parser.ml parser.mli: parser.mly ast.mli
	$(YACC) parser.mly
parser.output: parser.mly ast.mli
	$(YACC) $(YACC_V) parser.mly

clean:
	rm -rf *.cmx *.cmi *.cmo *.o $(TARGETS)
