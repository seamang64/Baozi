# src/Makefile

all: ppc

PPC = mach.cmo keiko.cmo tree.cmo \
	lexer.cmo parser.cmo analyse.cmo main.cmo

ppc: $(PPC)
	ocamlc -g $^ -o $@ 

parser.ml parser.mli: parser.mly
	ocamlyacc -v parser.mly

lexer.ml: lexer.mll
	ocamllex lexer.mll

%.cmi: %.mli
	ocamlc -c -g -I ../lib $<

%.cmo: %.ml
	ocamlc -c -g -I ../lib $<

KEIKO = ../keiko
TESTSRC := $(wildcard test/*.p)

ppp : $(KEIKO)/lib.k ppp.k
pprolog : $(KEIKO)/lib.k pprolog.k

pprolog ppp :
	$(KEIKO)/pplink -i `cd ..; pwd`/keiko/ppx -nostdlib $^ -o $@
	chmod +x $@

%.k : %.p
	./ppc -O $< >$@

pprolog.p : pprolog.x ppp
	./ppp <$< | sed -e 's/{/(*/g' -e 's/}/*)/g' >$@

test : $(TESTSRC:test/%.p=test-%)

test-%: force
	@echo "*** Test $*.p"
	./ppc -O test/$*.p >a.k
	-sed -n -e '1,/^(\*\[\[/d' -e '/^]]\*)/q' -e p test/$*.p | diff -b - a.k
	$(KEIKO)/pplink -nostdlib $(KEIKO)/lib.k a.k -o a.x >/dev/null
	-$(KEIKO)/ppx ./a.x >a.test 2>&1
	sed -n -e '1,/^(\*<</d' -e '/^>>\*)/q' -e p test/$*.p | diff - a.test
	@echo "*** Passed"; echo

promote: $(TESTSRC:test/%.p=promote-%)

promote-%: force
	./ppc -O test/$*.p >a.k
	$(KEIKO)/pplink -nostdlib $(KEIKO)/lib.k a.k -o a.x >/dev/null
	-$(KEIKO)/ppx ./a.x >a.test 2>&1
	sed -f promote.sed test/$*.p >test/$*.new
	mv test/$*.new test/$*.p

force:

MLGEN = parser.mli parser.ml lexer.ml 

ML = $(MLGEN) analyse.ml mach.ml mach.mli main.ml keiko.ml tree.ml

realclean: clean

clean: force
	rm -f *.cmi *.cmo *.output $(MLGEN) \
		ppc ppx a.out a.k a.x a.test \
		pprolog.k pprolog.p pprolog ppp.k ppp

depend: $(ML) force
	(sed '/^###/q' Makefile; echo; ocamldep $(ML)) >new
	mv new Makefile

###

parser.cmi : tree.cmi keiko.cmi
parser.cmo : tree.cmi lexer.cmi keiko.cmi parser.cmi
parser.cmx : tree.cmx lexer.cmx keiko.cmx parser.cmi
analyse.cmo : tree.cmi mach.cmi lexer.cmi keiko.cmi analyse.cmi
analyse.cmx : tree.cmx mach.cmx lexer.cmx keiko.cmx analyse.cmi
analyse.cmi : tree.cmi
lexer.cmo : parser.cmi keiko.cmi lexer.cmi
lexer.cmx : xparser.cmx keiko.cmx lexer.cmi
lexer.cmi : parser.cmi keiko.cmi
mach.cmo : mach.cmi
mach.cmx : mach.cmi
mach.cmi :
main.cmo : tree.cmi parser.cmi mach.cmi lexer.cmi analyse.cmi
main.cmx : tree.cmx parser.cmx mach.cmx lexer.cmx analyse.cmx
keiko.cmi :
keiko.cmo : keiko.cmi
keiko.cmx : keiko.cmi
tree.cmo : keiko.cmi tree.cmi
tree.cmx : keiko.cmx tree.cmi
tree.cmi : keiko.cmi