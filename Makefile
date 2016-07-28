#Makefile by Michalis Pitidis

.PHONY: clean distclean pack count

# OS type: Linux/Win DJGPP
ifdef OS
   EXE=.exe
else
   EXE=
endif

EXEFILE = alanc$(EXE)

MLFILES = Hashcons.ml Identifier.ml Error.ml Types.ml Symbol.ml \
  Quad.ml Subroutine.ml Finalcode.ml Lexer.ml Parser.ml main.ml

MLIFILES = Hashcons.mli Identifier.mli Error.mli Types.mli Symbol.mli \
  Quad.mli Finalcode.mli Parser.mli

CMOFILES = $(patsubst %.ml,%.cmo, $(MLFILES))
CMIFILES = $(patsubst %.ml,%.cmi, $(MLFILES))
CMXFILES = $(patsubst %.ml,%.cmx, $(MLFILES))
OBJFILES = $(patsubst %.ml,%.o, $(MLFILES))

PARSERFILES = Parser.ml Parser.mli Parser.output Lexer.ml

SRCFILES = Makefile extend.ml Lexer.mll Parser.mly \
  $(filter-out Parser.% Lexer.%, $(MLFILES)) \
  $(filter-out Parser.%, $(MLIFILES))

CAMLP4_FLAGS = -pp "camlp4o ./extend.cmo"
OCAMLC_FLAGS = -g
OCAMLOPT_FLAGS =

OCAMLC = ocamlc $(OCAMLC_FLAGS)
OCAMLOPT = ocamlopt $(OCAMLOPT_FLAGS)
ifdef OS
	OCAMLDEP = ocamldep
else
	OCAMLDEP = ocamldep
endif


INCLUDES =
# ----Uncomment your preffered install path!----
#INSTALL_PATH=/home/michalis/NTUA/Lambda/Compilers/alanPrograms/
INSTALL_PATH=/home/michalis/NTUA/komp/compiler/alanPrograms/
#INSTALL_PATH=/home/kostas/Desktop/quads/
#INSTALL_PATH=/usr/bin
#INSTALL_PATH=/c/compiler/alanPrograms/


#default: symbtest$(EXE)
default: all

symbtest$(EXE): $(filter-out Lexer.cmo Parser.cmo,$(CMOFILES))
	$(OCAMLC) -o $@ $^

all: $(EXEFILE)

top: $(EXEFILE).top

extend.cmo: extend.ml
	$(OCAMLC) -pp "camlp4o pa_extend.cmo q_MLast.cmo" -I +camlp4 -c $<

%.cmo: %.ml %.mli extend.cmo
	$(OCAMLC) $(CAMLP4_FLAGS) -c $<

%.cmx: %.ml extend.cmo
	$(OCAMLOPT) $(CAMLP4_FLAGS) -c $<

%.cmi: %.mli extend.cmo
	$(OCAMLC) $(CAMLP4_FLAGS) -c $<

%.cmo %.cmi: %.ml extend.cmo
	$(OCAMLC) $(CAMLP4_FLAGS) -c $<

.PHONY: all clean count depend

$(EXEFILE): Parser.mli Lexer.ml $(CMOFILES)
	$(OCAMLC) -o $@ $(CMOFILES)

TOPFILES := $(filter-out main.cmo, $(CMOFILES))
$(EXEFILE).top: Parser.mli Lexer.ml $(TOPFILES)
	ocamlmktop -o $@ $(TOPFILES)

Parser.ml Parser.mli: Parser.mly
	ocamlyacc -v Parser.mly

Lexer.ml: Lexer.mll
	ocamllex Lexer.mll

-include .depend

depend: $(MLFILES) $(MLIFILES) extend.cmo
	$(OCAMLDEP) $(CAMLP4_FLAGS) $(INCLUDES) \
          $(filter-out extend.cmo,$^) > .depend

clean:
	$(RM) $(CMXFILES) $(CMOFILES) $(CMIFILES) $(OBJFILES) $(EXEFILES) \
           extend.cmi extend.cmo \
           $(patsubst %,%.cm?,$(EXEFILES)) $(PARSERFILES) pplib.cma *~

distclean: clean
	$(RM) $(EXEFILE) .depend
	$(RM) "$(INSTALL_PATH)$(EXEFILE)" 

pack: clean
	tar cvfz alanc.tar.gz $(SRCFILES)

count:	
	wc -l $(SRCFILES)

#The following recompiles alanc and copies the executable to the folder were alanc programs are kept
#To determine the folder were alanc programs are kept tweak the INSTALL_PATH variable above
install: distclean default  
	cp $(EXEFILE) "$(INSTALL_PATH)$(EXEFILE)"

