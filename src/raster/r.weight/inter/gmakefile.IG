#
#
#  this is version of Gmakefile that should be used on Intergraph machines.
#  It removes the reference to gis_lxcl.c in the clean routine, and
#  the entire lex routine
LIBES = $(D_LIB) $(DISPLAYLIB) $(RASTERLIB) $(GISLIB)
OFILES  =\
	add_mult.o\
	ask_weight.o\
	assign.o\
	colors.o\
	choose_map.o\
	execute.o\
	dots.o\
	falloc.o\
	gets.o\
	histo.o\
	list_anal.o\
	list_cats.o\
	list_maps.o\
	sav_rec.o\
	weighted.o\
	gis_pars.o\
	gis_lxcl.o\
	set_sigs.o\
	write_supp.o


all: clean pgm shell help
shell: $(BIN_MAIN_INTER)/r.weight
pgm: $(ETC)/r.weight

$(ETC)/r.weight: $(OFILES) $(LIBES) $(VASKLIB)
	rm -f a.out
	$(CC) $(LDFLAGS) $(OFILES) $(LIBES) $(VASK) $(MATHLIB)
	mv a.out $@

$(BIN_MAIN_INTER)/r.weight: weight.sh
	rm -f $@ ; cp $? $@ ; chmod 0755 $@

help:
	rm -rf $(TXT)/WEIGHT
	mkdir $(TXT)/WEIGHT
	cp HELP_TEXT/* $(TXT)/WEIGHT

clean:
	-rm -f gis_pars.c gis_pars.h gis_pars.o gis_lxcl.o

#
#
# PARSER
gis_pars.c:     yes_no.h gis_pars.y
	yacc -v -d gis_pars.y
	mv y.tab.c gis_pars.c
	mv y.tab.h gis_pars.h
	mv y.output gis_pars.out
	@echo "See gis_pars.out for review of this parser." 

#produces bad .c file on Intergraph
#
#gis_lxcl.c:     gis_lxcl.l gis_pars.h
#	lex -t gis_lxcl.l > gis_lxcl.c

gis_pars.h: gis_pars.c

add_mult.o: include.h
assign.o: include.h
choose_map.o: include.h
choose_map.o: yes_no.h
execute.o: include.h
gis_pars.o: include.h
list_anal.o: include.h
list_cats.o: include.h
sav_rec.o: include.h
weighted.o: include.h
weighted.o:    yes_no.h 

$(LIBES): #
$(VASKLIB): #

print:
	pr *.h *.l *.y *.c | lpr
