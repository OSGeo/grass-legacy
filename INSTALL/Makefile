DOC=grass
all:
	@echo type make troff, or make nroff
# the current date and time are encoded into the CF string
# so current date/time will appear at bottom of page if
# CF string not explicitly set in the document.
troff:
	date="`date|awk '{print $$2 \" \" $$3 \" \" $$6}'`" ; \
	hour=`uptime|awk '{print $$1}'` ; \
	(echo .ds CF \\s-2$$date $$hour\\s0; tbl $(DOC) ) | \
	troff -ms -t $(opt) | lpr -Poscar -t

nroff:
	tbl -TX $(DOC) | nroff -ms -Tlp | col -b | expand | more

tsee:
	tbl $(DOC) | trofftool -ms
