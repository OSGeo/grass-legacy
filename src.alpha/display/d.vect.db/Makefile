rm=zorro
all:
	@echo type make something

desc.olga.nroff:
#	cat desc.olga | rsh ${rm} 'tbl|nroff -ms' | more
	cat desc.olga | tbl|nroff -ms | more
desc.olga.troff:
#	cat desc.olga | rsh ${rm} 'tbl|troff -ms -t' | lpr -t
	cat desc.olga | tbl|troff -ms -t | lpr -t

