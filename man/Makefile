MODULE_TOPDIR = ..

include $(MODULE_TOPDIR)/include/Make/Dir.make

# some definitions
SECT = 1
GRASS_MANDIR  = $(GISBASE)/man/man$(SECT)
GRASS_HTMLDIR = $(GISBASE)/docs/html
HTML2MAN = $(GRASS_HOME)/scripts/g.html2man/g.html2man


default: getModTime manpages

getModTime:
	(cd ../tools ; $(MAKE) -C getModTime )

# generate the man pages
manpages:
	@test -d $(GRASS_MANDIR) || mkdir -p $(GRASS_MANDIR)
	@echo Creating man pages
	@(cd $(GRASS_HTMLDIR); \
	  for h in *.html; \
	  do \
		f=`echo $$h | sed 's@^@$(GRASS_MANDIR)/@; s@\.html@\.$(SECT)@'`; \
	    	F_MOD_TIME=`${GISBASE}/etc/getModTime $$f`; \
	    	H_MOD_TIME=`${GISBASE}/etc/getModTime $$h`; \
		if [ ! -f $$f -o $$H_MOD_TIME -gt $$F_MOD_TIME ]; then $(HTML2MAN) $$h $$f $(SECT); fi; \
	  done)
	@(for h in *.html; \
	  do \
		f=`echo $$h | sed 's@^@$(GRASS_MANDIR)/@; s@\.html@\.$(SECT)@'`; \
	    	F_MOD_TIME=`${GISBASE}/etc/getModTime $$f`; \
	    	H_MOD_TIME=`${GISBASE}/etc/getModTime $$h`; \
		if [ ! -f $$f -o $$H_MOD_TIME -gt $$F_MOD_TIME ]; then $(HTML2MAN) $$h $$f $(SECT); fi; \
	  done)

#... why twice this loop?? Taken from 5.3-CVS.
