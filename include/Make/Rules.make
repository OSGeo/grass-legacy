
# lexical analyzer and default options
LEXFLAGS= -d -i -s -t

# parser generator and default options
YACCFLAGS = -d -v -t

ifndef LOCAL_HEADERS
LOCAL_HEADERS = $(wildcard *.h)
endif

# default cc rules
$(OBJDIR)/%.o : %.c $(DEPENDENCIES) $(LOCAL_HEADERS) 
	@test -d $(OBJDIR) || mkdir $(OBJDIR)	
	$(CC) $(CFLAGS) $(EXTRA_CFLAGS) $(EXTRA_INC) $(INC) \
		-o $(OBJDIR)/$*.o -c $*.c

# default parser generation rules, include prefix for files/vars
%.yy.c: %.l
	$(LEX) -P$* $(LEXFLAGS) $*.l | \
	$(SED) -e 's/unistd.h/limits.h/g' \
	> $@

%.tab.h %.tab.c: %.y
	$(YACC) -b$* -p$* $(YACCFLAGS) $<


# default clean rules
clean:
	${SHELL} -c "find . -name 'OBJ*' -exec rm -rf {} \; 2>/dev/null ; true"
	rm -rf $(EXTRA_CLEAN_DIRS)
	rm -f $(EXTRA_CLEAN_FILES)

# html rules for cmd commands
htmlcmd:
	GRASS_FAKE_START=1 GISBASE=$(GISBASE) $(ETC)/bin/cmd/$(PGM) --html-description | grep -v '</body>' > $(PGM).html ; true
	@test ! -f description.html || ( cat description.html >> $(PGM).html )
	echo "<HR>" >> $(PGM).html
	echo "<P><a href=index.html>Help Index</a>" >> $(PGM).html
	echo "</body></html>" >> $(PGM).html
	mkdir -p $(GISBASE)/docs/html
	mv $(PGM).html $(GISBASE)/docs/html

# html rules for scripts
htmlscript:
	GRASS_FAKE_START=1 GISBASE=$(GISBASE) $(GISBASE)/scripts/$(PGM) --html-description | grep -v '</body>' > $(PGM).html ; true
	@test ! -f description.html || ( cat description.html >> $(PGM).html )
	echo "<HR>" >> $(PGM).html
	echo "<P><a href=index.html>Help Index</a>" >> $(PGM).html
	echo "</body></html>" >> $(PGM).html
	mkdir -p $(GISBASE)/docs/html
	mv $(PGM).html $(GISBASE)/docs/html

# html rules for inter commands
# note that fakestart doesn't work here
htmlinter:
	@test ! -f description.html || ( cat description.html >> $(PGM).html )
	echo "<HR>" >> $(PGM).html
	echo "<P><a href=index.html>Help Index</a>" >> $(PGM).html
	echo "</body></html>" >> $(PGM).html
	mkdir -p $(GISBASE)/docs/html
	mv $(PGM).html $(GISBASE)/docs/html
