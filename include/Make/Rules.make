
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
	$(CC) $(CFLAGS) $(EXTRA_CFLAGS) $(GDALCFLAGS) $(ALLINC) $(EXTRA_INCL) $(INC) \
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

