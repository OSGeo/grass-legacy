
$(OBJARCH)/%.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@

$(OBJARCH)/%.o: %.cc
	$(CXX) $(CXXFLAGS) -c $< -o $@

makefile: Gmakefile
	${SRCDIR}/mk/genmake.sed < $< > $@

dir: force
	mkdir -p $(OBJARCH)
	$(MAKE) -f makefile

ctags:
	ctags *.[ch]
	sed 's/\?/\//g' tags > tags.tmp
	mv tags.tmp tags
tags: ctags ; #

force: ;
