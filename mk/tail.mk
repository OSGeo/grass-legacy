
EXTRA_CFLAGS += -I$(OBJARCH)

$(OBJARCH)/%.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@

dir: force
	mkdir -p $(OBJARCH)
	$(MAKE) -f makefile

ctags:
	ctags *.[ch]
	sed 's/\?/\//g' tags > tags.tmp
	mv tags.tmp tags
tags: ctags ; #

force: ;
