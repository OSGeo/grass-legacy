MODULE_TOPDIR = ..

SUBDIRS = timer mkftcap

include $(MODULE_TOPDIR)/include/Make/Dir.make

default: parsubdirs $(TOOLSDIR)/g.echo$(EXE)

clean: cleansubdirs

$(TOOLSDIR)/g.echo$(EXE): $(OBJDIR)/g.echo.o
	$(MAKE) $(TOOLSDIR)
	$(CC) $(LDFLAGS) $(OBJDIR)/g.echo.o -o $@

$(TOOLSDIR):
	$(MKDIR) $@
