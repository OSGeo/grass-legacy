
MODULE_TOPDIR = ../../..

include $(MODULE_TOPDIR)/include/Make/Platform.make
include $(MODULE_TOPDIR)/include/Make/Grass.make
include $(MODULE_TOPDIR)/include/Make/Rules.make

RAWFONT = \
	../fonts/hersh.oc1 \
	../fonts/hersh.oc2 \
	../fonts/hersh.oc3 \
	../fonts/hersh.oc4

EXTRA_CLEAN_FILES = font.bin 

FNT = $(FONTDIR)/cyrilc 

FONTMAPS := $(shell cd ../fonts && ls *.hmp | sed 's/.hmp//')

default: $(OBJDIR)/font.bin $(FNT)

$(FNT): $(OBJDIR)/splitfont $(OBJDIR)/font.bin
	$(OBJDIR)/splitfont $(OBJDIR)/font.bin $(FONTDIR) $(FONTMAPS)

$(OBJDIR)/font.bin: $(OBJDIR)/font_2_bin $(RAWFONT)
	cat $(RAWFONT) | $(OBJDIR)/font_2_bin $@

$(OBJDIR)/font_2_bin: $(OBJDIR)/font_2_bin.o
	$(CC) $(LDFLAGS) $(OBJDIR)/font_2_bin.o $(FMODE_OBJ) -o $@

$(OBJDIR)/splitfont: $(OBJDIR)/splitfont.o $(OBJDIR)/fontmap.o
	$(CC) $(LDFLAGS) $(OBJDIR)/splitfont.o $(OBJDIR)/fontmap.o $(FMODE_OBJ)\
		-o $@

try: $(OBJDIR)/try.o $(OBJDIR)/showchar.o $(OBJDIR)/font.o
	$(CC) $(LDFLAGS) $(OBJDIR)/try.o $(OBJDIR)/showchar.o $(OBJDIR)/font.o \
	      $(FMODE_OBJ) -o $(OBJDIR)/try $(CURSES)
