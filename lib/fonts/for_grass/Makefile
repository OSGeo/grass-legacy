
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

default: $(OBJDIR)/font.bin $(FNT)

$(FNT): $(OBJDIR)/splitfont font.bin
	$(OBJDIR)/splitfont $(FONTDIR) < $(OBJDIR)/font.bin

$(OBJDIR)/font.bin: $(OBJDIR)/font_2_bin $(RAWFONT)
	cat $(RAWFONT) | $(OBJDIR)/font_2_bin > $@

$(OBJDIR)/font_2_bin: $(OBJDIR)/font_2_bin.o
	$(CC) $(LDFLAGS) $(OBJDIR)/font_2_bin.o -o $@

$(OBJDIR)/splitfont: $(OBJDIR)/splitfont.o $(OBJDIR)/fontmap.o
	$(CC) $(LDFLAGS) $(OBJDIR)/splitfont.o $(OBJDIR)/fontmap.o -o $@

try: $(OBJDIR)/try.o $(OBJDIR)/showchar.o $(OBJDIR)/font.o
	$(CC) $(LDFLAGS) $(OBJDIR)/try.o $(OBJDIR)/showchar.o $(OBJDIR)/font.o \
	      -o $(OBJDIR)/try $(CURSES)
