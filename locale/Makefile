# written by Markus Neteler
# tcl stuff by Cedric Shock
# wxpython by Martin Landa

MODULE_TOPDIR = ..
include $(MODULE_TOPDIR)/include/Make/Module.make

default:
	@if [ "$(HAVE_NLS)" != "" ] ; then \
		echo "Creating translations (= 'make mo')" >&2 ; \
		$(MAKE) mo ; \
	else \
		echo "NLS disabled." ; \
	fi

all:
	@echo 'Usage:'
	@echo '   make pot        create grass.pot (containing original messages)'
	@echo '   make update-po  merge new messages into existing *.po files'
	@echo '   make mo         create the *.mo files'
	@echo '   make verify     verify the *.po files'

# Directory for installing tcl .msg files:
MSG_DIR = $(ARCH_DISTDIR)/etc/msgs
NVIZ_DIR = $(ARCH_DISTDIR)/etc/nviz2.2/msgs
MO_DIR = $(ARCH_DISTDIR)/locale
PO_DIR = po

#distinguish between library messages and modules:
LIBDOMAIN = grasslibs
MODDOMAIN = grassmods
TCLDOMAIN = grasstcl
NVIZDOMAIN = grassnviz
WXPYDOMAIN = grasswxpy
DOMAINS = $(LIBDOMAIN) $(MODDOMAIN) $(TCLDOMAIN) $(WXPYDOMAIN)

LIB_POTFILES = find ../lib -name '*.c' | xargs grep -l "_(\""
MOD_POTFILES = find ../ -name '*.c' | grep -v '../lib' | xargs grep -l "_(\""
TCL_POTFILES = find ../ -name '*.tcl' ! -path '../dist*' ! -path '../visualization/nviz/*' | xargs grep -l "\\[_ \|\\[G_msg"
NVIZTCL_POTFILES = find ../visualization/nviz/scripts \( ! -regex '.*/\..*' \) | xargs grep -l "\\[_ \|\\[G_msg"
WXPY_POTFILES = find ../gui/wxpython -name '*.py' | xargs grep -l "_(\""

#The xgettext utility is used to automate the creation of
#portable message files (.po)
pot:
	if [ ! -f ../gui/wxpython/menustrings.py ] ; then \
		echo "Build GRASS before running 'make pot'" >&2 ; \
		exit 1 ; \
	fi
	@echo "Generating $(LIBDOMAIN)..."
	xgettext -k_ -o ./templates/$(LIBDOMAIN).pot `$(LIB_POTFILES)`
	@echo "Generating $(MODDOMAIN)..."
	xgettext -k_ -o ./templates/$(MODDOMAIN).pot `$(MOD_POTFILES)`
	@echo "Generating $(TCLDOMAIN)..."
	xgettext -k_ --keyword=G_msg -o ./templates/$(TCLDOMAIN).pot `$(TCL_POTFILES)`
	@echo "Generating $(NVIZDOMAIN)..."
	xgettext -k_ --keyword=G_msg -L Tcl -o ./templates/$(NVIZDOMAIN).pot `$(NVIZTCL_POTFILES)`
	@echo "Generating $(WXPYDOMAIN)..."
	xgettext -k_ -o ./templates/$(WXPYDOMAIN).pot `$(WXPY_POTFILES)` 

#merge already existing translations with new messages in POT template file and create new po files:
update-po:
	@for i in $(DOMAINS) ; do \
		if [ "`ls ./po/$$i\_*.po 2>/dev/null`" = "" ] ; then \
		 echo "No $$i.po file found in ./po/ directory. Will create new po files from template." ; \
		 cp ./templates/$$i.pot ./po/$$i.po ; \
		 echo "Created ./po/$$i.po - Rename to ./po/$$i\_LANG.po (with LANG: de, ru, ...)" ; \
		 echo "Then you can translate the messages in this file (e.g with kbabel)" ; \
		fi ;\
	done
	@cd ./po/ ; for po in `ls *_*.po 2>/dev/null` ; do \
		suffix=`echo $$po | cut -d'_' -f2-`; \
		lingua=`basename $$suffix .po`; \
		prefix=`echo $$po | cut -d'_' -f1`; \
		if msgmerge -o $$prefix\_$$suffix.new $$prefix\_$$suffix ../templates/$$prefix.pot; then\
		  mv $$prefix\_$$suffix.new $$prefix\_$$suffix; \
		  echo "Merged new messages into $$prefix\_$$suffix" ; \
		else \
		    echo "Merging failed"; \
		fi \
		done
	@echo "Be careful with SVN commits as .po file updates must be syncronized with the individual translators."

verify:
	@cd ./po/ ; for po in `ls *_*.po 2>/dev/null` ; do \
		echo "----- $$po:" ; \
		msgfmt -c $$po; \
	done

libs_FILES := $(patsubst po/grasslibs_%.po,$(MO_DIR)/%/LC_MESSAGES/grasslibs.mo,$(wildcard po/grasslibs_*.po)) 
$(MO_DIR)/%/LC_MESSAGES/grasslibs.mo: po/grasslibs_%.po 
	@ [ -d $(MO_DIR)/$*/LC_MESSAGES ] || $(MKDIR) $(MO_DIR)/$*/LC_MESSAGES 
	msgfmt --statistics -o $@ $< 

mods_FILES := $(patsubst po/grassmods_%.po,$(MO_DIR)/%/LC_MESSAGES/grassmods.mo,$(wildcard po/grassmods_*.po)) 
$(MO_DIR)/%/LC_MESSAGES/grassmods.mo: po/grassmods_%.po 
	@ [ -d $(MO_DIR)/$*/LC_MESSAGES ] || $(MKDIR) $(MO_DIR)/$*/LC_MESSAGES 
	msgfmt --statistics -o $@ $< 

wxpy_FILES := $(patsubst po/grasswxpy_%.po,$(MO_DIR)/%/LC_MESSAGES/grasswxpy.mo,$(wildcard po/grasswxpy_*.po)) 
$(MO_DIR)/%/LC_MESSAGES/grasswxpy.mo: po/grasswxpy_%.po 
	@ [ -d $(MO_DIR)/$*/LC_MESSAGES ] || $(MKDIR) $(MO_DIR)/$*/LC_MESSAGES 
	msgfmt --statistics -o $@ $< 

$(MSG_DIR)/%.msg: po/grasstcl_%.po
	@ [ -d $(MSG_DIR) ] || $(MKDIR) $(MSG_DIR)
	msgfmt --statistics --tcl -l $* -d $(MSG_DIR)/ $<

MSGFILES := $(patsubst po/grasstcl_%.po,$(MSG_DIR)/%.msg,$(wildcard po/grasstcl_*.po))

$(NVIZ_DIR)/%.msg: po/grassnviz_%.po
	@ [ -d $(NVIZ_DIR) ] || $(MKDIR) $(NVIZ_DIR)
	msgfmt --statistics --tcl -l $* -d $(NVIZ_DIR)/ $<

NVIZFILES := $(patsubst po/grassnviz_%.po,$(NVIZ_DIR)/%.msg,$(wildcard po/grassnviz_*.po))

#create binary messages files
mo: $(libs_FILES) $(mods_FILES) $(wxpy_FILES) $(MSGFILES) $(NVIZFILES)

.PHONY: mo
