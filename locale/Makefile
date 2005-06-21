# written by MN

MODULE_TOPDIR = ..
include $(MODULE_TOPDIR)/include/Make/Module.make

default:
	@if [ "$(HAVE_NLS)" != "" ] ; then echo "Creating translations (= 'make mo')" ; make mo ; else echo "NLS disabled, cannot translate (re-configure GRASS first)." ; fi

all:
	@echo 'Usage:'
	@echo '   make pot        create grass.pot (containing original messages)'
	@echo '   make update-po  merge new messages into existing *.po files'
	@echo '   make mo         create the *.mo files'

MO_DIR = $(GISBASE)/locale
PO_DIR = po

#distinguish between library messages and modules:
LIBDOMAIN = grasslibs
MODDOMAIN = grassmods
DOMAINS = $(LIBDOMAIN) $(MODDOMAIN)

LIB_POTFILES = find ../lib -name '*.c' | xargs grep -l "_(\""
MOD_POTFILES = find ../ -name '*.c' | grep -v '../lib' | xargs grep -l "_(\""

#The xgettext utility is used to automate the creation of
#portable message files (.po)
pot:
	xgettext -k_ -o ./templates/$(LIBDOMAIN).pot `$(LIB_POTFILES)`
	xgettext -k_ -o ./templates/$(MODDOMAIN).pot `$(MOD_POTFILES)`

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

#create binary messages files
mo:
	@(cd ./po/ ; for po in `ls *_*.po 2>/dev/null` ; do\
		suffix=`echo $$po | cut -d'_' -f2-`; \
		lingua=`basename $$suffix .po`; \
		prefix=`echo $$po | cut -d'_' -f1`; \
		install -d $(MO_DIR)/$$lingua/LC_MESSAGES/ ; \
		echo -n $$po": "; \
		msgfmt --statistics \
			-o $(MO_DIR)/$$lingua/LC_MESSAGES/$$prefix.mo $$po ;\
	done \
	)
