# written by MN

MODULE_TOPDIR = ..
include $(MODULE_TOPDIR)/include/Make/Module.make

default:
	@echo 'Usage:'
	@echo '   make pot        create grass.pot (containing original messages)'
	@echo '   make update-po  merge new messages into existing *.po files'
	@echo '   make mo         create the *.mo files'

MO_DIR = $(GISBASE)/locale
PO_DIR = po

#distinguish between library messages and modules:
LIBDOMAIN = grasslibs
MODDOMAIN = grassmods

LIB_POTFILES = find ../lib -name '*.c' | xargs grep -l "_(\""
MOD_POTFILES = find ../ -name '*.c' | grep -v '../lib' | xargs grep -l "_(\""

#The xgettext utility is used to automate the creation of
#portable message files (.po)
pot:
	xgettext -k_ -o ./templates/$(LIBDOMAIN).pot `$(LIB_POTFILES)`
	xgettext -k_ -o ./templates/$(MODDOMAIN).pot `$(MOD_POTFILES)`

#merge already existing translations with new messages in POT template file and create new po files:
update-po:
	@if [ "`ls ./po/*.po 2> /dev/null`" = "" ] ; then \
		echo "No .po file(s) found in this directory. Will create new po files from templates." ; \
		cp ./templates/$(LIBDOMAIN).pot ./po/$(LIBDOMAIN).po ; \
		echo "Created ./po/$(LIBDOMAIN).po - Rename to ./po/$(LIBDOMAIN)_LANG.po (with LANG: de, ru, ...)" ; \
		cp ./templates/$(MODDOMAIN).pot ./po/$(MODDOMAIN).po ; \
		echo "Created ./po/$(MODDOMAIN).po - Rename to ./po/$(MODDOMAIN)_LANG.po (with LANG: de, ru, ...)" ; \
		echo "Then you can translate the messages in these files (e.g with kbabel)" ; \
		exit 0 ; \
	else \
	cd ./po/ ; for po in *.po; do \
		suffix=`echo $$po | cut -d'_' -f2`; \
		lingua=`basename $$suffix .po`; \
		prefix=`echo $$po | cut -d'_' -f1`; \
		if msgmerge -o $$prefix\_$$suffix.new $$prefix\_$$suffix ../templates/$$prefix.pot; then\
		  mv $$prefix\_$$suffix.new $$prefix\_$$suffix; \
		  echo "Merged new messages into $$prefix\_$$suffix" ; \
		else \
		    echo "Merging failed"; \
		fi \
		done \
	fi

#create binary messages files
mo:
	@(cd ./po/ ; for po in *.po; do\
		suffix=`echo $$po | cut -d'_' -f2`; \
		lingua=`basename $$suffix .po`; \
		prefix=`echo $$po | cut -d'_' -f1`; \
		install -d $(MO_DIR)/$$lingua/LC_MESSAGES/ ; \
		echo -n $$po": "; \
		msgfmt --statistics \
			-o $(MO_DIR)/$$lingua/LC_MESSAGES/$$prefix.mo $$po ;\
	done \
	)
