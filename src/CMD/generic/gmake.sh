# $Id$

SHELL=/bin/sh
export SHELL
umask 002
##################################################################
# This file is sourced (with the . command) by the real gmake5
me=$0
if test "$SRC" = ""
then
    echo `basename $me` - SRC variable NULL or not set
    exit 1
fi
if test "$CMD" = ""
then
    echo `basename $me` - CMD variable NULL or not set
    exit 1
fi
if test "$HEADER" = ""
then
    echo `basename $me` - HEADER variable NULL or not set
    exit 1
fi

all=no
install=no
parseonly=no
while test $# != 0
do
    case "$1" in
	-sh) parseonly=sh;shift;;
	-csh) parseonly=csh;shift;;
	-hasx) 
            if test "$HASX" = "yes"
            then
		echo yes
                exit 0
            else
		echo no
                exit 1
            fi
            ;;
	-hasmotif) 
            if test "$HASMotif" = "yes"
            then
		echo yes
                exit 0
            else
		echo no
                exit 1
            fi
            ;;
	-v) GMAKE_VERBOSE=no;export GMAKE_VERBOSE;shift;;
	-all) all=yes;shift;;
	-makeparentdir)

	    dirname="`expr  \
	      ${2-.}'/' : '\(/\)[^/]*/$'  \
	      \| ${2-.}'/' : '\(.*[^/]\)//*[^/][^/]*//*$'  \
	      \| .`"
	    test -d $dirname || mkdir $dirname
	    exit $?
	    ;;
	-i) install=yes;shift;;

	 *) break
    esac
done

HEAD=$CMD/head/$HEADER
MID=$CMD/generic/make.mid
TAIL=$CMD/generic/make.tail

if test ! -d "$SRC"
then
    echo "SRC=$SRC - directory not found" >&2
    exit 1
fi
if test ! -r "$HEAD"
then
    echo "$HEAD - file not found (or not readable)" >&2
    exit 1
fi
if test ! -r "$MID"
then
    echo "$MID - file not found (or not readable)" >&2
    exit 1
fi

GISBASE="`sed 's/=/ /' $HEAD | awk '$1 ~ /^GISBASE$/ {if(NF>1)print $2}'`"
VERSION_NUMBER=
VERSION_DATE=
VERSION_UPDATE_PKG=
VERSION_FILE=.
if test -r $CMD/VERSION
then
    VERSION_FILE=$CMD/VERSION
    VERSION_NUMBER="`awk '{print;exit}' $VERSION_FILE`"
    VERSION_DATE="`awk '{if(hit){print;exit}{hit=1}}' $VERSION_FILE`"
    VERSION_UPDATE_PKG="`awk '{if(hit==2){print;exit}{hit++}}' $VERSION_FILE`"
fi

if test "$GISBASE" = ""
then
    echo "GISBASE not set in header file $HEAD" >&2
    exit 1
fi

ARCH="`sed 's/=/ /' $HEAD | awk '$1 ~ /^ARCH$/ {if(NF>1)print $2}'`"


if test $parseonly = sh
then
    echo GISBASE=$GISBASE
    echo SRC=$SRC
    echo CMD=$CMD
    echo HEADER=$HEADER
    echo ARCH=$ARCH
    exit 0
fi

if test $parseonly = csh
then
    echo set GISBASE=$GISBASE
    echo set SRC=$SRC
    echo set CMD=$CMD
    echo set HEADER=$HEADER
    echo set ARCH=$ARCH
    exit 0
fi

if test "$ARCH" = ""
then
    OBJARCH=OBJ  # force an object directory
    LIBARCH=LIB
else
    OBJARCH=OBJ.$ARCH
    LIBARCH=LIB.$ARCH
fi

if test $# -gt 0
then
    if test ! -d $1
    then
	echo "$1 - directory not found"
	exit 1
    fi
    cd $1
    shift
fi

if test ! "$GMAKE_VERBOSE" = no
then
    echo "  SRC     = $SRC"
    echo "  CMD     = $CMD"
    echo "  HEADER  = $HEADER"
    echo "  ARCH    = $ARCH"
    echo "  GISBASE = $GISBASE"
    echo "  VERSION = $VERSION_NUMBER $VERSION_DATE $VERSION_UPDATE_PKG"
fi

if test $all = yes
then
    for dir in *
    do
	if test -r $dir/Gmakefile
	then
	    $me -v $dir || exit 1
	fi
    done
exit 0
fi

echo "#################################################################"
if test -f /bin/pwd
then
    /bin/pwd
else
    pwd
fi
if test ! -r Gmakefile
then
    echo "Gmakefile not found (or not readable)"
    exit 1
fi

if grep -s '[({]GIS[)}]' Gmakefile
then
    echo "Old style Gmakefile - contains reference to GIS"
    echo "Modify it to use GISBASE or SRC"
    exit 1
fi

if grep -s '[({]GISDBASE[)}]' Gmakefile
then
    echo "Old style Gmakefile - contains reference to GISDBASE"
    echo "Modify it to use DEFAULT_DATABASE"
    exit 1
fi

# reject any explicit .o rules
badline="`awk '
	    /^ *[0-9a-zA-Z_\.\-]*\.o[ \	]*:/{line=NR;next}
	    /^\t./{if(line) {print line+1; exit}}
	    {line=0}' Gmakefile`"

if test ! "$badline" = ""
then
    echo Gmakefile line $badline - contains explicit .o action
    echo "Modify it to eliminate the action"
    exit 1
fi

if test ! -r "$TAIL"
then
    TAIL=/dev/null
fi

makefile=$OBJARCH/make.rules
if test ! -d $OBJARCH
then
    echo "  mkdir $OBJARCH"
    mkdir $OBJARCH || exit 1
fi
if test -f $makefile
then
    rm -f $makefile
fi

(
# build the make.rules file
# 
# define VERSION, SRC, OBJARCH for .o files, LIBARCH for .a files
echo VERSION_NUMBER="$VERSION_NUMBER"
echo VERSION_DATE="$VERSION_DATE"
echo VERSION_UPDATE_PKG="$VERSION_UPDATE_PKG"
echo VERSION_FILE="$VERSION_FILE"
echo SRC=$SRC
echo OBJARCH=$OBJARCH
echo LIBARCH=$LIBARCH
echo ""

# remove ARCH from $HEAD
# awk '$1 !~ /^ARCH$/ {print}' $HEAD
cat $HEAD

# define gmake
echo GMAKE = $me

# prepend all .o with $(OBJARCH) and .a files with $(LIBARCH)
sed \
    -e 's#[\*0-9a-zA-Z_\.\-\$\(\)]*\.o[ \	]#$(OBJARCH)/&#g' \
    -e 's#[\*0-9a-zA-Z_\.\-\$\(\)]*\.o:[ \	]#$(OBJARCH)/&#g' \
    -e 's#[\*0-9a-zA-Z_\.\-\$\(\)]*\.o$#$(OBJARCH)/&#g' \
    -e 's#[\*0-9a-zA-Z_\.\-\$\(\)]*\.a[ \	]#$(LIBARCH)/&#g' \
    -e 's#[\*0-9a-zA-Z_\.\-\$\(\)]*\.a:[ \	]#$(LIBARCH)/&#g' \
    -e 's#[\*0-9a-zA-Z_\.\-\$\(\)]*\.a$#$(LIBARCH)/&#g' \
    -e '/\.\\a[ \	]/s/\\//' \
    -e '/\.\\a:[ \	]/s/\\//' \
    -e '/\.\\a$/s/\\//' \
	$MID Gmakefile


echo ""
# extract .o files to be built from .c and .f files in the current directory
# and build explict make rules

sed -e 's/=/ /' -e 's/\\//' Gmakefile |\
 awk '{for(i=1;i<=NF;i++) \
    if ($i ~ /^[a-zA-Z0-9_\.\-\$\(\)]*\.o$/) print substr($i,1,length($i)-2)} ' |\
 sort -u |\
 (
    while read file
    do
	if test -f $file.f
	then
	    echo '$(OBJARCH)/'${file}.o: ${file}.f
#	    echo '	rm -f $@'
#	    echo '	$(FC) $(FFLAGS) -c' ${file}.f
#	    echo '	mv' ${file}.o '$@'
# new version MN:
	    echo '	$(FC) $(FFLAGS) -c' ${file}.f -o '$@'
	else
	    echo '$(OBJARCH)/'${file}.o: ${file}.c
#	    echo '	rm -f $@'
#	    echo '	$(CC) $(CFLAGS) -c' ${file}.c
#	    echo '	mv' ${file}.o '$@'
# new version MN:
	    echo '	$(CC) $(CFLAGS) -c' ${file}.c -o '$@'
	fi
    done
 )

cat $TAIL
) > $makefile

echo "  make -f $makefile $*"
echo ""
${MAKE} -f $makefile $*
status=$?
if test $status != 0 
then
    exit $status
fi

if test "$install" = "no"
then
    if test "$GMAKE_DEL_OBJ" != ""
    then
       echo rm -rf $OBJARCH
       rm -rf $OBJARCH
    fi
    exit 0
fi


###########
# install #
###########

makefile2=$OBJARCH/make2.rules
if test -f $makefile2
then
    rm -f $makefile2
fi

sed -e 's#^GISBASE\([ 	]*\)=.*$#GISBASE\1= \${prefix}/grass5#' $makefile \
	> $makefile2

echo "  make -f $makefile2 $*"
echo ""
${MAKE} -f $makefile2 $*
status=$?
if test $status != 0 
then
    exit $status
fi

if test "$GMAKE_DEL_OBJ" != ""
then
   echo rm -rf $OBJARCH
   rm -rf $OBJARCH
fi
exit 0
