:

# Shell script useful for starting manual page documentation of
# GRASS commands.

if test $# -ne 1
then
	echo Usage: $0 grass.command
	exit
fi

if test ! -x $GISBASE/bin/$1
then
	echo Sorry, $1 is not a GRASS command
	exit
fi

echo .TH $1
echo .SH NAME
echo \\fI$1\\fR \\- ENTER ONE LINE DESCRIPTION HERE
echo .br
echo -n .I '"(GRASS '
case `echo $1 | sed -e 's/\(.\).*/\1/'` in
	d) echo -n Display ;;
	i) echo -n Imagery ;;
	r) echo -n Raster ;;
	v) echo -n Vector ;;
	s) echo -n Sites ;;
	m) echo -n Data Transformation ;;
	p) echo -n Hardcopy Output ;;
	g) echo -n Data Management ;;
	*) echo -n UNKNOWN ;;
esac
echo ' Program)"'
echo .SH SYNOPSIS
echo "\\fB$1\\fR"
echo .br
echo "\\fB$1 help\\fR"

for i in main alpha contrib
do
	if test -x $GISBASE/etc/bin/$i/cmd/$1
	then
		echo .br
		$GISBASE/bin/$1 help 2>&1 | \
		sed -e '1,/Usage/d' \
			-e '/^$/,$d' \
			-e 's/\\//' \
			-e 's/\[-\([^ ]*\)\]/[\\fB\\-\1\\fR]/' \
			-e '/^ [^ ]/s/ \([^ ]*\)/\\fB\1\\fR/' \
			-e 's/\([^ =]*\)=\([^ ]*\)/\\fB\1=\\fI\2\\fR/g'
	fi
done

echo .SH DESCRIPTION
echo enter short description of $1 here
echo ""

for i in main alpha contrib
do
	if test -x $GISBASE/etc/bin/$i/cmd/$1
	then
		echo .SH '"COMMAND LINE OPTIONS"'
		$GISBASE/bin/$1 help 2>&1 | \
		sed -e '1,/^$/d' \
			-e '/^$/d' \
			-e '/^[^ ]*:/s/^\([^:]*\).*/.LP\
\1/' \
			-e '/options:/s/ *\(.*\)/.br\
\1/' \
			-e '/default:/s/ *\(.*\)/.br\
\1/' \
			-e '/^  *[^ ][^ ]* *$/s/ *//' \
			-e '/^ /s/ *\([^ ]*\) */.IP \\fI\1\\fR\
/' \
			-e 's/,/, /g'
	fi
done

for i in main alpha contrib
do
	if test -x $GISBASE/etc/bin/$i/inter/$1
	then
		echo .SH '"INTERACTIVE MODE"'
	fi
done

echo ""
echo .SH BUGS
echo Describe known bugs here
echo ""
echo .SH '"SEE ALSO"'
echo Refer reader to related programs here
echo ""
echo .SH AUTHOR
echo Provide the programmer's name and institution
