:
# is a 64bit alpha machine ? if so set sizeoflong to 8.
#
# $Id$
# 
# script by Luca Palmeri (lpalmeri@ux1.unipd.it)
# 07 Dec 1999
#
# To be executed right before starting compilation,
# checks for the presence of the string 'alpha' into
# the ARCH variable and for sizeof(long) to be equal
# to 8.         
# Invoked in $SRCDIR/Makefile during pre_install.
###################################################

ARCH=$1
HEAD_FILE=`dirname $0`"/../head/head.$ARCH"
GRASSLIST="src/CMD/lists/GRASS"

####################### Set ARCH and CC variables
#eval `cat ${HEAD_FILE} | grep ARCH | sed "s/ //g"`
eval `cat ${HEAD_FILE} | grep CC | sed "s/ //g"`

####################### Test for the 64bit pipe
cat > 64test.c << 'EOM'
int main()
{
  printf("%d",sizeof(long));
  return 0;
}
EOM
########################
$CC 64test.c -o a

SIZEOFLONG=`./a`

rm -f ./a a.exe 64test.c

######################## Is that an alpha ?
ARCHIT=`echo $ARCH | grep -i "alpha"`

if [ "$SIZEOFLONG" -eq "8" -a "X$ARCHIT" != "X" ]; then
	echo "[ 64bit alpha ]"
	echo "  --> Using 64-bit PVF library"
#not required any more:
#	cat $GRASSLIST | \
#	sed "s,vect32/diglib$,vect32/diglib64," > $GRASSLIST.new
#	mv $GRASSLIST.new $GRASSLIST
fi

