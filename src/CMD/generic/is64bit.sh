:
# is a 64bit alpha machine ? if so set sizeoflong to 8.
# 
#    script by Luca Palmeri (lpalmeri@ux1.unipd.it)
#                                 07 Dec 1999
#                                 grass 5.0 beta5i
#
# To be executed right before starting compilation,
# checks for the presence of the string 'alpha' into
# the ARCH variable and for sizeof(long) to be equal
# to 8.         
# Invoked in $SRCDIR/Makefile during pre_install.
###################################################

HEAD_FILE=`dirname $0`"/../head/head"
PORTABLE="src/libes/vect32/diglib/portable.h"

####################### Set ARCH and CC variables
eval `cat ${HEAD_FILE} | grep ARCH | sed "s/ //g"`
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
$CC 64test.c

SIZEOFLONG=`./a.out`

rm ./a.out 64test.c

######################## Is that an alpha ?
ARCHIT=`echo $ARCH | grep -i "alpha"`

if test $SIZEOFLONG -eq 8 -a \
	 X$ARCHIT != X ; then
	 echo "[ 64bit alpha ]"
	 echo "  --> Setting size of long to 8"
	 cat $PORTABLE | \
         sed "s/#define LNG_SIZ  4/#define LNG_SIZ  8/" > $PORTABLE.new
	 mv $PORTABLE.new $PORTABLE
fi

