:
ARCH=$1
HEAD_FILE=`dirname $0`"/../head/head.$ARCH"

# Save head for this architecture.
#ARCH="`sed 's/=/ /' $HEAD_FILE | awk '$1 ~ /^ARCH$/ {if(NF>1)print $2}'`"
#mv $HEAD_FILE $HEAD_FILE.$ARCH

# set (and create) UNIX_BIN
eval `cat ${HEAD_FILE} | grep UNIX_BIN | sed "s/ //g"`
if [ ! -d $UNIX_BIN ]; then mkdir -p $UNIX_BIN ; fi
if [ $? != 0 ]; then
 echo "An error occured. Stop."
 exit 1
fi

# create gmake5 script to be used for local compiling
echo ":"                                   > $UNIX_BIN/gmake$NAME_VER
echo "SRC=$SRC/src"                       >> $UNIX_BIN/gmake$NAME_VER
echo "CMD=$SRC/src/CMD"                   >> $UNIX_BIN/gmake$NAME_VER
echo "HEADER=head.$ARCH"                  >> $UNIX_BIN/gmake$NAME_VER
echo "HASX=yes"                           >> $UNIX_BIN/gmake$NAME_VER
echo "HASMotif=no"                        >> $UNIX_BIN/gmake$NAME_VER
echo "MAKE=$MAKE"                         >> $UNIX_BIN/gmake$NAME_VER
echo ". $SRC/src/CMD/generic/gmake.sh"    >> $UNIX_BIN/gmake$NAME_VER

if [ $? != 0 ]; then
 echo "An error occured. Stop."
 exit 1
fi

chmod ugo+x $UNIX_BIN/gmake$NAME_VER

# create gmakelinks script to be used for linking after
# local compiling
echo ":"                                      > $UNIX_BIN/gmakelinks$NAME_VER
echo "GMAKE=$UNIX_BIN/gmake$NAME_VER"        >> $UNIX_BIN/gmakelinks$NAME_VER
echo ". $SRC/src/CMD/generic/MAKELINKS.sh"   >> $UNIX_BIN/gmakelinks$NAME_VER
chmod ugo+x $UNIX_BIN/gmakelinks$NAME_VER

exit 0
