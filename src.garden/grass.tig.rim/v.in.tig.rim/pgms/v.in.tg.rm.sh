:

if [ "$MAPSET" = "" ]
then
echo "You must be in GRASS4 to run this program."
exit
fi

if [ $# -lt 4 ]
then
echo "USAGE: $0 db Tiger.type1 Tiger.type2 utm_zone"
echo "  Where Tiger.type1 is path name to Tiger Type 1 Data File"
echo "        Tiger.type2 is path name to Tiger Type 2 Data File"
echo "    and db is name to give new v.db.rim/Rim data base (7 char limit)."
echo "  utm_zone is the target zone for lat-long to utm conversions."
echo "  Data base name will also be used to make master binary"
echo "    digit (dig) file (db.Master) in the current mapset <$MAPSET>."
exit 1
fi

ETC=$GISBASE/etc/tiger_support

if [ -f $ETC/db.layout -a -f $ETC/tiger1.format ]
then
echo "db.layout and tiger1.format files found"
else
echo "ERROR: db.layout and/or tiger1.format file not found in support directory"
exit 1
fi

DB=$1

TIGERtmp1=`g.tempfile pid=$$`
TIGER1=`g.tempfile pid=$$`

TIGER2=$3
MASTER_MAP=$DB.Master
ZONE=$4

CUR_DIR=`pwd`
MACHINE=`hostname`

# check to see if the tiger data needs linefeeds added to records

echo "Checking Tiger Type1 input file for linefeeds"
$GISBASE/etc/tig.linefeed < $2 > $TIGERtmp1

case $? in
    0) name1=$TIGERtmp1;;
    1) name1=$2;;
    *)  echo "Error in checking for linefeeds in input data file"
        rm -f $TIGERtmp1
        exit 2
esac

echo "Sorting records from $2"
echo "into $TIGER1 by recnum."
sort -T $LOCATION/.tmp/`hostname` +0.5 -0.14 < $name1 >$TIGER1
date
echo "Done."

# remove temp file, if any
if [ -f $TIGERtmp1 ]
then
rm $TIGERtmp1
fi

# Remove data base, if any.
rm -f $LOCATION/rim/vect/$DB.rimdb*

# Build new data base
v.db.rim $DB << EOF
.in $ETC/db.layout
.ex
EOF

# Load all data
echo "Loading data from sorted Tiger Type1 file: $TIGER1"
date
rim << EOF
open $LOCATION/rim/vect/$DB
remove key for recnum in data
load referencemaps
1 "$MASTER_MAP" "$MAPSET"
end
load data from "$TIGER1" using $ETC/tiger1.format
exit
EOF
echo "Done loading Type 1 into v.db.rim data base."
date

# change Vect type and Map number to Required values.
echo "Changing Vector Type to A and Map Number to 1 for all records"
echo "and building recnum key."
cd $LOCATION/.tmp/$MACHINE
rim << EOF
open $LOCATION/rim/vect/$DB
change VT in data to 'A' where RECNUM exists
change MAP in data to 1 where RECNUM exists
build key for recnum in data
exit
EOF
cd $CUR_DIR

#4.0 version had c12.to.v use $TIGERtmp2 whether the original
# file already had linefeeds or not.  If original file was ok,
# then it was copied to $TIGERtmp2...this isn't necessary -
# just use the file where it is -- now name2 references correct
# file for c.12.to.v to use    ...msl

# check to see if the tiger data needs linefeeds added to records
TIGERtmp2=`g.tempfile pid=$$`
$GISBASE/etc/tig.linefeed < $TIGER2 >$TIGERtmp2

case $? in
    0) name2=$TIGERtmp2;;
    1) name2=`pwd`/$TIGER2 ;;
    *)  echo "Error in checking for linefeeds in input data file"
        rm -f $TIGERtmp2
        exit 2
esac

# Now make the dig binary file
echo "Building the binary vector file <$MASTER_MAP> from Types 1 and 2"
date
DBtmp=`g.tempfile pid=$$`
$GISBASE/etc/c12.to.v $DB $TIGER1 $name2 z=$ZONE s=clark66 $DBtmp
rm $TIGER1 $TIGERtmp2

echo " "
echo "Updating the data base for record offsets in vector file..."
v.db.rim $DB < $DBtmp

echo "Building the rest of the keys."
cd $LOCATION/.tmp/$MACHINE
rim << EOF
open $LOCATION/rim/vect/$DB
build key for cfcc in data
build key for ctbnal in data
build key for ctbnar in data
build key for bgl in data
build key for bgr in data
exit
EOF
cd $CUR_DIR

rm $DBtmp

echo "Done.  $DB.Master vector file and $DB v.db.rim data base built."
date

