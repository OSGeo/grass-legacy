:

if [ $# -lt 2 ]
then
  echo "USAGE: $0 db tractN1 tractN2 ......"
  exit 1
fi

# Generate the standard set of county, Tract, Block Group and Block Maps
DB=$1
shift
TLIST=$*
# 9961 9962 9963 9964 9965 for Lawrence Co. SD
# 9751 9752 9753 9754 9755 9756 9757 for Kittitas Co. WA
# 9601 through 9612 f0r Chelan Co. WA

#do whole county outline map and tract map and block group map

g.remove vect=$DB.county
g.remove vect=$DB.tract
g.remove vect=$DB.bg

v.db.rim $DB <<EOF
.q
where sidecde eq 1
.e
.vect -s vectoff $DB.county recnum
.q -a
where ctbnal ne ctbnar
.e
.vect -s vectoff $DB.tract recnum
.q -a
where bgl ne bgr
.e
.vect -s vectoff $DB.bg recnum
.ex
EOF
v.support <<EOF
$DB.county
1


EOF
v.support <<EOF
$DB.tract
1


EOF
v.support <<EOF
$DB.bg
1


EOF

# do each tract
for T in $TLIST
do
echo Starting Tract $T at
date
# do tract outline
g.remove vect=T$T
v.db.rim $DB <<EOF
.q
where ctbnal=$T and (sidecde eq 1 or ctbnal ne ctbnar)
.e
.q -a
where ctbnar=$T and (sidecde eq 1 or ctbnal ne ctbnar)
.e
.vect -s vectoff T$T recnum
.ex
EOF

LO=100
while [ $LO -lt 1000 ]
do
echo Tract $T block group $LO
BGMAP=$T.$LO.bg
BLMAP=$T.$LO.bk
g.remove vect=$BGMAP,$BLMAP

v.db.rim $DB <<EOF
.q
where ctbnal eq $T and bgl eq $LO and (bgl ne bgr or ctbnal ne ctbnar)
.e
.q -a
where ctbnar eq $T and bgr eq $LO and (bgl ne bgr or ctbnal ne ctbnar)
.e
.vect -s vectoff $BGMAP recnum
.q -a
where ctbnal eq $T and bgl eq $LO and blkl ne blkr
.e
.vect -s vectoff $BLMAP recnum
.ex
EOF
# Stop this block group if no more maps
if [ ! -f $LOCATION/dig/$BGMAP ]
then
  break
fi
# Build topology
v.support <<EOF
$BGMAP
1


EOF
v.support <<EOF
$BLMAP
1


EOF
LO=`expr $LO + 100`
#end of blk grp loop
done
#end of tract loop
done
