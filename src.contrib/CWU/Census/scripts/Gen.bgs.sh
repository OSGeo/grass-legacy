#!/bin/sh

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
# 9960 through 9964 for Sta. Cruz Co. AZ

#do whole county outline map and tract map and block group map

#g.remove vect=$DB.county
#g.remove vect=$DB.tract
g.remove vect=$DB.bg

v.db.rim $DB <<EOF
.q
where side1 eq 1
.e
.q -a
where ctbnal ne ctbnar
.e
.q -a
where bgl ne bgr
.e
.vect -s vectoff $DB.bg tlid
.ex
EOF

v.support <<EOF
$DB.bg
1


EOF

