vector=wdbtemp4
awk -F: ' {if (NF==2){print $2;}}' $LOCATION/dig_cats/$vector | sort -u
