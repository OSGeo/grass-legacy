:
CATh=`head -1 $LOCATION/$mapset/dig_cats/*|sed -e "s/ categories//" -e "s/#//" -e "s/ //g"`
CATt=`tail -1 $LOCATION/$mapset/dig_cats/*|cut -d":" -f1 |sed -e "s/ //g"`
if [ "$CATh" -lt "$CATt" ]; then
	echo
	echo "ERROR:  Your dig_cats file $name is not correct."
	echo "Please Run the Command v.support to update the category information."
	echo "Maximum category number should be <$CATt> not <$CATh>."
	echo "EXITING..........................................................."
sleep 3
	exit
else
	echo
	echo "Highest category value in $name = $CATh"
fi
ATT=`cut -c32-50 $LOCATION/$mapset/dig_att/*|sort|tail -1 |sed -e "s/ //g"`
if [ "$ATT" -gt "$CATh" ]; then
	echo
	echo "ERROR:  Your dig_att file may have a label ERROR"
	echo "Label Value <$ATT> is larger than the maximum category number <$CATh>"
	echo "Please check the file $LOCATION/../$mapset/dig_att/$name"
	echo "EXITING..........................................................."
sleep 3
	exit
else
	echo
	echo "Highest Label value in $name = $ATT"
sleep 3
fi
