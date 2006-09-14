projshare=$1
file=$2

# MS-Windows TclTk does not recognize POSIX path.
if [ "$file" = "epsg_option.tcl.in" -a -n "$MSYSCON" ] ; then
	msys_path=`echo $WD | sed -e 's+\\\\+/+g' -e 's+//bin/$++'`
	projshare=`echo "$projshare" | sed "s+^/usr+$msys_path+"`
fi
sed -e "s+PROJSHARE+$projshare+" $file
