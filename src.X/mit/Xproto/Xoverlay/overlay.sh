if [ $# != 1 ]
then
	echo usage: `basename $0` cell_file
	exit 1
fi
xcell $1 o=1
