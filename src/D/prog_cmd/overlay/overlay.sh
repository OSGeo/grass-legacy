if [ $# != 1 ]
then
	echo usage: `basename $0` cell_file
	exit 1
fi
Dcell "$1" o=1
