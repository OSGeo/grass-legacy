:
# DO NOT EDIT THIS FILE
##########################################################################
umask 002
cd /tmp

# GMAKE must be set outside this script, by CMD/GISGEN

: ${GMAKE?}
$GMAKE -sh
eval `$GMAKE -sh` || exit 1
: ${GISBASE?} ${SRC?} ${CMD?}

echo '####################################################################'
echo "GISGEN                             " `date`
echo " GISBASE = $GISBASE"
echo " SRC     = $SRC"
echo " CMD     = $CMD"
echo " HEADER  = $HEADER"

list=lists/GRASS
if test -r $CMD/lists/local
then
    list="$list lists/local"
fi
if test "$ARCH" = ""
then
    next_step=$CMD/next_step/next_step
else
    echo " ARCH    = $ARCH"
    next_step=$CMD/next_step/$ARCH
    if test -r $CMD/lists/$ARCH
    then
	list="$list lists/$ARCH"
    fi
fi
echo ""

if test ! -d $GISBASE
then
    echo "GISGEN failure: $GISBASE - directory doesn't exist"
    exit 1
fi

# Make sure GISBASE sub-directories exist
for i in \
    bin \
    man man/help man/1 man/2 man/3 man/4 man/5 \
    etc etc/paint etc/paint/driver etc/paint/driver.sh etc/sites \
    etc/dig_drivers etc/imagery \
    etc/bin etc/bin/main etc/bin/alpha etc/bin/contrib \
    etc/bin/main/inter etc/bin/main/cmd \
    etc/bin/alpha/inter etc/bin/alpha/cmd \
    etc/bin/contrib/inter etc/bin/contrib/cmd \
    txt txt/COMBINE txt/DIGIT txt/DIGIT2 txt/WEIGHT txt/MONITOR \
    garden garden/bin garden/etc \
    driver locks dev
do
    if test ! -d $GISBASE/$i
    then
	mkdir $GISBASE/$i || exit 1
	echo $GISBASE/$i created
    fi
done
chmod 0777 $GISBASE/locks

cd $CMD

# create fifo files for graphics drivers
for i in a b
do
	fifo=0
	while test $fifo -lt 21
	do
		if test ! -p $GISBASE/dev/fifo.$fifo$i
		then
		/etc/mknod $GISBASE/dev/fifo.$fifo$i p
		chmod 0666 $GISBASE/dev/fifo.$fifo$i
		echo $GISBASE/dev/fifo.$fifo$i created
		fi
		fifo=`expr $fifo + 1`
	done
done


if test "$1" = "-all"
then
    shift
    rm -f $next_step
elif test "$1" = "-skip"
then
    shift
    skip=yes
fi
# determine where to start and what's left to do
case $# in
0)
    if test -r $next_step
    then
	STEP=`cat $next_step`
    else
	STEP=`awk '{print $0;exit}' $list`
    fi
    echo first step: $STEP

    tmp1=/tmp/GISGEN1.$$
    tmp2=/tmp/GISGEN2.$$
    tmp3=/tmp/GISGEN3.$$
    rm -f $tmp1
	touch $tmp1
	for i in $list
	do
		grep -v '^#' $i >> $tmp1
	done
    echo DONE >> $tmp1

    rm -f $tmp2
    rm -f $tmp3
    echo "a == 1 { print \$0 ; next }" > $tmp3
    echo "\$0 == \"$STEP\" { a = 1; print \$0 }" >> $tmp3
    awk -f $tmp3 $tmp1 > $tmp2
    rm -f $tmp3 $tmp1

    STEPS=`cat $tmp2`
    rm -f $tmp2
    if test "$STEPS" = ""
    then
	echo ""
	echo "GISGEN failure: $STEP - not found in $list"
	exit 1
    fi

    set $STEPS
    ;;
*)
    single=1
    ;;
esac

if test "$skip" = yes
then
    echo "Skipped"
    shift
fi
for i
do
    cd $SRC/..
    echo ""
    echo GISGEN: $i - `date`
    echo ""
    bailout=0
    if test "$single" != 1
    then
	echo $i > $next_step
    fi

    case $i in
    DONE)
	echo DONE generating GIS binary code
	exit 0
	;;

    *)
	$GMAKE -v $i
	case $? in
	0) ;;
	*) bailout=1 ;;
	esac
	;;
    esac

    case $bailout in
    1)
	echo GISGEN failure at STEP: $i
	exit 1
	;;
    *) ;;
    esac

done
