: ${GISBASE?}

# get home directory, then construct path to the databases file

home=`cd;pwd`
xgrassdbfile=$home/.xgrass/databases
appendfile=/tmp/xgdbap$$
tmpfile=/tmp/xgdb$$
append=n

trap "rm -f $tmpfile; rm -f $appendfile; exit 1" 2 3


totalcont=t
while test "$totalcont"
do
    if test -f $xgrassdbfile
    then
	echo A databases file exists. 
        echo "Should we remove it ? (y/n) [n]" | awk '{printf("'%s'",$0)}'
        read ans
	rm -f $appendfile $tmpfile
        if test "$ans" = "y"
        then
            rm $xgrassdbfile
	    echo Enter desired databases [return to quit]:
        else
	    echo Will append any databases you enter.
	    append=y
	    cat $xgrassdbfile > $appendfile
	    cat <<EOF
Your current database list:
EOF
	    echo ----------
	    cat $xgrassdbfile
	    echo ----------
	    echo Enter additional databases [return to quit]:
        fi
    else
	echo Enter desired databases [return to quit]:
    fi

    cont=t

    while test "$cont"
    do
	echo '> ' | awk '{printf("'%s'",$0)}'
	read x
	if test "$x" = ""
	then 
	    cont=
	else
	    echo $x >> $tmpfile
	fi
    done

    cat <<EOF

Your current database list:
EOF
    echo ----------
    if test -f $appendfile
    then
	cat $appendfile
    fi
    if test -f $tmpfile
    then
	cat $tmpfile
    fi
    echo ----------
    echo "Is this ok ? (y/n) [n]" | awk '{printf("'%s'",$0)}'
    read x
    if test "$x" = "y"
    then
	if test -f $appendfile
	then
	    cat $appendfile > $xgrassdbfile
	    rm -f $appendfile
	fi
	if test -f $tmpfile
	then
	    cat $tmpfile >> $xgrassdbfile
	    rm -f $tmpfile
	fi
	totalcont=
    else
	rm -f $tmpfile
    fi
done

rm -f $tmpfile $appendfile
