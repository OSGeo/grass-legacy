
eval `$GMAKE -sh` || exit 1
: ${GISBASE?} ${SRC?} ${CMD?}

#  Code to create links to front.end
for i in `( cd $GISBASE/etc/bin/cmd;       ls;
            cd $GISBASE/etc/bin/inter;     ls; ) | sort -u `
do
#    if [ ! -s $GISBASE/bin/$i ]
#    then
        echo Creating link for $i
		rm -f $GISBASE/bin/$i
        ln $GISBASE/etc/front.end $GISBASE/bin/$i
#    fi
done
