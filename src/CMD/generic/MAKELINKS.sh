
eval `$GMAKE -sh` || exit 1
: ${GISBASE?} ${SRC?} ${CMD?}

#  Code to create links to front.end
for i in `( cd $GISBASE/etc/bin/main/cmd;       ls;
            cd $GISBASE/etc/bin/main/inter;     ls;
            cd $GISBASE/etc/bin/alpha/cmd;      ls;
            cd $GISBASE/etc/bin/alpha/inter;    ls;
            cd $GISBASE/etc/bin/contrib/cmd;    ls;
            cd $GISBASE/etc/bin/contrib/inter;  ls; ) | sort -u `
do
#    if [ ! -s $GISBASE/bin/$i ]
#    then
        echo Creating link for $i
		rm -f $GISBASE/bin/$i
        ln $GISBASE/etc/front.end $GISBASE/bin/$i
#    fi
done
