@@ -0,0 +1,19 @@
#!/bin/sh

# this script try to locate all the GRASS script than got hardcoded
# the building path $HOME/... (given as an argument) and to replace it
# with the default /usr/lib/grass

ZOTPATH="$1"
TMPPATH="$2"
echo Mumble mumble... trying to zot:
echo "    $ZOTPATH"
echo " in $TMPPATH"

for i in `grep -r $ZOTPATH $TMPPATH | cut -d : -f 1 | sort -u` ; do
    echo -n "Grr. Zotting $i ... "
    cp $i $i.old
    sed -e s,$ZOTPATH,,g $i.old >$i
    rm -f $i.old
    echo done
done
