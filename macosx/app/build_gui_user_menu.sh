#!/bin/sh

# Build addon menu files, from the global /Library/GRASS/$GRASS_MMVER/Modules
# and the user's $HOME/Library/GRASS/$GRASS_MMVER/Modules.
# It builds a etc/gm/addons folder full of tcl .menu menu files.

# test files to make sure they are appropriate for adding to the GUI menu.
# Using 'file', assume executable binaries OK.  Check scripts to see if they
# have GRASS options configured - a simple grep for #%Module.
# Other script languages may need their own test.

# addon commands can't have spaces in them or sh for loop messes up.
# may be my limited knowledge of sh scripting and there could be a way.

GRASS_MMVER=`cut -d . -f 1-2 "$GISBASE/etc/VERSIONNUMBER"`
BINDIR="$HOME/Library/GRASS/$GRASS_MMVER/Modules/bin"
BINDIRG="/Library/GRASS/$GRASS_MMVER/Modules/bin"
MENUDIR="$HOME/Library/GRASS/$GRASS_MMVER/Modules/etc/gm/addons"

echo "Rebuilding Addon menu..."

mkdir -p "$MENUDIR"
rm -f "$MENUDIR/"*.menu

# global addons:
if [ -d "$BINDIRG" ] ; then
  cd "$BINDIRG"
  CMDLISTG=`ls -1 2> /dev/null | sort -u`
else
  CMDLISTG=""
fi

if [ "$CMDLISTG" != "" ] ; then
  for i in $CMDLISTG
  do
    ftype="`file $BINDIRG/$i`"
    if [ "`echo $ftype | grep 'Mach-O'`" ] || [ "`grep '#% *Module' $BINDIRG/$i`" ] ; then
      echo "command {[G_msg \"$i\"]} {} \"$i\" {} -command {execute $i }" > $MENUDIR/$i.menu
    fi
  done
fi

# user addons pages:
cd "$BINDIR"
CMDLIST=`ls -1 2> /dev/null | sort -u`

if [ "$CMDLIST" != "" ] ; then
  for i in $CMDLIST
  do
    ftype="`file $BINDIR/$i`"
    if [ "`echo $ftype | grep 'Mach-O'`" ] || [ "`grep '#% *Module' $BINDIR/$i`" ] ; then
      echo "command {[G_msg \"$i\"]} {} \"$i\" {} -command {execute $i }" > $MENUDIR/$i.menu
    fi
  done
fi
