#############################################################################
#
# $Id$
#
# MODULE:   	Grass Initialization
# AUTHOR(S):	Original author unknown - probably CERL
#   	    	Huidae Cho - Korea - hdcho@geni.knu.ac.kr
#   	    	Justin Hickey - Thailand - jhickey@hpcc.nectec.or.th
#   	    	Markus Neteler - Germany - neteler@geog.uni-hannover.de
# PURPOSE:  	The source file for this shell script is in
#   	    	src/general/init/init.sh. It sets up some environment
#   	    	variables and the lock file. It also parses any remaining
#   	    	command line options for setting the GISDBASE, LOCATION, and/or
#   	    	MAPSET. Finally it starts grass with the appropriate user
#   	    	interface and cleans up after it is finished.
# COPYRIGHT:    (C) 2000 by the GRASS Development Team
#
#               This program is free software under the GNU General Public
#   	    	License (>=v2). Read the file COPYING that comes with GRASS
#   	    	for details.
#
#############################################################################

: ${GISBASE?}

echo "Starting GRASS ..."

# Check if we need to find wish
if [ $GRASS_GUI = "tcltk" ] ; then
    
    # Search for a wish program
    SEARCHCOMMAND=wish
    WISH=""
    found=0
    
    for i in `echo $PATH | sed 's/^:/.:/
    	    	    		s/::/:.:/g
				s/:$/:./
				s/:/ /g'`
    do
	if [ -f $i/$SEARCHCOMMAND ] ; then
    	    WISH=$i/$SEARCHCOMMAND
	    found=1
	fi
    done

    #NEEDED: is wish >= wish8.0? Because wish4.2 won't work.

    # Check if any wish8.x programs are stored in $PATHLIST 
    if [ $found -gt 0 ] ; then
    	
	# Take the first and the tcltkgrass base directory
	TCLTKGRASSBASE=$GISBASE/tcltkgrass
	export TCLTKGRASSBASE
    else
    	
	# Wish was not found - switch to text interface mode
	echo ""
    	echo "WARNING: The wish command was not found!"
	echo "Switching to text based interface mode"
	sleep 2
    	
	GRASS_GUI="text"
    fi
fi

# Set PAGER environment variable if not set
if [ ! "$PAGER" ] ; then
    PAGER=more
    export PAGER
fi

# Get name of lockfile and gisrc file
lockfile=$HOME/.gislock5
GISRC=$HOME/.grassrc5
export GISRC

# Set the GIS_LOCK variable to current process id
GIS_LOCK=$$
export GIS_LOCK

# Set PATH to GRASS bin, ETC to GRASS etc
ETC=$GISBASE/etc
PATH=$GISBASE/bin:$GISBASE/scripts:$PATH
export PATH

# Check for concurrent use
$ETC/lock $lockfile $$
case $? in
    0) ;;
    1)
    	echo `whoami` is currently running GRASS. Concurrent use not allowed.
    	exit ;;
    *)
    	echo Unable to properly access $lockfile
    	echo Please notify system personel.
    	exit ;;
esac

# First time user ...
if [ ! -f $GISRC ] ; then
    cat $ETC/grass_intro
    echo ""
    echo "Hit RETURN to continue"
    read ans
fi

# Parsing argument to get LOCATION
if [ ! "$1" ] ; then

    # Try interactive startup
    LOCATION=
else

    # Try non-interactive startup
    L=
    
    if [ "$1" = "-" ] ; then
    
    	if [ "$LOCATION" ] ; then
    	    L=$LOCATION
    	fi
    else
    	L=$1
    
    	if [ `echo $L | cut -c 1` != "/" ] ; then
    	    L=`pwd`/$L
    	fi
    fi

    if [ "$L" ] ; then
    	MAPSET=`basename $L`
    	L=`dirname $L`
    
    	if [ "$L" != "." ] ; then
    	    LOCATION_NAME=`basename $L`
    	    L=`dirname $L`
    
    	    if [ "$L" != "." ] ; then
    	    	GISDBASE=$L
    	    fi
    	fi
    fi

    if [ "$GISDBASE" -a "$LOCATION_NAME" -a "$MAPSET" ] ; then
    	LOCATION=$GISDBASE/$LOCATION_NAME/$MAPSET
    
    	if [ ! -r $LOCATION/WIND ] ; then
    	    echo "$LOCATION: No such location"
    	    exit
    	fi
    	export GISDBASE LOCATION_NAME MAPSET
    
    	if [ -s $GISRC ] ; then
    	    sed -e "s|^GISDBASE:.*$|GISDBASE: $GISDBASE|; \
    	    	s|^LOCATION_NAME:.*$|LOCATION_NAME: $LOCATION_NAME|; \
    	    	s|^MAPSET:.*$|MAPSET: $MAPSET|" $GISRC > $GISRC.$$
    
    	    if [ $? -eq 0 ] ; then
    	    	mv -f $GISRC.$$ $GISRC
    	    else
    	    	rm -f $GISRC.$$
    	    	echo "Failed to create new $GISRC"
    	    	LOCATION=
    	    fi
    	else
    	    echo "GISDBASE: $GISDBASE" > $GISRC
    	    echo "LOCATION_NAME: $LOCATION_NAME" >> $GISRC
    	    echo "MAPSET: $MAPSET" >> $GISRC
    	fi
    else
    	echo "GISDBASE, LOCATION_NAME and MAPSET variables not set properly."
    	echo "Interactive startup needed."
    	exit
    fi
fi

# User selects LOCATION and MAPSET if not set
if [ ! "$LOCATION" ] ; then
    
    case $GRASS_GUI in
    	
	# Check for text interface
	text)
	    $ETC/set_data
    	    
	    case $? in
     	    	0) ;;
     	    	*) exit ;;
    	    esac
	    
	    eval `g.gisenv`
	    ;;
	
	# Check for tcltk interface
	tcltk)
	    eval `$WISH -file $TCLTKGRASSBASE/script/gis_set.tcl`
	    
	    # These checks should not be necessary with real init stuff
	    if [ $LOCATION_NAME = "##NONE##" ] ; then
    	    	$ETC/set_data
    	    	if [ $? != 0 ]; then
    	    	    echo "GISDBASE: $OLD_DB" > $GISRC
    	    	    echo "LOCATION_NAME: $OLD_LOC" >> $GISRC
    	    	    echo "MAPSET: $OLD_MAP" >> $GISRC
    	    	    exit
    	    	fi
    	    	eval `g.gisenv`
    	    fi

	    if [ $LOCATION_NAME = "##ERROR##" ] ; then
    	    	echo "The selected location is not a valid GRASS location"
    	    	exit
	    fi
    	    
	    ;;
	*)
	    # Shouldn't need this but you never know
	    echo "ERROR: Invalid user interface specified."
	    echo "Use the -help option to grass for valid interfaces."
	    exit
	    ;;
    esac
fi

LOCATION=${GISDBASE?}/${LOCATION_NAME?}/${MAPSET?}
export LOCATION

trap "" 2 3

sh="`basename $SHELL`"
case $sh in
    ksh)  shellname="Korn Shell";;
    csh)  shellname="C Shell" ;;
    tcsh) shellname="TC Shell" ;;
    bash) shellname="Bash Shell" ;;
    sh)   shellname="Bourne Shell";;
    *)    shellname=shell;;
esac

# Start the chosen GUI but ignore text
case $GRASS_GUI in
    
    # Check for tcltk interface
    tcltk)
        $GISBASE/bin/tcltkgrass &
	;;
    
    # Ignore others
    *)
    	;;
esac

# Display the version and license info
clear

echo "Welcome to GRASS VERSION_NUMBER (VERSION_DATE) VERSION_UPDATE_PKG"
echo
echo "Geographic Resources Analysis Support System (GRASS) is Copyright,"
echo "1999-2000 by the GRASS Development Team, and licensed under terms of the"
echo "GNU General Public License (GPL)."
echo 
echo "This GRASS VERSION_NUMBER release is coordinated and produced by the"
echo "GRASS Development Team headquartered at University of Hannover with"
echo "worldwide support and further development sites located at Baylor"
echo "University and the University of Illinois."
echo 
echo "This program is distributed in the hope that it will be useful, but"
echo "WITHOUT ANY WARRANTY; without even the implied warranty of"
echo "MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU"
echo "General Public License for more details."
echo 
echo "This version running thru the $shellname ($SHELL)"
echo "Help is available with the command:      g.help"
echo "See the licence terms with:              g.version"
#
#if [ $GRASS_GUI = "text" ] ; then
    echo "Start the graphical user interface with: tcltkgrass&"
#fi

echo "When ready to quit enter:                exit"

case "$sh" in

csh|tcsh)
    USERHOME=$HOME      # save original home
    HOME=$LOCATION
    export HOME
    cshrc=$HOME/.cshrc
    tcshrc=$HOME/.tcshrc
    rm -f $cshrc $tcshrc
    echo "set home = $USERHOME" > $cshrc
    echo "set history = 30 noclobber ignoreeof" >> $cshrc

    echo "set prompt = '\\" >> $cshrc
    echo "Mapset <${MAPSET}> in Location <${LOCATION_NAME}> \\" >> $cshrc
    echo "GRASS VERSION_NUMBER > '" >> $cshrc
    echo 'set BOGUS=``;unset BOGUS' >> $cshrc

    if [ -r $home/.grass.cshrc ]
    then
	cat $home/.grass.cshrc >> $cshrc
    fi

    if [ -r $home/.cshrc ]
    then
	grep '^ *set  *mail *= *' $home/.cshrc >> $cshrc
    fi

    if [ -r $home/.tcshrc ]
    then
	grep '^ *set  *mail *= *' $home/.tcshrc >> $cshrc
    fi

    if [ -r $home/.login ]
    then
	grep '^ *set  *mail *= *' $home/.login >> $cshrc
    fi

    echo "set path = ( $PATH ) " | sed 's/:/ /'g >> $cshrc

    cp $cshrc $tcshrc
    $ETC/run $SHELL

    HOME=$USERHOME
    export HOME
    ;;

bash)
    USERHOME=$HOME      # save original home
    HOME=$LOCATION      # save .bashrc in $LOCATION
    export HOME
    bashrc=$HOME/.bashrc
    rm -f $bashrc
    echo "test -z $PROFILEREAD && . /etc/profile" > $bashrc
    echo "test -e ~/.alias && . ~/.alias" >> $bashrc
    echo "umask 022" >> $bashrc
    echo "PS1='GRASS:\w > '" >> $bashrc

    if [ -r $home/.grass.bashrc ]
    then
        cat $home/.grass.bashrc >> $bashrc
    fi

    echo "export PATH=$PATH" >> $bashrc
    echo "export HOME=$USERHOME" >> $bashrc # restore user home path

    $ETC/run $SHELL
    HOME=$USERHOME
    export HOME
    ;;

*)

PS1="
Mapset <$MAPSET> in Location <$LOCATION_NAME>
GRASS-GRID > "

    export PS1

    $ETC/run $SHELL
    ;;
esac

trap 2 3

# Grass session finished
clear
echo "Cleaning up temporary files....."

($ETC/clean_temp > /dev/null &)
rm -f $lockfile

echo "done"
echo 
echo 
echo 
echo "Goodbye from GRASS GIS"
echo
