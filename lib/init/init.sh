#! /bin/sh
#############################################################################
#
# $Id$
#
# MODULE:   	GRASS Initialization
# AUTHOR(S):	Original author unknown - probably CERL
#               Andreas Lange - Germany - andreas.lange@rhein-main.de
#   	    	Huidae Cho - Korea - hdcho@geni.knu.ac.kr
#   	    	Justin Hickey - Thailand - jhickey@hpcc.nectec.or.th
#   	    	Markus Neteler - Germany/Italy - neteler@itc.it
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

trap "echo 'User break!' ; exit" 2 3 9 15

# Set the GRASS_PERL variable
GRASS_PERL=PERL_COMMAND
export GRASS_PERL

# Get the command name
CMD_NAME=`basename "$0"`

# Go through the command line options
for i in "$@" ; do
    
    # Use a case to check the command line options
    case "$i" in
    
    	# Check if the user asked for the version
	-v|--version)
	    cat "$GISBASE/etc/license"
	    exit
	    ;;

    	# Check if the user asked for help
	help|-h|-help|--help)
	    echo "Usage:"
	    echo "  $CMD_NAME [-h | -help | --help] [-text | -tcltk] [[[<GISDBASE>/]<LOCATION_NAME>/]<MAPSET>]"
	    echo
            echo "Flags:"
            echo "  -h or -help or --help          print this help message"
            echo "  -text                          use text based interface"
            echo "  -tcltk                         use Tcl/Tk based graphical interface"
            echo
            echo "Parameters:"
            echo "  GISDBASE                       initial database"
            echo "  LOCATION_NAME                  initial location"
            echo "  MAPSET                         initial mapset"
            echo
            echo "  GISDBASE/LOCATION_NAME/MAPSET  fully qualified initial LOCATION directory"
            echo
            echo "Environment variables:"
            echo "  GRASS_TCLSH                    set tclsh shell name to override 'tclsh'"
            echo "  GRASS_WISH                     set wish shell name to override 'wish'"
            echo "  GRASS_HTML_BROWSER             set html browser for help pages"
            echo "  GRASS_ADDON_PATH               set additional path(s) to local GRASS modules"
	    exit
	    ;;
	
	# Check if the -text flag was given
	-text)
	    GRASS_GUI="text"
	    shift
	    ;;
	
	# Check if the -tcltk flag was given
	-tcltk)
	    GRASS_GUI="tcltk"
	    shift
	    ;;
    esac
done

# Set the GRASSRC file
GISRC="$HOME/.grassrc51"
export GISRC

# At this point the grass user interface variable has been set from the
# command line, been set from an external environment variable, or is 
# not set. So we check if it is not set
if [ ! "$GRASS_GUI" ] ; then
    
    # Check for a reference to the grass user interface in the grassrc file
    if [ -f "$GISRC" ] ; then
    	GRASS_GUI=`awk '/GRASS_GUI/ {print $2}' "$GISRC"`
    fi
    
    # Set the grass user interface to the default if needed - currently tcltk
    if [ ! "$GRASS_GUI" ] ; then
    	GRASS_GUI="tcltk"
    fi
fi

# Export the user interface variable
export GRASS_GUI

# Set the GIS_LOCK variable to current process id
lockfile="$HOME/.gislock51"
GIS_LOCK=$$
export GIS_LOCK

# Set PATH to GRASS bin, ETC to GRASS etc
ETC=$GISBASE/etc

if [ "$LC_ALL" ] ; then
	LCL=`echo "$LC_ALL" | sed 's/\(..\)\(.*\)/\1/'`
elif [ "$LC_MESSAGES" ] ; then
	LCL=`echo "$LC_MESSAGES" | sed 's/\(..\)\(.*\)/\1/'`
else
	LCL=`echo "$LANG" | sed 's/\(..\)\(.*\)/\1/'`
fi

PATH=$GISBASE/bin:$GISBASE/scripts:$PATH:$GRASS_ADDON_PATH
export PATH

# Set LD_LIBRARY_PATH.  For GRASS 5.1 we don't depend on this much, though
# r.in.gdal may use it to find some things.  Over time we intend to put
# more GRASS related shared libraries in $GISBASE/lib.
# first search local libs, then in GRASS lib/
if [ ! "$LD_LIBRARY_PATH" ] ; then
  LD_LIBRARY_PATH=$GISBASE/lib
  export LD_LIBRARY_PATH
else
  LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$GISBASE/lib
fi

# Check for concurrent use
"$ETC/lock" "$lockfile" $$
case $? in
    0) ;;
    1)
    	echo `whoami` is currently running GRASS VERSION_NUMBER. Concurrent use not allowed.
    	exit ;;
    *)
    	echo Unable to properly access "$lockfile"
    	echo Please notify system personel.
    	exit ;;
esac

# Once the new environment system is committed we can delete these lines
# Export the PAGER environment variable for those who have it set
if [ "$PAGER" ] ; then
    export PAGER
fi

# Set some environment variables if they are not set
if [ ! "$GRASS_PAGER" ] ; then
    if [ -x /bin/more ] ; then
        GRASS_PAGER=more
    else 
        GRASS_PAGER=less
    fi
    export GRASS_PAGER
fi

if [ ! "$GRASS_TCLSH" ] ; then
    GRASS_TCLSH=tclsh
    export GRASS_TCLSH
fi

if [ ! "$GRASS_WISH" ] ; then
    GRASS_WISH=wish
    export GRASS_WISH
fi

if [ ! "$GRASS_HTML_BROWSER" ] ; then
    for i in `echo "$PATH" | sed 's/^:/.:/
                                s/::/:.:/g
                                s/:$/:./
                                s/:/ /g'`
    do
        if [ -f "$i/konqueror" ] ; then
            GRASS_HTML_BROWSER=konqueror
            break
        elif [ -f "$i/mozilla" ] ; then
            GRASS_HTML_BROWSER=mozilla
            break
        elif [ -f "$i/opera" ] ; then
            GRASS_HTML_BROWSER=opera
            break
        elif [ -f "$i/netscape" ] ; then
            GRASS_HTML_BROWSER=netscape
            break
        fi
    done
fi
if [ ! "$GRASS_HTML_BROWSER" ] ; then
    echo "Searching for web browser, but neither konqueror, nor mozilla, opera, netscape found."
    # so we set konqueror, though, to make lib/gis/parser.c happy:
    GRASS_HTML_BROWSER=konqueror
fi
export GRASS_HTML_BROWSER

if [ ! "$GRASS_GNUPLOT" ] ; then
    GRASS_GNUPLOT="gnuplot -persist"
    export GRASS_GNUPLOT
fi

# First time user - GISRC is defined in the grass script
if [ ! -f "$GISRC" ] ; then

    if [ ! -f "$GISBASE/locale/$LCL/etc/grass_intro" ] ; then
	cat "$ETC/grass_intro"
    else
	cat "$GISBASE/locale/$LCL/etc/grass_intro"
    fi

    echo
    echo "Hit RETURN to continue"
    read ans

    #for convenience, define pwd as GISDBASE:
    echo "GISDBASE: `pwd`" > "$GISRC"
    
    # This is a hack for not having a good initial gui - should be removed
    # with next version of initialization gui
    GRASS_GUI="text"
    
else
    echo "Cleaning up temporary files....."
    ("$ETC/clean_temp" > /dev/null &)
fi


# Force text startup: Until GUI is updated to GRASS 5.1
GRASS_GUI="text"

echo "Starting GRASS ..."

# Check if we are running X windows by checking the DISPLAY variable
if [ "$DISPLAY" ] ; then

    # Check if we need to find wish
    if [ "$GRASS_GUI" = "tcltk" ] ; then

	# Search for a wish program
	WISH=

	for i in `echo "$PATH" | sed 's/^:/.:/
    	    	    		    s/::/:.:/g
				    s/:$/:./
				    s/:/ /g'`
	do
	    if [ -f "$i/$GRASS_WISH" ] ; then
    		WISH=$GRASS_WISH
		break
	    fi
	done

	# Check if wish was found
	if [ "$WISH" ] ; then

	    # Set the tcltkgrass base directory
	    TCLTKGRASSBASE=$GISBASE/tcltkgrass
	    export TCLTKGRASSBASE
	else

	    # Wish was not found - switch to text interface mode
	    echo 
    	    echo "WARNING: The wish command ($GRASS_WISH) was not found!"
	    echo "Please check your GRASS_WISH environment variable."
	    echo "Use the -help option for details."
	    echo "Switching to text based interface mode."
	    echo
	    echo "Hit RETURN to continue."
	    read ans

	    GRASS_GUI="text"
	fi
    fi
else
    
    # Display a message if a graphical interface was expected
    if [ "$GRASS_GUI" != "text" ] ; then
        # Set the interface mode to text
    	echo
	echo "WARNING: It appears that the X Windows system is not active."
	echo "A graphical based user interface is not supported."
	echo "Switching to text based interface mode."
	echo
	echo "Hit RETURN to continue"
	read ans

        GRASS_GUI="text"
    fi
fi

# Save the user interface variable in the grassrc file - choose a temporary
# file name that should not match another file
if [ -f "$GISRC" ] ; then
    awk '$1 !~ /GRASS_GUI/ {print}' "$GISRC" > "$GISRC.$$"
    echo "GRASS_GUI: $GRASS_GUI" >> "$GISRC.$$"
    mv -f "$GISRC.$$" "$GISRC"
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
    	    L="$LOCATION"
    	fi
    else
    	L="$1"
    
    	if [ `echo "$L" | cut -c 1` != "/" ] ; then
    	    L="`pwd`/$L"
    	fi
    fi

    if [ "$L" ] ; then
    	MAPSET=`basename "$L"`
    	L=`dirname "$L"`
    
    	if [ "$L" != "." ] ; then
    	    LOCATION_NAME=`basename "$L"`
    	    L=`dirname "$L"`
    
    	    if [ "$L" != "." ] ; then
    	    	GISDBASE="$L"
    	    fi
    	fi
    fi

    #strip off white space from LOCATION_NAME and MAPSET: only supported for $GISDBASE
    MAPSET=`echo $MAPSET | sed 's+ ++g'`
    LOCATION_NAME=`echo $LOCATION_NAME | sed 's+ ++g'`

    if [ "$GISDBASE" -a "$LOCATION_NAME" -a "$MAPSET" ] ; then
    	LOCATION="$GISDBASE/$LOCATION_NAME/$MAPSET"
    
    	if [ ! -r "$LOCATION/WIND" ] ; then
    	    echo "$LOCATION: Not a valid GRASS location"
    	    exit
    	fi
    
    	if [ -s "$GISRC" ] ; then
    	    sed -e "s|^GISDBASE:.*$|GISDBASE: $GISDBASE|; \
    	    	s|^LOCATION_NAME:.*$|LOCATION_NAME: $LOCATION_NAME|; \
    	    	s|^MAPSET:.*$|MAPSET: $MAPSET|" "$GISRC" > "$GISRC.$$"
    
    	    if [ $? -eq 0 ] ; then
    	    	mv -f "$GISRC.$$" "$GISRC"
    	    else
    	    	rm -f "$GISRC.$$"
    	    	echo "Failed to create new $GISRC"
    	    	LOCATION=
    	    fi
    	else
    	    echo "GISDBASE: $GISDBASE" > "$GISRC"
    	    echo "LOCATION_NAME: $LOCATION_NAME" >> "$GISRC"
    	    echo "MAPSET: $MAPSET" >> "$GISRC"
    	fi
    else
    	echo "GISDBASE, LOCATION_NAME and MAPSET variables not set properly."
    	echo "Interactive startup needed."
    	exit
    fi
fi

# User selects LOCATION and MAPSET if not set
if [ ! "$LOCATION" ] ; then
    
    case "$GRASS_GUI" in
    	
	# Check for text interface
	text)
	    "$ETC/set_data"
    	    
	    case $? in
     	    	0) ;;
     	    	*) 
		    # Check for an invalid GISRC file
		    if [ -f "$GISRC" ] ; then
			VALUE=`grep "GISDBASE" "$GISRC"`
			if [ "$VALUE" = "" ] ; then
    			    echo "Invalid resource file, removing $GISRC"
			    rm -f "$GISRC"
			fi
		    fi
		    
		    exit
		    ;;
    	    esac
	    ;;
	
	# Check for tcltk interface
	tcltk)
	    eval `"$WISH" -file "$TCLTKGRASSBASE/script/gis_set.tcl"`
	    
	    case $? in
     	    	1)
		    # The gis_set.tcl script printed an error message so wait
		    # for user to read it
		    echo "Error in Tcl/Tk startup. If necessary, please"
		    echo "report this error to the Grass developers."
		    echo "Switching to text mode now."
		    echo "Hit RETURN to continue..."
		    read ans
		    
		    GRASS_GUI="text"

                    if [ -f "$GISRC" ] ; then
                        awk '$1 !~ /GRASS_GUI/ {print}' "$GISRC" > "$GISRC.$$"
                        echo "GRASS_GUI: $GRASS_GUI" >> "$GISRC.$$"
                        mv -f "$GISRC.$$" "$GISRC"
                    fi

		    "$ETC/set_data"

		    case $? in
     	    		0) ;;
     	    		*) 
			    # Check for an invalid GISRC file
			    if [ -f "$GISRC" ] ; then
				VALUE=`grep "GISDBASE" "$GISRC"`
				if [ "$VALUE" = "" ] ; then
    				    echo "Invalid resource file, removing $GISRC" 
				    rm -f "$GISRC"
				fi
			    fi

			    exit
			    ;;
    		    esac
		    ;;
	    
     	    	0)
		    # These checks should not be necessary with real init stuff
		    if [ "$LOCATION_NAME" = "##NONE##" ] ; then
    	    		"$ETC/set_data"
    	    		if [ $? != 0 ]; then
    	    		    echo "GISDBASE: $OLD_DB" > "$GISRC"
    	    		    echo "LOCATION_NAME: $OLD_LOC" >> "$GISRC"
    	    		    echo "MAPSET: $OLD_MAP" >> "$GISRC"
    	    		    exit
    	    		fi
    		    fi

		    if [ "$LOCATION_NAME" = "##ERROR##" ] ; then
    	    		echo "The selected location is not a valid GRASS location"
    	    		exit
		    fi

		    ;;
		    
		*)
		    echo "ERROR: Invalid return code from gis_set.tcl."
		    echo "Please advise GRASS developers of this error."
		    ;;
    	    esac
	    
	    ;;
	*)
	    # Shouldn't need this but you never know
	    echo "ERROR: Invalid user interface specified - $GRASS_GUI."
	    echo "Use the -help option to grass for valid interfaces."
	    exit
	    ;;
    esac
fi

eval `g.gisenv`
LOCATION=${GISDBASE?}/${LOCATION_NAME?}/${MAPSET?}

trap "" 2 3
CYGWIN=`uname | grep CYGWIN`

# cygwin has many problems with the shell setup
# below, so i hardcoded everything here.
if [ "$CYGWIN" ] ; then
    sh="cygwin"
    shellname="GNU Bash (Cygwin)"
    SHELL=/usr/bin/bash.exe
else 
    sh=`basename "$SHELL"`
    case "$sh" in
        ksh)  shellname="Korn Shell";;
        csh)  shellname="C Shell" ;;
        tcsh) shellname="TC Shell" ;;
        bash) shellname="Bash Shell" ;;
        sh)   shellname="Bourne Shell";;
        *)    shellname=shell;;
    esac
fi

# Start the chosen GUI but ignore text
case "$GRASS_GUI" in
    
    # Check for tcltk interface
    tcltk)
        "$GISBASE/bin/tcltkgrass" &
	;;
    
    # Ignore others
    *)
    	;;
esac

# Display the version and license info
tput clear

if [ -f "$GISBASE/locale/$LCL/etc/license" ] ; then
	cat "$GISBASE/locale/$LCL/etc/license"
else
	cat "$ETC/license"
fi
echo 
echo "This version running thru the $shellname ($SHELL)"
echo "Help is available with the command:      g.help"
echo "See the licence terms with:              g.version -c"

if [ "$GRASS_GUI" = "text" ] ; then
    echo "Start the graphical user interface with: d.m &"
else
    echo "If required, restart the graphical user interface with: d.m &"
fi

echo "When ready to quit enter:                exit"

case "$sh" in

csh|tcsh)
    USERHOME="$HOME"      # save original home
    HOME="$LOCATION"
    export HOME
    cshrc="$HOME/.cshrc"
    tcshrc="$HOME/.tcshrc"
    rm -f "$cshrc" "$tcshrc"
    echo "set home = $USERHOME" > "$cshrc"
    echo "set history = 500 savehist = 500  noclobber ignoreeof" >> "$cshrc"

    echo "set prompt = '\\" >> "$cshrc"
    echo "Mapset <${MAPSET}> in Location <${LOCATION_NAME}> \\" >> "$cshrc"
    echo "GRASS VERSION_NUMBER > '" >> "$cshrc"
    echo 'set BOGUS=``;unset BOGUS' >> "$cshrc"

    if [ -r "$USERHOME/.grass.cshrc" ]
    then
	cat "$USERHOME/.grass.cshrc" >> "$cshrc"
    fi

    if [ -r "$USERHOME/.cshrc" ]
    then
	grep '^ *set  *mail *= *' "$USERHOME/.cshrc" >> "$cshrc"
    fi

    if [ -r "$USERHOME/.tcshrc" ]
    then
	grep '^ *set  *mail *= *' "$USERHOME/.tcshrc" >> "$cshrc"
    fi

    if [ -r "$USERHOME/.login" ]
    then
	grep '^ *set  *mail *= *' "$USERHOME/.login" >> "$cshrc"
    fi

    echo "set path = ( $PATH ) " | sed 's/:/ /g' >> "$cshrc"

    cp "$cshrc" "$tcshrc"
    "$ETC/run" "$SHELL"

    HOME="$USERHOME"
    export HOME
    ;;

bash)
    USERHOME="$HOME"      # save original home
    HOME="$LOCATION"      # save .bashrc in $LOCATION
    export HOME
    bashrc="$HOME/.bashrc"
    rm -f "$bashrc"
    echo "test -z $PROFILEREAD && . /etc/profile" > "$bashrc"
    echo "test -r ~/.alias && . ~/.alias" >> "$bashrc"
    echo "umask 022" >> "$bashrc"
    echo "PS1='GRASS VERSION_NUMBER:\w > '" >> "$bashrc"

    if [ -r "$USERHOME/.grass.bashrc" ]
    then
        cat "$USERHOME/.grass.bashrc" >> "$bashrc"
    fi

    echo "export PATH=\"$PATH\"" >> "$bashrc"
    echo "export HOME=\"$USERHOME\"" >> "$bashrc" # restore user home path

    "$ETC/run" "$SHELL"
    HOME="$USERHOME"
    export HOME
    ;;

cygwin)
    USERHOME="$HOME"      # save original home
    HOME="$LOCATION"      # save .bashrc in $LOCATION
    export HOME
    bashrc="$HOME/.bashrc"
    rm -f "$bashrc"
    # this does not work on cygwin for unknown reasons
    # echo "test -z $PROFILEREAD && . /etc/profile" > "$bashrc"
    echo "test -r ~/.alias && . ~/.alias" >> "$bashrc"
    echo "umask 022" >> "$bashrc"
    echo "PS1='GRASS VERSION_NUMBER:\w > '" >> "$bashrc"

    if [ -r "$USERHOME/.grass.bashrc" ]
    then
        cat "$USERHOME/.grass.bashrc" >> "$bashrc"
    fi

    echo "export PATH=\"$PATH\"" >> "$bashrc"
    echo "export HOME=\"$USERHOME\"" >> "$bashrc" # restore user home path

    "$ETC/run" "$SHELL"
    HOME="$USERHOME"
    export HOME
    ;;

*)

PS1="
Mapset <$MAPSET> in Location <$LOCATION_NAME>
GRASS-GRID > "

    export PS1

    "$ETC/run" "$SHELL"
    ;;
esac

trap 2 3

# Grass session finished
tput clear
echo "Cleaning up temporary files....."

("$ETC/clean_temp" > /dev/null &)
rm -f "$lockfile"

echo "done"
echo 
echo 
echo 
echo "Goodbye from GRASS GIS"
echo
