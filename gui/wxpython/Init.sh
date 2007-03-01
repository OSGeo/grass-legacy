#!/bin/sh
#############################################################################
#
# $Id: init.sh,v 1.104 2007/02/12 06:31:54 hamish Exp $
#
# MODULE:   	GRASS Initialization
# AUTHOR(S):	Original author unknown - probably CERL
#               Andreas Lange - Germany - andreas.lange@rhein-main.de
#   	    	Huidae Cho - Korea - grass4u@gmail.com
#   	    	Justin Hickey - Thailand - jhickey@hpcc.nectec.or.th
#   	    	Markus Neteler - Germany/Italy - neteler@itc.it
# PURPOSE:  	The source file for this shell script is in
#   	    	src/general/init/init.sh. It sets up some environment
#   	    	variables and the lock file. It also parses any remaining
#   	    	command line options for setting the GISDBASE, LOCATION, and/or
#   	    	MAPSET. Finally it starts GRASS with the appropriate user
#   	    	interface and cleans up after it is finished.
# COPYRIGHT:    (C) 2000 by the GRASS Development Team
#
#               This program is free software under the GNU General Public
#   	    	License (>=v2). Read the file COPYING that comes with GRASS
#   	    	for details.
#
#############################################################################

trap "echo 'User break!' ; exit" 2 3 15

# Set the GRASS_PERL variable
GRASS_PERL=/usr/local/bin/perl
export GRASS_PERL

# GRASS_SH is normally just for Windows when not started from a bourne 
# shell. But when starting from Init.sh is still needed for Tcl/Tk.
GRASS_SH=/bin/sh
export GRASS_SH

# Set GRASS version number for R interface etc (must be an env_var for MS-Windows)
GRASS_VERSION="6.3.cvs"
export GRASS_VERSION

# Get the command name
CMD_NAME=grass63

# Get the system name
SYSTEM=`uname -s`
case $SYSTEM in
MINGW*)
	MINGW=1
	;;
CYGWIN*)
	CYGWIN=1
	;;
esac


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
	    echo "  $CMD_NAME [-h | -help | --help] [-text | -gui] [[[<GISDBASE>/]<LOCATION_NAME>/]<MAPSET>]"
	    echo
            echo "Flags:"
            echo "  -h or -help or --help          print this help message"
            echo "  -text                          use text based interface and set as default"
            echo "  -gui (or -tcltk)               use Tcl/Tk based graphical user interface"
            echo "                                   and set as default"
	    echo "  -oldgui                        use the old GUI interface and set as default"
            echo
            echo "Parameters:"
            echo "  GISDBASE                       initial database (path to GIS data)"
            echo "  LOCATION_NAME                  initial location"
            echo "  MAPSET                         initial mapset"
            echo
            echo "  GISDBASE/LOCATION_NAME/MAPSET  fully qualified initial mapset directory"
            echo
            echo "Environment variables relevant for startup:"
            echo "  GRASS_GUI                      select GUI (text, gis.m, d.m)" # wx
            echo "  GRASS_TCLSH                    set tclsh shell name to override 'tclsh'"
            echo "  GRASS_WISH                     set wish shell name to override 'wish'"
            echo "  GRASS_HTML_BROWSER             set html web browser for help pages"
            echo "  GRASS_ADDON_PATH               set additional path(s) to local GRASS modules"
            echo "  GRASS_BATCH_JOB                shell script to be processed as batch job"
            echo "  GRASS_PYTHON                   set python shell name to override 'python'"
	    exit
	    ;;
	
	# Check if the -text flag was given
	-text)
	    GRASS_GUI="text"
	    shift
	    ;;
	
	# Check if the -tcltk flag was given
	# change -gui to wxpython as needed
	-gui | -tcltk)
	    GRASS_GUI="tcltk"
	    shift
	    ;;

	# Check if the -oldgui flag was given
	-oldgui)
	    GRASS_GUI="d.m"
	    shift
	    ;;

	# Check if the -wx flag was given
	-wx)
	    GRASS_GUI="wx"
	    shift
	    ;;
    esac
done

# Set the GIS_LOCK variable to current process id
GIS_LOCK=$$
export GIS_LOCK

# Set the global grassrc file
GISRCRC="$HOME/.grassrc6"

# Set the session grassrc file
if [ "$MINGW" ] ; then
	PWD=`pwd -W`
	USER="$USERNAME"
	if [ ! "$USER" ] ; then
		USER="user_name"
	fi
	if [ ! -f "$GISBASE/etc/monitorcap" ] ; then
		# create an empty monitorcap
		touch "$GISBASE/etc/monitorcap"
	fi
else
	PWD=`pwd`
	USER="`whoami`"
fi

## use TMPDIR if it exists, otherwise /tmp
#tmp=${TMPDIR-/tmp}
#tmp="$tmp/grass6-$USER-$GIS_LOCK"
tmp=/tmp/grass6-$USER-$GIS_LOCK
(umask 077 && mkdir "$tmp") || {
    echo "Cannot create temporary directory! Exiting." 1>&2
    exit 1
}
GISRC="$tmp/gisrc"
export GISRC

# remove invalid GISRC file to avoid disturbing error messages:
cat "$GISRCRC" 2>/dev/null| grep UNKNOWN >/dev/null
if [ $? -eq 0 ] ; then
   rm -f "$GISRCRC"
fi

# Copy the global grassrc file to the session grassrc file
if [ -f "$GISRCRC" ] ; then
    cp "$GISRCRC" "$GISRC"
    if [ $? -eq 1 ] ; then
    	echo "Cannot copy '$GISRCRC' to '$GISRC'"
    	exit
    fi
fi

# Copy global grassrc file to session grassrc

# At this point the GRASS user interface variable has been set from the
# command line, been set from an external environment variable, or is 
# not set. So we check if it is not set
if [ ! "$GRASS_GUI" ] ; then

    # Check for a reference to the GRASS user interface in the grassrc file
    if [ -f "$GISRC" ] ; then
    	GRASS_GUI=`awk '/GRASS_GUI/ {print $2}' "$GISRC"`
    fi
    
    # Set the GRASS user interface to the default if needed - currently tcltk
    if [ ! "$GRASS_GUI" ] ; then
	GRASS_GUI="tcltk"
    fi
else
    if [ "$GRASS_GUI" = "gui" ] ; then
	# change to wxpython as needed
    	GRASS_GUI="tcltk"
    fi

fi

# Set PATH to GRASS bin, ETC to GRASS etc
ETC="$GISBASE/etc"

if [ "$LC_ALL" ] ; then
	LCL=`echo "$LC_ALL" | sed 's/\(..\)\(.*\)/\1/'`
elif [ "$LC_MESSAGES" ] ; then
	LCL=`echo "$LC_MESSAGES" | sed 's/\(..\)\(.*\)/\1/'`
else
	LCL=`echo "$LANG" | sed 's/\(..\)\(.*\)/\1/'`
fi

if [ -n "$GRASS_ADDON_PATH" ] ; then
   PATH="$GISBASE/bin:$GISBASE/scripts:$GRASS_ADDON_PATH:$PATH"
else
   PATH="$GISBASE/bin:$GISBASE/scripts:$PATH"
fi
export PATH

# Set LD_LIBRARY_PATH to find GRASS shared libraries
if [ ! "$LD_LIBRARY_PATH" ] ; then
  LD_LIBRARY_PATH="$GISBASE/lib"
else
  LD_LIBRARY_PATH="$GISBASE/lib:$LD_LIBRARY_PATH"
fi
export LD_LIBRARY_PATH
# Additional copy of variable to use with grass-run.sh
GRASS_LD_LIBRARY_PATH="$LD_LIBRARY_PATH"
export GRASS_LD_LIBRARY_PATH

# Set some environment variables if they are not set
if [ ! "$GRASS_PAGER" ] ; then
    if [ -x /bin/more ] || [ -x /usr/bin/more ] ; then
        GRASS_PAGER=more
    elif [ -x /bin/less ] || [ -x /usr/bin/less ] ; then
        GRASS_PAGER=less
    elif [ "$MINGW" ] ; then
        GRASS_PAGER=more
    else
        GRASS_PAGER=cat
    fi
    export GRASS_PAGER
fi


# Set up tcltk and wish environment 
# with options for aqua tcltk with Mac OSX


if [ ! "$GRASS_TCLSH" ] ; then
   if [ "$HOSTTYPE" = "macintosh" ] ; then
      if [ -d "/usr/local/grasslib" ]; then
            GRASS_TCLSH=/usr/local/grasslib/bin/tclsh
      else
         GRASS_TCLSH=tclsh
      fi
   else
      GRASS_TCLSH=tclsh
   fi
   export GRASS_TCLSH
fi   


#WISH_OS=`echo 'puts $tcl_platform(platform) ; exit 0' | wish`

if [ ! "$GRASS_WISH" ] ; then
   if [ "$HOSTTYPE" = "macintosh" ] ; then
      if [ "$osxaqua" ] ; then
         if [ -d "/usr/local/grasslib/bin/Grass.app" ] ; then
            GRASS_WISH=/usr/local/grasslib/bin/Grass.app/Contents/MacOS/Grass
         else
            GRASS_WISH=/usr/bin/wish
         fi
         export osxaqua
      elif [ -d "/usr/local/grasslib" ] ; then
         GRASS_WISH=/usr/local/grasslib/bin/wish
      else
         #force X11 tcl on Mac without grasslib directory:
         GRASS_WISH=/usr/bin/wish
      fi
   else
      GRASS_WISH=wish
   fi   
fi
export GRASS_WISH

if [ ! "$GRASS_PYTHON" ] ; then
      GRASS_PYTHON=python
fi
export GRASS_PYTHON


if [ ! "$GRASS_HTML_BROWSER" ] ; then
    for i in `echo "$PATH" | sed 's/^:/.:/
                                s/::/:.:/g
                                s/:$/:./
                                s/:/ /g'`
    do
        if [ -f "$i/htmlview" ] ; then  
            GRASS_HTML_BROWSER=htmlview  
            break  
        elif [ -f "$i/konqueror" ] ; then  
            GRASS_HTML_BROWSER=konqueror
            break
        elif [ -f "$i/mozilla" ] ; then
            GRASS_HTML_BROWSER=mozilla
            break
        elif [ -f "$i/mozilla-firefox" ] ; then
            GRASS_HTML_BROWSER=mozilla-firefox
            break
        elif [ -f "$i/opera" ] ; then
            GRASS_HTML_BROWSER=opera
            break
        elif [ -f "$i/netscape" ] ; then
            GRASS_HTML_BROWSER=netscape
            break
        elif [ "$HOSTTYPE" = "macintosh" -o "$HOSTTYPE" = "powermac" -o "$HOSTTYPE" = "powerpc" ] ; then
            GRASS_HTML_BROWSER=open
            break
        elif [ "$HOSTTYPE" = "arm" ] ; then
            GRASS_HTML_BROWSER=dillo
            break
        elif [ "$HOSTTYPE" = "arm" ] ; then
            GRASS_HTML_BROWSER=dillo2
            break
	elif [ "$MINGW" -o "$CYGWIN" ] ; then
	    iexplore="$SYSTEMDRIVE/Program Files/Internet Explorer/iexplore.exe"
	    if [ -f "$iexplore" ] ; then
		GRASS_HTML_BROWSER=$iexplore
	    else
		GRASS_HTML_BROWSER="iexplore"
	    fi
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

#predefine monitor size for certain architectures
if [ "$HOSTTYPE" = "arm" ] ; then
   #small monitor on ARM (iPAQ, zaurus... etc)
   GRASS_HEIGHT=320
   GRASS_WIDTH=240
   export GRASS_HEIGHT GRASS_WIDTH
fi


if [ ! "$GRASS_GNUPLOT" ] ; then
    GRASS_GNUPLOT="gnuplot -persist"
    export GRASS_GNUPLOT
fi

if [ ! "$GRASS_PROJSHARE" ] ; then
    GRASS_PROJSHARE=/usr/local/share/proj
    export GRASS_PROJSHARE
fi

# First time user - GISRC is defined in the GRASS script
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
    echo "GISDBASE: $PWD" > "$GISRC"
    echo 'LOCATION_NAME: <UNKNOWN>' >> "$GISRC"
    echo 'MAPSET: <UNKNOWN>' >> "$GISRC"

    # This is a hack for not having a good initial gui - should be removed
    # with next version of initialization gui
    #GRASS_GUI="text"
    
else
    echo "Cleaning up temporary files....."
    ("$ETC/clean_temp" > /dev/null &)
fi


echo "Starting GRASS ..."

# Check if we are running X windows by checking the DISPLAY variable
if [ "$DISPLAY" -o "$MINGW" ] ; then

    # Check if python is working properly
    if [ "$GRASS_GUI" = "wx" ]; then
        echo 'variable=True' | "$GRASS_PYTHON" >/dev/null 2>&1
    fi
    # Check if we need to find wish
    if [ "$GRASS_GUI" = "tcltk" ] || \
        [ "$GRASS_GUI" = "gis.m" ] || \
        [ "$GRASS_GUI" = "d.m" ] ; then

	# Check if wish is working properly
	echo 'exit 0' | "$GRASS_WISH" >/dev/null 2>&1
    fi

    # ok
    if [ "$?" = 0 ] ; then
        # Set the tcltkgrass base directory
        TCLTKGRASSBASE="$ETC"
        # Set the wxpython base directory
        WXPYTHONGRASSBASE="$ETC/wx"
    else

        # Wish was not found - switch to text interface mode
        echo 
        echo "WARNING: The wish command does not work as expected!"
        echo "Please check your GRASS_WISH environment variable."
        echo "Use the -help option for details."
        echo "Switching to text based interface mode."
        echo
        echo "Hit RETURN to continue."
        read ans

        GRASS_GUI="text"
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
    fi

    if [ "$L" ] ; then
	if [ "$L" = "." ] ; then
	    L=$PWD
	elif [ `echo "$L" | cut -c 1` != "/" ] ; then
    	    L="$PWD/$L"
    	fi

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
		if [ "$LOCATION_NAME" = "PERMANENT" ] ; then
		   echo "$LOCATION: Not a valid GRASS location"
		   exit 1
		else
		   # the user wants to create mapset on the fly
		   if [ ! -f "$GISDBASE/$LOCATION_NAME/PERMANENT/WIND" ] ; then
			echo "The LOCATION \"$LOCATION_NAME\" does not exist. Please create first"
			exit 1
		   else
			mkdir -p "$LOCATION"
			cp "$GISDBASE/$LOCATION_NAME/PERMANENT/WIND" "$LOCATION/WIND"
			echo "Missing WIND file fixed"
		   fi
		fi
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
	tcltk | gis.m | d.m | wx)

            if [ "$GRASS_GUI" = "tcltk" ] || \
                [ "$GRASS_GUI" = "gis.m" ] || \
                [ "$GRASS_GUI" = "d.m" ] ; then

                #"$GRASS_WISH" -file "$TCLTKGRASSBASE/gis_set.tcl"
                #exit
                eval `"$GRASS_WISH" -file "$TCLTKGRASSBASE/gis_set.tcl"`
                thetest=$?
	        #0: failure
	        #1: successfully read environment
        
		###########################################################################
		# at the moment when the EPSG facility is used, the eval returns "1"
		# whereas everything should be ok, therefore til that problem is not solved
		# the return is forced to be ok ("0")
                if [ "$EPSGSCRIPT" = "yes" ] ; then
                    thetest=0
                fi
		############################################################################
            else
                #"$GRASS_PYTHON" "$WXPYTHONGRASSBASE/gis_set.py"
                #exit
                eval `"$GRASS_PYTHON" "$WXPYTHONGRASSBASE/gis_set.py"`
                thetest=$?
            fi
            

	    case $thetest in
     	    	1)

         	    # The gis_set.tcl script printed an error message so wait
		    # for user to read it
		    echo "Error in Tcl/Tk startup. If necessary, please"
		    echo "report this error to the GRASS developers."
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
	    echo "Use the -help option to select a valid interface."
	    exit
	    ;;
    esac
fi

GISDBASE=`g.gisenv GISDBASE`
LOCATION_NAME=`g.gisenv LOCATION_NAME`
MAPSET=`g.gisenv MAPSET`

LOCATION="${GISDBASE?}/${LOCATION_NAME?}/${MAPSET?}"

# Check for concurrent use
lockfile="$LOCATION/.gislock"
"$ETC/lock" "$lockfile" $$
case $? in
    0) ;;
    1)
    	echo "$USER is currently running GRASS in selected mapset (file $lockfile found). Concurrent use not allowed."
    	rm -rf "$tmp"  # remove session files from tmpdir
    	exit 1 ;;
    *)
    	echo Unable to properly access "$lockfile"
    	echo Please notify system personel.
    	exit 1 ;;
esac

trap "" 2 3 15

# cygwin has many problems with the shell setup
# below, so i hardcoded everything here.
if [ "$CYGWIN" ] ; then
    sh="cygwin"
    shellname="GNU Bash (Cygwin)"
    export SHELL=/usr/bin/bash.exe
    export OSTYPE=cygwin
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

# check for SHELL
if [ ! -x "$SHELL" ] ; then
    echo "ERROR: The SHELL variable is not set" 1>&2
    rm -f "$lockfile"
    rm -rf "$tmp"  # remove session files from tmpdir
    exit 1
fi

# hack to process batch jobs:
if [ -n "$GRASS_BATCH_JOB" ] ; then
   # defined, but ...
   if [ ! -f "$GRASS_BATCH_JOB" ] ; then
      # wrong file
      echo "Job file '$GRASS_BATCH_JOB' has been defined in the 'GRASS_BATCH_JOB' variable but not found. Exiting."
      echo "Use 'unset GRASS_BATCH_JOB' to disable batch job processing."
      exit 1
   else
      # right file, but ...
      if [ ! -x "$GRASS_BATCH_JOB" ] ; then
         echo "Please change file permission to 'executable' for '$GRASS_BATCH_JOB'"
         exit 1
      else
         echo "Executing '$GRASS_BATCH_JOB' ..."
         GRASS_GUI="text"
         SHELL="$GRASS_BATCH_JOB"
      fi
   fi
fi

# Start the chosen GUI but ignore text
case "$GRASS_GUI" in
    
    # Check for tcltk interface
    tcltk | gis.m | wx)
	if [ "$osxaqua" ] ; then
		"$GISBASE/scripts/gis.m" | sh &
	else
		"$GISBASE/scripts/gis.m"
	fi	
	;;
    d.m)
	if [ "$osxaqua" ] ; then
		"$GISBASE/scripts/d.m" | sh &
	else
		"$GISBASE/scripts/d.m"
	fi	
	;;

    wx)
        # comming soon, see ^
	echo "TODO: wxPython GUI" 1>&2
	;;
    
    # Ignore others
    *)
    	;;
esac

# Display the version and license info
if [ "$MINGW" ] ; then
	:
# TODO: uncomment when PDCurses works.
#	cls
else
	if [ -z "$GRASS_BATCH_JOB" ] ; then
	   tput clear
	fi
fi

if [ -f "$GISBASE/locale/$LCL/etc/welcome" ] ; then
	cat "$GISBASE/locale/$LCL/etc/welcome"
else
	cat "$ETC/welcome"
fi

echo "GRASS homepage:                          http://grass.itc.it/"
echo "This version running thru:               $shellname ($SHELL)"
echo "Help is available with the command:      g.manual -i"
echo "See the licence terms with:              g.version -c"

if [ "$GRASS_GUI" = "text" ] ; then
    echo "Start the graphical user interface with: gis.m"
else
    echo "If required, restart the graphical user interface with: gis.m"
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
    echo "set histfile = $HOME/.history" >> "$cshrc"

    echo "set prompt = '\\" >> "$cshrc"
    echo "Mapset <${MAPSET}> in Location <${LOCATION_NAME}> \\" >> "$cshrc"
    echo "GRASS 6.3.cvs > '" >> "$cshrc"
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
    EXIT_VAL=$?

    HOME="$USERHOME"
    export HOME
    ;;

bash|msh)
    USERHOME="$HOME"      # save original home
    HOME="$LOCATION"      # save .bashrc in $LOCATION
    export HOME
    bashrc="$HOME/.bashrc"
    rm -f "$bashrc"
    echo "test -z $PROFILEREAD && . /etc/profile" > "$bashrc"
    echo "test -r ~/.alias && . ~/.alias" >> "$bashrc"
    echo "umask 022" >> "$bashrc"
    echo "PS1='GRASS 6.3.cvs ($LOCATION_NAME):\w > '" >> "$bashrc"
    echo "PROMPT_COMMAND=$GISBASE/etc/prompt.sh" >> "$bashrc"
    
    if [ -r "$USERHOME/.grass.bashrc" ]
    then
        cat "$USERHOME/.grass.bashrc" >> "$bashrc"
    fi

    echo "export PATH=\"$PATH\"" >> "$bashrc"
    echo "export HOME=\"$USERHOME\"" >> "$bashrc" # restore user home path

    "$ETC/run" "$SHELL"
    EXIT_VAL=$?

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
    echo "PS1='GRASS 6.3.cvs ($LOCATION_NAME):\w > '" >> "$bashrc"

    if [ -r "$USERHOME/.grass.bashrc" ]
    then
        cat "$USERHOME/.grass.bashrc" >> "$bashrc"
    fi

    echo "export PATH=\"$PATH\"" >> "$bashrc"
    echo "export HOME=\"$USERHOME\"" >> "$bashrc" # restore user home path

    "$ETC/run" "$SHELL"
    EXIT_VAL=$?

    HOME="$USERHOME"
    export HOME
    ;;

*)

    PS1="GRASS $GRASS_VERSION ($LOCATION_NAME):\w > "
    export PS1

    if [ "$MINGW" ] ; then
	# "$ETC/run" doesn't work at all???
        "$SHELL"
	rm -rf "$LOCATION/.tmp"/*  # remove gis.m session files from .tmp
    else
    	"$ETC/run" "$SHELL"
	EXIT_VAL=$?
    fi
    ;;
esac

trap 2 3 15

# GRASS session finished
if [ "$MINGW" ] ; then
	:
# TODO: uncomment when PDCurses works.
#	cls
else
	if [ -z "$GRASS_BATCH_JOB" ] ; then
	   tput clear
	fi
fi

echo "Closing monitors....."
for MON  in `d.mon -L | grep running | grep -v "not running" | sed 's/ .*//'`
do 
    echo d.mon stop=$MON
    d.mon stop=$MON 
done

echo "Cleaning up temporary files....."

"$ETC/clean_temp" > /dev/null
rm -f "$lockfile"

# Save GISRC
cp "$GISRC" "$GISRCRC"
rm -rf "$tmp"  # remove session files from tmpdir

echo "done"
echo 
echo 
echo 
echo "Goodbye from GRASS GIS"
echo
if [ -x "$GRASS_BATCH_JOB" ] ; then
   echo "Batch job '$GRASS_BATCH_JOB' (defined in GRASS_BATCH_JOB variable) was executed."
   exit $EXIT_VAL
fi

