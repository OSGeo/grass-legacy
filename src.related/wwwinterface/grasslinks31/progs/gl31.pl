#!/usr/local/bin/perl
################################################################
# gl.pl
# GRASSLinks                           	Version 3.1.1
# Created by Susan M. Huse, 1994        Last Modified 05/06/98 
# tested / runs under perl 4.0, 5.002
#################################################################
# GRASSLinks COPYRIGHT NOTICE: Copyright (c) 1994-98
# Susan M. Huse and The Regents of the University of California
# All rights reserved.
#
# Permission to use, copy, modify, and distribute this software and 
# its documentation for educational, research and non-profit purposes, 
# without fee, and without a written agreement is hereby granted, 
# provided that the above copyright notice, this paragraph and the 
# following three paragraphs appear in all copies. 
#
# Permission to incorporate this software into commercial products may 
# be obtained by contacting the University of California.
#
# This software program and documentation are copyrighted by 
# Susan M. Huse and The Regents of the University of California. 
# The software program and documentation are supplied "as is", 
# without any accompanying services  from Susan M. Huse or 
# The Regents. Susan M. Huse and The Regents do not warrant 
# that the operation of the program will be uninterrupted 
# or error-free. The end-user understands that the program was 
# developed for research purposes and is advised not 
# to rely exclusively on the program for any reason. 
#
# IN NO EVENT SHALL SUSAN M. HUSE OR THE UNIVERSITY OF CALIFORNIA 
# BE LIABLE TO ANY PARTY FOR DIRECT, INDIRECT, SPECIAL, 
# INCIDENTAL, OR CONSEQUENTIAL DAMAGES, INCLUDING LOST PROFITS, 
# ARISING OUT OF THE USE OF THIS SOFTWARE AND ITS
# DOCUMENTATION, EVEN IF THE UNIVERSITY OF CALIFORNIA HAS BEEN
# ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. THE UNIVERSITY OF
# CALIFORNIA SPECIFICALLY DISCLAIMS ANY WARRANTIES, INCLUDING, BUT NOT
# LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
# FOR A PARTICULAR PURPOSE. THE SOFTWARE PROVIDED HEREUNDER IS ON AN
# "AS IS" BASIS, AND THE UNIVERSITY OF CALIFORNIA HAS NO OBLIGATIONS TO
# PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR MODIFICATIONS. 
#################################################################

#################################################################
# ERROR LOGGING note:
# GL errors written to STDERR which is the web server error
# log.  You could set up a temp file and write debug messages
# to it if needed.
#################################################################

require "flush.pl"; #flush buffer/forces print statements to STDERR & STDOUT

open(SAVEOUT, ">&STDOUT");
select(STDOUT); $|=1;

&set_glvars;		# set GL web variables
&parse_vars; 		# read input variables from the client form
&read_gldata; 		# read in GL data file info
&set_gloptions;

# GRASSLINKS ACCESS LOG FILE
open(ACCESSLOG,">>$GL_ACCESS_LOG") if defined $GL_ACCESS_LOG;

&start_GRASS;		# start grass preamble


# process and/or exit
if ($pid=fork) {
	wait();
	close(STDOUT);
	open(STDOUT,">&SAVEOUT");
    	$SIG{'ALRM'}='IGNORE';
    	&stop_GRASS;
	close(ACCESSLOG) if defined $GL_ACCESS_LOG;

	if ( -e $error_tmp ) {
		system("cat $error_tmp");
	}
	else {
		system("cat $html_tmp");
	}	
} else {
	setsid;
	$SIG{'ALRM'}= 'roundup';
	alarm($TIME_OUT);

	open (STDOUT, ">$html_tmp");
	# run the appropriate subroutine
	eval "&$value{ANALYSIS}"; 

} 
exit 0;

#################################################################
# End of main GRASSLinks routine				#
# everything from here down is subroutines			#
#################################################################

#################################################################
# ERROR CHECKING SUBROUTINES & BASIC UTILITIES			#	
#################################################################

#-------------------------------------------------------------------------
# NAME: roundup	
# 	This routine should exit gracefully in the event of bad
# 	process or timeout triggered.
#	Kills child processes that might linger (I hope).
#-------------------------------------------------------------------------
sub roundup {

    	$SIG{'ALRM'}='IGNORE';

	if ( defined $GL_ACCESS_LOG) {
		print ACCESSLOG "GL ROUNDUP: Timeout alarm triggered in $value{ANALYSIS}, $TIME_OUT seconds\n";
	} else {
		print STDERR "GL ROUNDUP: Timeout alarm triggered in $value{ANALYSIS}, $TIME_OUT seconds\n";
	}

        &error("TIMEOUT ERROR: command ran for $TIME_OUT seconds and will be killed");
	kill HUP => -$$;
}

#-------------------------------------------------------------------------
# NAME: system_errors_logged	
#	System call wrapper. STDERR sent to web server log file.
#-------------------------------------------------------------------------
sub system_errors_logged {
        system( "$_[0]" );
}

#-------------------------------------------------------------------------
# NAME: system_silent
#	System call wrapper; squelches any noise.
#-------------------------------------------------------------------------
sub system_silent {
        system( "$_[0] > /dev/null 2>&1  " );
}

#-------------------------------------------------------------------------
# NAME: error
# 	Creates error / warning html to predefined $error_tmp file
#	which is returned to user when program exits. 
# ARGS: $_[0] = Error message string.
#-------------------------------------------------------------------------
sub error {	
	my $errorMsg = $_[0];
	open(OUTPUT, ">$error_tmp") || print STDERR "GL can't open $error_tmp to create error html file\n";
        print OUTPUT "Content-type: text/html\n\n";
	print OUTPUT << "ENDPRINT";
        <HTML>
        <HEAD>
        <TITLE>GRASSLinks Error</TITLE>
        </HEAD>
        <BODY>
	<h2>GRASSLINKS Error:</h2>
	<hr>
	<p>
	<h3>$errorMsg </h3>
	<p><hr><p>
	<b>Back up your Web browser, review your selection(s) and try again!</b>
	</BODY>
	</HTML>
ENDPRINT
	close(OUTPUT);
	&flush(STDERR);
}

#-------------------------------------------------------------------------
# NAME: print_file	
#	Print out contents of file into html file.
# ARGS: $_[0] = The name of the file to print out.
#-------------------------------------------------------------------------
sub print_file {
	my $thefile = $_[0];

	if ( -e $thefile ) {
		open (THE_FILE,"$thefile") || print STDERR "In GL print_file subroutine and can't open file .$thefile.\n";
		while (<THE_FILE>) {
			print $_;
		}
		close (THE_FILE);
		&flush(STDERR);
	}
	else {
		print STDERR "In GL print_file, can't find file .$thefile.\n";
	}
}


#################################################################
# Variable Assignment Subroutines				#
#################################################################

#-------------------------------------------------------------------------
# NAME:	parse_vars
#	This subroutine reads in variables in either POST or GET
# 	method, checks input values, and then processes selected
#	analysis option.
# 	SECURITY ESSENTIAL - make no changes unless you are sure
# 	how they may effect security!
# REQUIRES: A valid http GET or POST input stream that contains
# 	at least a valid value for $value{PROJECT} and $value{ANALYSIS}
#	(validity checked in this subroutine).
# RETURNS: Input stream read into Hash of variables $value{}
#	and into Hash of Arrays $Values{}.
#-------------------------------------------------------------------------
sub parse_vars { 

	my ($in, @input, $input, $name, $val, $goodval); 
	# read in input stream	
	# eventually replace this with cgi-lib.pl
	if ($ENV{REQUEST_METHOD} eq 'POST') {
		read(STDIN,$input,$ENV{'CONTENT_LENGTH'});
		$input =~ s/%(..)/pack("C",hex($1))/eg;
		@input = split(/&/, $input);
		#UNCOMMENT FOR DEBUGGING
		#foreach $i (@input) { print STDERR "POST input: $i \n";}
	} elsif ($ENV{REQUEST_METHOD} eq 'GET') {
		$in = $ENV{'QUERY_STRING'};
		$in =~ s/%(..)/pack("C",hex($1))/eg;
		@input = split(/\+/,$in);	
		#UNCOMMENT FOR DEBUGGING
		#foreach $i (@input) { print STDERR "GET input: $i \n";}
	} else {
		&error("Invalid input, exiting!");
		return;
	}

	foreach $i(@input) { #create assoc. array to hold forms variables
		($name, $val) =  split (/=/, $i);
		if ( length($val) > 80 ) {
			&error( "Form input variable $name longer than 80 characters; exiting.");
			return;
		}
		#remove all potentially unsafe characters
		$val =~ tr/ /_/; #no spaces
		$val =~ tr/+/_/; #no pluses
		$val =~ s/[^a-zA-Z0-9_.,@-]//g; #anything but these strip
		#reassign val to goodval to be extra safe
		$goodval = $val;

		if ( $goodval ) {
	    		$value{$name} = $goodval;
			# Values{} holds multiple values- HashOfArrays 
			push (@{ $Values{$name} }, $goodval); 
		}

	} #end of foreach to process name-value associative array

	# For security reasons, make sure the value of $value{ANALYSIS}, which
	# launches the main GL routine, is a valid one as set in the
	# subroutine set_glvars

	unless ($GL_ANALYSES{ $value{ANALYSIS} } ) {
		&error("Analysis value \"$value{ANALYSIS}\" not recognized.");
		return;
	}
	#  Same security chec for valid project codes
	unless ($GL_PROJECTS{ $value{PROJECT} } ) {
		&error("Project value \"$value{PROJECT}\" not recognized.");
		return;
	}

	&flush(STDERR);
	
} #end of sub parse_vars

#-------------------------------------------------------------------------
# NAME: set_glvars
# 	Sets variables needed to run GL on this web server set.  
# REQUIRES: Must be configured for local site.
#-------------------------------------------------------------------------
sub set_glvars { 

        # GL dies if any process takes more than $TIME_OUT seconds
        $TIME_OUT = 240;

        # Directory for the many temp files created by GL;
        # should have cron job clean it out every hour or so
        $TMP = "/usr/local/httpd/htdocs/glinks/tmp";
	# The HTML_TMP is a link from the htdocs directory to the
	# temp directory so that tmp gif images can be viewed in GLinks
	$HTML_TMP = "/glinks/tmp";

        # These two temp files are the html files created during process
        # of this script, one of which will be returned to user
        $html_tmp="$TMP/$$.out";
        $error_tmp="$TMP/$$.error";

        # Web server running GL 
        $GL_HOST = "http://www.somewhere.edu";
        # HTML htdocs directory for GL files
        $HTML_PATH = "/glinks";
        $HTML_FULLPATH = "/usr/local/httpd/htdocs/glinks";
        # The name of this perl script!
        $GL_ACTION = "/cgi-bin/glinks/gl31.pl";
        # File listing info on all GL data files
        $GLDATA_LIST = "/usr/local/etc/httpd/cgi-bin/glinks/GL_list";
        # GL main url or path to grasslinks index.html 
        $GL_URL = "$HTML_PATH";
        # GL Help file
        $GL_HELP = "$HTML_PATH/help.html";
        # HTML File listing info on all GL data files
        $GL_MATRIX = "GL_matrix.html";
	# GL contact person for email
	$GL_MAILTO = "mailto:";

        # Home directory of user under which web server runs
        $WEBHOME = "/usr/home/web";
	# Grasslinks access log file *should NOT be world readable*
	$GL_ACCESS_LOG = "$WEBHOME/glaccess.log";

        # MAX Number of simultaneous GLinks sessions
        $N_GL_USERS = 5;

        # path for some helper programs
        #$PBM_PATH = "$HTML_FULLPATH/progs";
        $PBM_PATH = "/usr/local/pbmplus/bin";
        $ENV{PROG_PATH}="$HTML_FULLPATH/progs";
        $PROG_PATH = $ENV{PROG_PATH};
	
	# Stuff for gif image legends
	############################################
	# the dummy grass raster file used to create legend gif boxes
	$LEGEND_RASTER="thumbnail\@grasslinks0";
	# the color table for the dummy raster used for gif boxes
	$LEGEND_COLR_TABLE="/grass/data/sfbay/grasslinks0/colr/thumbnail";
	# The html directory that will contain the legend gifs
	$LGIF_PATH = "$HTML_PATH/legendGifs";
	# The full path of the directory containing the legend gifs
	$LGIF_FULLPATH = "$HTML_FULLPATH/legendGifs";
	# maximum number of legend categories to display
	$MAX_LEGEND_CATS = 36;
	# maximum number of legend table columns
	$MAX_LEGEND_COLS = 2;
	# size of legend gifbox in pixels
	$LEGEND_GIF_PSIZE = 10;
	############################################

	# set all valid GL operations for this script
	%GL_ANALYSES = ("display1", "ok", "display2", "ok", "display3", "ok", "report1", "ok", "report1a", "ok", "report2", "ok", "report3", "ok", "reclass1", "ok", "reclass2", "ok", "reclass3", "ok", "buffer1", "ok", "buffer2", "ok", "buffer3", "ok", "combine1", "ok", "combine2", "ok", "combine3", "ok");

	# set all valid GL projects for this script
	%GL_PROJECTS = ("r", "ok", "d", "ok", "b", "ok", "s", "ok", "g", "ok", "f", "ok", "t", "ok"); 

	# GL Header - set header for all GL html files
	# used in the subroutine begin_html
	$GL_HEADER = "GRASSLinks 3.1 at REGIS";	
}

#-------------------------------------------------------------------------
# NAME: set_GRASSvars
# 	Sets variables specific to GRASS as it runs under GL.
# REQUIRES: Must be configured for local site.
#-------------------------------------------------------------------------
sub set_GRASSvars { 
	$ENV{GISBASE} = "/grass/src/grass4.1.5_fp/axp";
        $ENV{GISDBASE} = "/grass/data";
        $ENV{LOCATION_NAME} = "sfbay";
        $ENV{ETC} = "$ENV{$GISBASE}/etc";
        $ENV{PATH} = "$ENV{GISBASE}/bin:$ENV{GISBASE}/scripts:$ENV{GISBASE}/garden/bin:$PATH:/usr/bin";

        $GISBASE=$ENV{GISBASE};
        $GISDBASE = $ENV{GISDBASE};
        $LOCATION_NAME = $ENV{LOCATION_NAME};
        $ETC = $ENV{ETC};
        $PATH = "$ENV{PATH}";

	$DEFAULT_REGION = $project{region}; #project dependent
	$DEFAULT_COLOR = "black"; # for vector and site maps
	$DEFAULT_GIFSIZE = 400; # for vector and site maps
}

#-------------------------------------------------------------------------
# NAME: read_gldata 
#	Reads in from file grass data files available in GL
# 	depending on a project variable.
# REQUIRES: The project variable $value{PROJECT} must be set
#	and a file must exist that lists available GRASS raster,
#	vector, sites, and region files, as well as specifics
# 	about the current GL project. 
# NOTE: This subroutine expects each GRASS file listed in the $GLDATA_LIST
#	file to be of the following format: 1) fields are delimited by the
#	"|" character; 2) field 0 always contains the file type: raster,
#	vector, sites, region, and project - which is not a grass file type! 
#	3) each file has info listed about it in a determined order as 
#	indicated in this subroutine in the SWITCH block
#-------------------------------------------------------------------------
sub read_gldata {

	my (@thefile, @data, $line); 

        open(LIST, "$GLDATA_LIST") || print STDERR "GL unable to open $GLDATA_LIST in subroutine read_gldata\n"; 

	@thefile = <LIST>;
	close(LIST);

        foreach $line (@thefile) {
	   chop $line;
	   # load into temp array @data
	   @data = split(/\|/,$line);
	   for ($i = 0; $i < 4; $i++){
		$data[$i] = "none" unless defined $data[$i];
	   }

	   # only grab files for current project
	   next unless $data[1] =~ /$value{PROJECT}/ || $data[1] =~ /\!/;

	   SWITCH: {
		if ($line =~ /^raster\|/) {
			$rec = {};
			$rec->{projects} = $data[1];
			$rec->{filename} = $data[2];
			$rec->{descShort} = $data[3];
			# the following 3 fields aren't used in
			# this script but they could be. They
			# are used in helper GL scripts
			#$rec->{mdata} = $data[4];
			#$rec->{gif} = $data[5];
			#$rec->{descLong} = $data[6];
			#whole thing in HoH
			$RASTERS{ $rec->{filename} } = $rec;
			#whole thing in HoA
			push @{ $GLfiles{raster} }, [ @data ];
			last SWITCH;
                } 
		if ($line =~ /^vector\|/) {
			$rec = {};
			$rec->{projects} = $data[1];
			$rec->{filename} = $data[2];
			$rec->{descShort} = $data[3];
			#whole thing into HoH
			$VECTORS{ $rec->{filename} } = $rec;
			#whole thing in HoA
			push @{ $GLfiles{vector} }, [ @data ];
			last SWITCH;
                } 
		if ($line =~/^sites\|/) {
			$rec = {};
			$rec->{projects} = $data[1];
			$rec->{filename} = $data[2];
			$rec->{descShort} = $data[3];
			#whole thing
			$SITES{ $rec->{filename} } = $rec;
			#whole thing in HoA
			push @{ $GLfiles{sites} }, [ @data ];
			last SWITCH;
                } 
		if ($line =~/^region\|/) {
			$rec = {};
			$rec->{filename} = $data[2];
			$rec->{descShort} = $data[3];

			#whole thing
			$REGIONS{ $rec->{filename} } = $rec;
			#whole thing in HoA
			push @{ $GLfiles{region} }, [ @data ];
			last SWITCH;
		}
		if ($line =~/^project\|/) {
			# only one project per GL session
			$project{title} = $data[2];
			$project{gif} = $data[3];
			$project{region} = $data[4];
			last SWITCH;
		}
	   } # end SWITCH BLOCK

        }# end foreach 

} # end of  subroutine

#-------------------------------------------------------------------------
# NAME: set_gloptions 
#	Indicates list of values for specific GL options.
# 	If you want to indicate a default select value, then
#	you must indicate by placing a "!!" in field [1].
#-------------------------------------------------------------------------
sub set_gloptions {

	my (@color, @gifsize, @legend, @repUnits, @bufUnits, @resValues);

	@color = (
		[ "color","!!","black","black" ],
		[ "color","!","white","white" ],
		[ "color","!","red","red" ],
		[ "color","!","orange","orange" ],
		[ "color","!","yellow","yellow" ],
		[ "color","!","green","green" ],
		[ "color","!","blue","blue" ],
		[ "color","!","indigo","indigo" ],
		[ "color","!","brown","brown" ],
		[ "color","!","magenta","magenta" ],
		[ "color","!","aqua","aqua" ],
		[ "color","!","grey","grey" ],
	);
	$GLfiles{color} = [ @color ];

	@gifsize = (
		[ "gifsize","!","200","small (200X200 -- est. processing time 10 seconds)" ],
		[ "gifsize","!!","400","medium (400X400 -- est. processing time 30 seconds)" ],
		[ "gifsize","!","800","large (800X800 -- est. processing time 60 seconds)"],
		[ "gifsize","!","150","thumbnail (150X150 -- est. processing time 10 seconds)" ]
	);
	$GLfiles{gifsize} = [ @gifsize ];

	@legend = (
		[ "legend","!","yes","Create a Legend for the Map (increases processing time" ],
		[ "legend","!!","no","Do Not Create a Legend for the Map"],
	);
	$GLfiles{legend} = [ @legend ];

	@repUnits = (
		[ "report_units","!","mi","square miles" ],
		[ "report_units","!","me","square meters" ],
		[ "report_units","!","a","acres" ],
		[ "report_units","!","h","hectares" ],
		[ "report_units","!!","k","square kilometers" ],
	);
	$GLfiles{repUnits} = [ @repUnits ];

	@bufUnits = (
		[ "buff_units","!!","kilometers","kilometers" ],
		[ "buff_units","!","meters","meters" ],
		[ "buff_units","!","feet","feet" ],
		[ "buff_units","!","miles","miles" ],
	);
	$GLfiles{bufUnits} = [ @bufUnits ];

	@resValues = (
		[ "resolution","!","5","coarse" ],
		[ "resolution","!!","3","medium" ],
		[ "resolution","!","1","fine" ],
	);
	$GLfiles{resValues} = [ @resValues ];

} # end of set_gloptions  subroutine


#################################################################
# GRASS base subroutines					#
#################################################################

#-------------------------------------------------------------------------
# NAME: lock_Gmapset 
# 	Steps through all grasslinks mapsets until find an available one.
#	A mapset is available if there exists in the mapset the file
#	UNLOCK.  The mapset is unavailable if the file is LOCK.
# REQUIRES: This subroutine assumes that foreach number $N_GL_USERS
#	there exists a grass mapset directory in the name form:
#	$ENV{GISDBASE}/$ENV{LOCATION_NAME}/grasslinks$locnumber
#	and that only $N_GL_USERS can run grasslinks simultaneously
#	the UNLOCK file must be manually setup in mapset when GL first setup
#	and the LOCK files may need to be renamed UNLOCK if GL bombs 
# RETURNS: A mapset locked for user. Must be unlocked by the
#	subroutine stop_GRASS when user exits GL.
#-------------------------------------------------------------------------
sub lock_Gmapset {

	my $location = "$ENV{GISDBASE}/$ENV{LOCATION_NAME}/grasslinks";
	my $locnumber = 1;

	while ( $locnumber <= $N_GL_USERS ) {
		if (rename("$location$locnumber/UNLOCK", "$location$locnumber/LOCK")) {
			#the mapset is available and locked
			$ENV{MAPSET} = "grasslinks$locnumber";
			$ENV{LOCATION} = "$ENV{GISDBASE}/$ENV{LOCATION_NAME}/$ENV{MAPSET}";
			$ENV{GISRC} = "$WEBHOME/.grassrc$locnumber";
			$ENV{MAPLP} = "$TMP/pmap.$ENV{MAPSET}";
			$MAPSET = $ENV{MAPSET}; 
			$LOCATION = "$ENV{GISDBASE}/$ENV{LOCATION_NAME}/$MAPSET";
			$GISRC = $ENV{GISRC}; 
			$MAPLP= $ENV{MAPLP};
			
			# Update Access Log
			if ( defined $GL_ACCESS_LOG) {
				$theTime = `date`;
				chomp($theTime);
				#print ACCESSLOG "GL STARTED ($value{ANALYSIS}) at .$theTime. by: .$ENV{REMOTE_USER}.$ENV{REMOTE_ADDR}.$ENV{REMOTE_HOST}.$ENV{REMOTE_IDENT} in mapset .$MAPSET.\n"; 
				print STDERR "GL STARTED ($value{ANALYSIS}) at .$theTime. by: .$ENV{REMOTE_USER}.$ENV{REMOTE_ADDR}.$ENV{REMOTE_HOST}.$ENV{REMOTE_IDENT} in mapset .$MAPSET.\n"; 
				&flush(ACESSLOG);
			}
			last; 

		} else {
			#that mapset is not available, move to the next
			$locnumber = $locnumber + 1;
			if ($locnumber > $N_GL_USERS) {
				&error("All grasslinks mapsets are busy.  Try again later, or press the back button and retry now.\n");
				return;
			}
		}
	} #end of while loop

	&flush(STDERR);
}

#-------------------------------------------------------------------------
# NAME: start_GRASS 
# 	Sets necessary GRASS variables, locks a grass mapset, and
#	removes any set MASK.	
#-------------------------------------------------------------------------
sub start_GRASS { 

	&set_GRASSvars; #set grass specific variables
	&lock_Gmapset; #lock a GL mapset
	#just in case a grass mask exists, remove it!
	&system_silent("g.remove rast=MASK"); 
}

#-------------------------------------------------------------------------
# NAME: stop_GRASS 
#	Unlocks a grass mapset.
#-------------------------------------------------------------------------
sub stop_GRASS { 

	rename("$LOCATION/LOCK", "$LOCATION/UNLOCK")|| print STDERR "GL can't unlock mapset in $LOCATION/LOCK\n";
	if (defined $GL_ACCESS_LOG) {
		print ACCESSLOG "GL FINISHED ($value{ANALYSIS}): unlocked mapset $MAPSET\n";
		#print STDERR "GL FINISHED ($value{ANALYSIS}): unlocked mapset $MAPSET\n";
	}

	&flush(STDERR);
}

#################################################################
# MAIN subroutines for each GRASSLinks analysis option		#
#################################################################

#-------------------------------------------------------------------------
# NAME: display1 
#	This subroutine creates select menus for display request form.
#	Items displayed in select lists are from the $GLDATA_LIST file
#	and are a function of the current $value{PROJECT} code.
#-------------------------------------------------------------------------
sub display1 { 

	&begin_html("Display Request Form", "FORM");

	print << "ENDPRINT";
	<b>&#42;</b> To create a <b>map display</b>, select from
	the menus of <a href="$HTML_PATH/$GL_MATRIX" target="new">
	GRASS map layers</a> shown below.
	<br>
	<b>&#42;</b> Refer to the <a href="$GL_HELP#display" target="new">Help 
	document</a> if you need further information.
	<br>
	<b>&#42;</b> See bottom of this page for <a href="#Advanced">
	Advanced GRASSLinks Options</a>.
	<p>
	<hr>
	<p>
	<p><li><b>Select a single, raster map for display and query:
	</b><br>
ENDPRINT

	&make_select("RASTER", "8", "raster");

	print '
	<p><hr><p><b>
	<li>Select any number of vector maps for overlay display:
	</b><br>
	';

	&make_select("VECTOR", "8", "vector", "MULTIPLE");

	print '
	<p>
	Select colors for the first three vector maps
	(any others selected will default to black):
	<br>
	';

	&make_select("VCOLOR1", "1", "color");
	&make_select("VCOLOR2", "1", "color");
	&make_select("VCOLOR3", "1", "color");

	print '
	<p><hr><p><li><b>
	Select any number of sites maps to display as 
	overlay points:
	</b><br>
	';

	&make_select("SITES", "2", "sites", "MULTIPLE");

	print '
	<p>
	Select colors for up to the first three sites maps
	(any others selected will default to black):
	<br>
	';

	&make_select("SCOLOR1", "1", "color");
	&make_select("SCOLOR2", "1", "color");
	&make_select("SCOLOR3", "1", "color");

	print '
	<p><hr><p>
	<li><b>Select a region to display:</b>
	<br>
	';

	&make_select("REGION", "4", "region");

	print '
	<p><li>
	<b>and an image size for the map:
	</b><br>
	';

	&make_select("GIFSIZE", "3", "gifsize");

	print '
	<p>
	<p><li><b>
	and if you want a legend for the map:
	</b><br>
	';

	&make_select("LEGEND", "2", "legend");

	print '<br>
	<i>NB: Legends are only created for raster maps.</i>
	<p><hr><p>
	';

	&make_hidden("ANALYSIS", "display2");
	&make_hidden("PROJECT");
	&end_form ("Create the Display Map", "Reset");
	&end_html;
}

#-------------------------------------------------------------------------
# NAME: display2 
#	Create gif image of map layers and options selected by the
#	user in subroutine display1 and display in an html file.
#-------------------------------------------------------------------------
sub display2 {
	
	my ($mapDesc, $regionInfo);

	if ( (! $value{RASTER}) && (! $value{VECTOR}) && (! $value{SITES}) ) {
		&error("No Map Layers chosen for Display!\n");
		return;
	}

	# set defaults image size and region
	$value{GIFSIZE} = $DEFAULT_GIFSIZE unless defined $value{GIFSIZE};
	$value{REGION} = $DEFAULT_REGION unless defined $value{REGION}; 

	# file to contain script needed to create gif of map layer
	$value{PMAP_SCRIPT} = "pmap_script.$$";

	# gif file of map layer, must be in an htdocs directory
	$value{GIF} = "grasslinks.$$.gif";

	# file of description of map gif
	$value{DISPLAY_DESC} = "gl.dispDesc.$$";

	# make the pmap file need to create a GRASS Map
	$mapDesc = &make_pmap;

	# Get some more description info
	$regionInfo = $REGIONS{ $value{REGION} } ->{descShort};
	$mapDesc .= "<li>Initial display region: <b>$regionInfo</b></li>\n";
	$mapDesc .= "<li>Image Size: <b>$value{GIFSIZE} x $value{GIFSIZE}</b></li>\n";

	#save map display description text for re-use
	open (MAP_DESC, ">$TMP/$value{DISPLAY_DESC}");
	print MAP_DESC "<b>Map Display Elements:</b><ul>$mapDesc</ul>\n";
	close(MAP_DESC);

	# set the region & create the GIF 
	&set_region;
	&get_coords;
	&adjust_region;
	&create_gif; 

	# return map display to browser 
	&begin_html("Map Display, Zoom & Query", "FORM");
	&create_display_html;
	&end_form;
	&end_html;
}

#-------------------------------------------------------------------------
# NAME: display3
# 	Either query a raster and sites maps to return
# 	attribute & coordinate info for the user's selected mousepoint
# 	OR zoom-in, zoom-out, or pan and recreate gif image. 
#	All depends on value of $value{ZOOM}. 
# 	This whole subroutine depends on existence of temp files
# 	made by earlier subroutines. But a cronjob routinely cleans
# 	temp files more than one hour old.
#-------------------------------------------------------------------------
sub display3 { 


	my ($newMsg, $cmd, $sites_file, $sites_detail, @siteinfo, @catin, @catdata);

	# First test for existance of key temp files!
        my $fileOne = "$TMP/$value{DISPLAY_DESC}";
        my $fileTwo = "$TMP/$value{PMAP_SCRIPT}";
        my $fileThree = "$TMP/$value{GIF}";

        if (!( -r $fileOne && -r $fileTwo && -r $fileThree)) {
		&error("Sorry, your GRASSLinks session has timed out!\n");
		return;
        } else {
                &system_silent("touch $fileOne $fileTwo $fileThree");
        }

	#make sure ZOOM has a value, set to 0 as default if not defined
	if (!$value{ZOOM}) { $value{ZOOM} = 0;}

	# Calculate coordinates of selected center point, if pt valid
	if( $value{x} > 0 && $value{y} > 0) {
		$center{east} = int ( ( ($value{east}-$value{west}) * ($value{x}-1)/$value{COLS}) + $value{west} );
		$center{north} = int ( $value{north} - ( ($value{north}-$value{south}) * ($value{y}-1)/$value{ROWS}) );
	}

	#big if block to figure out if to QUERY, ZOOM, or PAN
	if ($value{ZOOM} == 0 ) {#query base maps
		$newMsg .= "<li>UTM coordinates of selected center point are: ";
		$newMsg .= " <b>$center{east}E, $center{north}N </b><br>\n";

		if ( ! $value{RASTER} || $value{RASTER} eq "none") {
               		#can't query a vector/site map
                	$newMsg .= "<li><b>Sorry, no Raster basemap available to query</b></li>\n";

	        } else { # find category value for selected point
			&system_errors_logged("g.region save=saved_region");
                	# reset region for raster query
               		&system_silent("g.region raster=$value{RASTER}");
			# get category info
			$cmd  =  "echo \"$center{east} $center{north}\"  | r.what -if input=$value{RASTER} null=\"null data\"";
			@catin = `$cmd`;
               		@catdata = split(/\|/,$catin[0]);
               		$newMsg .= "<li>Category information of raster basemap is: <b>\n";
               		$newMsg .= " ($catdata[3]) $catdata[4] </b></li>\n";
			# restore saved region
			&system_errors_logged("g.region saved_region");	

       		} #end of query on base rastermap

		if ( $value{SITEQUERY} && $value{SITEQUERY} ne "none" ) {
                        $sites_file = $value{SITEQUERY};
                        $sites_detail = `echo $center{east} $center{north} | s.what in=$sites_file`;
                        if ( $sites_detail ) {
                               	($siteinfo{e} , $siteinfo{n} , $siteinfo{rest}) = split(/\|/ , $sites_detail);
                               	$newMsg .= "<li>Category information of nearest site in selected sites map:<br> <b> $siteinfo{rest} </b></li>";
                        }
        	} #end of sites query if block

	} else { 
		#do the appropriate pan or zoom calculation
		#which require creating a new gif
		$value{GIF} = "grasslinks.$$.gif";

		if ($value{ZOOM} > 0) {#shrink the region
			$adjust = (1 / $value{ZOOM} / 2); 
		} else { #grow the region
			$adjust = $value{ZOOM} / -2; 
		}
		$north = $center{north} + (($value{north} - $value{south}) * $adjust);
		$south = $center{north} - (($value{north} - $value{south}) * $adjust);
		$east = $center{east} + (($value{east} - $value{west}) * $adjust);
		$west = $center{east} - (($value{east} - $value{west}) * $adjust);
		
		&system_errors_logged ("g.region n=$north s=$south e=$east w=$west ewres=$value{ew} nsres=$value{ns}");
		&get_coords;
		&adjust_region;
		&create_gif;
	}

	# Print out html
	&begin_html("Map Display, Zoom & Query", "FORM");
	&create_display_html("$newMsg");
	&end_form;
	&end_html;

} # end of sub display3

#-------------------------------------------------------------------------
# NAME: report1 
#	Allows user to select one or two raster maps for area
#	calculation. User can also select region.
#	BUG - all rasters in $GLDATA_LIST appear in select boxes
#	but not all are suitable for area calculations (e.g. shaded-relief)
#	Also -importance of region and any masks (which aren't apparent
#	to user) needs to be conveyed to user
#-------------------------------------------------------------------------
sub report1 {
	
	&begin_html("Areal Report of Mapped Information", "FORM");

	print << "ENDPRINT";
	Areas are calculated as a total for each category within
	the selected raster map(s).  A report on "Counties" would give the total
	area for each county.  A report on "Counties" against "Wetlands to
	16 Categories" would show how much of each wetland type occurs
	in each county. 
	<p>
	<b>&#42;</b> View information on 
	<a href="$HTML_PATH/$GL_MATRIX" target="new">
	Raster Maps</a> available in GRASSLinks for Areal Calculations.
	<br> 
	<b>&#42;</b> Refer to the <a href="$GL_HELP#report" target="new">
	Help document</a> for further information.
	<p>
	<hr>
	<p>
	<li><b>Select a raster map for area calculation:</b>
	<br>
ENDPRINT

	&make_select("RASTER", "5", "raster");

	print '<p><li><b>
	Select an optional second raster map to overlay with the first:
	</b><br>
	';
	&make_select("RASTER", "5", "raster");

	print "<p><hr><p><li><b>Select a region:</b><br>\n";

	&make_select("REGION", "3", "region");

	print << "ENDPRINT";
	<p>
	<b>NOTE</b>: Your selected region setting is very
	important for Areal Reports. This is for two reasons: 1) the larger
	the region setting, the longer the process takes; and 2) the report
	analysis will be clipped to the selected GRASS region. For example,
	if you were to do "Counties" against "Wetlands" with a region
	setting of "Marin County", your area totals would be restricted to
	the predefined Marin County region box (which would include all of
	Marin and some of the surrounding counties).  
	<p>
	<hr><p>
	<li><b>Input a Raster Grid Resolution Setting between 10 and 1000:</b>
	<INPUT type="text" size=5 name="RESOLUTION" value="250">
	Meters per Cell.
	<p>
	<b>NOTE:</b> 
	A coarse resolution will increase the speed of the reporting
	process but decrease the accuracy. But we suggest you only refine
	as needed. The finest resolution setting would be that of the
	original raster map(s).  See the appropriate metadata record
	for this value. 
	<p>
	<hr><p>
	<li><b>Select the units to report in:</b>
ENDPRINT

	&make_select("UNITS", "1", "repUnits");

	print "<p><hr><p>\n";
	&make_hidden("ANALYSIS", "report2");
	&make_hidden("PROJECT");
	&end_form ("Run Areal Report Calculation", "Reset");
	print "(estimated run time 30 seconds)<BR>\n";
	&end_html;
}

#-------------------------------------------------------------------------
# NAME: report2
# Do the area tabulations based on the raster(s) selected in
# report1 and then return the formatted results to user
# BUG - area calculations trickiness includes: 1) resolution of
# the selected region (greater res. faster yet coarser results;
# and 2) masks. Masks determine the area of the calculation and
# need to be hand made and stored in the format:
# $GISDBASE/$LOCATION_NAME/grasslinks0/cell/MASK.<region>
# If no mask exists then the extent of the selected region is
# the area of the calculation. This stuff needs to be conveyed
# to user so they know what they are getting
#-------------------------------------------------------------------------
sub report2 { 

	my ($reportDesc, $rnumber, $rast1, $rast2, $regionInfo, @rasts);
	my ($convert, $units, $att, $cat, $area);

	@rasts = @{ $Values{RASTER} }; 
	if (! @rasts) {# if no rasters selected
			&error("Areal Report Operation Failed! <br>You must select at least one raster file!");
			return;
	}

	if ( $rasts[1]) { 
		# two rasters selected - crosstab
		$rnumber = 2;
		$value{RASTER1} = $rasts[0];
		$value{RASTER2} = $rasts[1];
		&set_maxregion;

		# get some descriptive text
		$rast1 = $RASTERS{ $value{RASTER1} } ->{descShort};
		$rast2 = $RASTERS{ $value{RASTER2} } ->{descShort};
		$regionInfo = $REGIONS{ $value{REGION} } ->{descShort};
		$reportDesc = "<li>Report for the map layer: ";
		$reportDesc .= "  <b>$rast1</b> <br><li>against the map layer: ";
		$reportDesc .= "  <b>$rast2</b> <br><li>within the region setting: ";
		$reportDesc .= "  <b>$regionInfo</b>";
	} else { 
		# only one raster selected
		$rnumber = 1;
		$value{RASTER1} = $rasts[0];
		$value{RASTER} = $value{RASTER1};
		&set_region();

		# get some descriptive text
		$rast1 = $RASTERS{ $value{RASTER1} } ->{descShort};
		$regionInfo = $REGIONS{ $value{REGION} } ->{descShort};
		$reportDesc = "<li>Report for the map layer: ";
		$reportDesc .= " <b>$rast1</b><br><li>within the region setting: ";
		$reportDesc .= " <b>$regionInfo</b>";
	}

	# reset the resolution to speed up process
	$value{RESOLUTION} = 250 unless defined $value{RESOLUTION}; 
	if ($value{RESOLUTION} < 10 || $value{RESOLUTION} > 1000 ) {
		$value{RESOLUTION} = 250;
	}
	&system_silent("g.region res=$value{RESOLUTION}");
	$reportDesc .= "<br><li>Raster Grid Resolution Settings: "; 
	$reportDesc .= " <b>$value{RESOLUTION} Meters per Cell </b>"; 

	#to the grass stats routine & write output to temp file
	&system_errors_logged("r.stats -aql fs=\"^\" input=$value{RASTER1},$value{RASTER2} > $TMP/stats.$$");


	#create HTML code from GRASS temp stats file and close
	###########################################################
	if ($value{UNITS} eq "me") {
		$convert=1; $units="sq meters";}
	elsif ($value{UNITS} eq "k") {
		$convert=0.000001; $units="sq kilometers";}
	elsif ($value{UNITS} eq "mi") {
		$convert=0.0000003861; $units="sq miles";}
	elsif ($value{UNITS} eq "a") {
		$convert=0.0002471; $units="acres";}
	elsif ($value{UNITS} eq "h") {
		$convert=0.000002471; $units="hectares";}
	###########################################################

	#print out Report HTML 
	&begin_html("Areal Calculations");

	print << "ENDPRINT";
	$reportDesc
	<p><hr><p>
ENDPRINT

	printf ("<PRE><B>%-44s %8s</B><BR>\n", "    Category:", "Area in $units");
	open(STATS, "$TMP/stats.$$");
	if ($rnumber ==  1) { #only one raster
		while(<STATS>) {
			chop($_);
			($att,$cat,$area) = split(/\^/,$_);
			printf ("%4s %-40s %12.2f<br>\n", $att, $cat, $area*$convert);
			if ($. % 5 == 0 ) { print "<BR>\n";}
		} #end while loop
	} else { 
		#doing a crosstab
		$old = "";
		
		while(<STATS>) {
			chop;
			($att1, $cat1, $att2, $cat2, $area) = split(/\^/);
			if ($att1 eq $old) {
				printf("    %4s %-30s %12.2f<br>\n", $att2, $cat2, $area*$convert);
			} else {
				printf("\n%4s %-30s<br>\n", $att1, $cat1);
				printf("    %4s %-30s %12.2f<br>\n", $att2, $cat2, $area*$convert);
			}
			$old = $att1;
		} #end of while loop
	} #end of if block

	close (STATS);
	print "</PRE>\n";

	print << "ENDPRINT";
	<p><hr>
	<i>Please refer to the <a href="$GL_HELP#report" target="new">
	Help document</a> for further information on interpretting these results.
	</i>
	<hr><p>
ENDPRINT

	&end_html;
	&flush(STDERR);
} #end of report2 

#-------------------------------------------------------------------------
# NAME: reclass1
# 	Displays html form for select map layers for reclass.	
#-------------------------------------------------------------------------

sub reclass1 { 
	&begin_html("Reclassification of Mapped Information", "FORM");

	print << "ENDPRINT";
	This GIS tool allows you to create a new map from an
	existing one by combining categories from the chosen map into 
	one new category.  For example, one could create a map of East
	Bay counties by aggregating Contra Costa and Alameda from the 
	Counties map.  Or a user could simply isolate a category, 
	e.g., a map showing only Alameda County.
	<p>
	<b>&#42;</b> View information on 
	<a href="$HTML_PATH/$GL_MATRIX" target="new">
	Raster Maps</a> available in GRASSLinks for Reclass operations.
	<br> 
	<b>&#42;</b> Refer to the <a href="$GL_HELP#reclass" target="new">
	Help document</a> for further information.
	<p>
	<hr>
	<p>
	<li><b>Select a single <a href="$GL_HELP#raster" target="new">
	Raster</a> Map for reclassification:</b>
ENDPRINT

	&make_select("RASTER", "8", "raster");

	print "<p>\n";
	&make_hidden ("ANALYSIS", "reclass2");
	&make_hidden ("PROJECT");
	&end_form ("Continue to Select Categories");
	&end_html;
}

#-------------------------------------------------------------------------
# NAME: reclass2
#	Displays selection list of unique attribute values (categories) for
#	the raster selected in reclass1 & allows user to choose which of these
#	values to select out or aggregate as a new category in a new map.
#-------------------------------------------------------------------------
sub reclass2 { 

	my ($numcats, $rasttmp);
	
	# make sure a valid raster selected
	if (! $value{RASTER} || $value{RASTER} eq "none" ) {
		&error("No raster map selected");
		return;
	}

	$rasttmp = $RASTERS{ $value{RASTER} } ->{descShort};
	$value{CURRENT_ATT} = 1;

	&begin_html("Reclassification of Mapped Information, continued", "FORM");
	
	print << "ENDPRINT";
	As an example, if you wanted to create a new counties
	map, the Map Filename might be "counties.sue" and the Data
	Label for category 1 could be "East Bay" and it might
	include Alameda and Contra Costa counties.  Category 2
	could be "North Bay" with Marin, Sonoma, Napa, and Solano
	counties.

	Refer to the <a href="$GL_HELP#reclass" target="new">Help
	document</a> for further information.
	<p>
	<b>1) Choose categories that you wish to aggregate from your 
	selected raster map: </b><br>$rasttmp
	<p>
	<li><b>Available Categories:</b><br>
ENDPRINT

	$numcats = &find_cats ("$value{RASTER}");

	if ($numcats < 1 ) {
		&error("Reclassification of map \"$rasttmp\" <br> failed due to insufficient number of categories in that map. <p>Please select a different map.");
		return;
	}

	&make_select_cats("IN_CAT","6","1","multiple");
	print "<p>\n";

	print << "ENDPRINT";
	<li><b>Enter a Data Label for your aggregate category number 
	$value{CURRENT_ATT}:</b>
	<INPUT type="text" size=45 name="NEWCATLABEL">
	<BR><hr><BR>

	<b>2) Enter a new name for your map.</b><br>
	The reclass operation will create a new map.  A unique (but short)
	filename is necessary to avoid confusion with other users.
	<i>You must restrict punctuation to \"_\" and \".\", \
	do not include spaces. Any unapproved characters will be stripped.
	</i>
	<p>
	<b>Map Name:</b> <INPUT type="text" size=40 name="OUT_RASTER">
	<Br><hr><br>
	<b>3) Add more new categories or create the map:</b><br>
	You may opt to add more categories to your map, or if you have
	enough you can go on to create the requested map.
	<p>
	<INPUT TYPE="radio" NAME="NEXT" VALUE="MORE">
	Add more categories
	<b>or</b><br>
	<INPUT TYPE="radio" NAME="NEXT" VALUE="DONE" CHECKED>
	Create the new reclassified map
	<p>
ENDPRINT

	# this file must be defined here though not used until
	# reclass3 subroutine
	$value{RECLASS_RULES} = "reclassrules.$$";

	foreach $i ("RASTER", "CURRENT_ATT", "RECLASS_RULES", "PROJECT") {
		&make_hidden($i);
	}
	&make_hidden("ANALYSIS", "reclass3");
	&end_form("Continue Reclassification Operation", "Reset default values");
	&end_html;
}

#-------------------------------------------------------------------------
# NAME: reclass3
# 	Loop for more reclass categories or run the GRASS reclass program
#	and return results in html to user.
# REQUIRES: THe file $value{RECLASS_RULES} must be pre-defined.
#-------------------------------------------------------------------------
sub reclass3 {

	my ($theCats, $rasttmp); 

	if ( ! $value{OUT_RASTER} ) {
		&error("You must enter a name for the new Reclass Map!");
		return;
	}

	# First update reclass rules file
	@theCats = @{ $Values{IN_CAT} };
        if (@theCats) {# if category(s) selected
		if (! $value{NEWCATLABEL} ) {
			&error("No label input for the reclassification category");
			return;
		} else {
			open(RECLASS_RULES, ">>$TMP/$value{RECLASS_RULES}");
                	for ( $i = 0; $i <= $#theCats; $i++) {
				print RECLASS_RULES "$theCats[$i] = $value{CURRENT_ATT} $value{NEWCATLABEL}\n";
                	} 
			close(RECLASS_RULES);

		}
        } else {
		# no cats selected
		if ( $value{CURRENT_ATT} == 1 ) {
			&error("No category selected for Reclass Operation");
			return;
		}
	} 

	#create form for further input or do the reclass and display
	if ($value{NEXT} eq "MORE") { 
		&find_cats ("$value{RASTER}");
		$value{CURRENT_ATT}++;
		$rasttmp = $RASTERS{ $value{RASTER} } ->{descShort};

		&begin_html("Reclassified Map \"$value{OUT_RASTER}\"", "FORM");

		print << "ENDPRINT";
		<b>1) Select the next categories from the map "$rasttmp" 
		to aggregate and include in your new map:</b>  
		$value{OUT_RASTER}
		<p>
		<li><b>Available Categories:</b><br>
ENDPRINT
		&make_select_cats("IN_CAT","6","1","multiple");

		print << "ENDPRINT";
		<p>
		<li><b>Enter a Data Label for your aggregate category number
        	$value{CURRENT_ATT}:</b>
        	<INPUT type="text" size=45 name="NEWCATLABEL">
        	<BR><hr>
		<p>
		<b>3) Add more new categories or create the map:</b><br>
        	You may opt to add more categories to your map, or if you have
        	enough you can go on to create the requested map.
        	<p>
        	<INPUT TYPE="radio" NAME="NEXT" VALUE="MORE">
        	Add more categories
        	<b>or</b><br>
        	<INPUT TYPE="radio" NAME="NEXT" VALUE="DONE" CHECKED>
        	Create the new reclassified map
        	<p>
ENDPRINT

		foreach $i ("RASTER", "OUT_RASTER", "CURRENT_ATT", "RECLASS_RULES", "PROJECT") {
			&make_hidden($i);
		}
		&make_hidden("ANALYSIS", "reclass3");
		&end_form("Continue Reclassification Operation");
		&end_html;

	} 
	else { #DONE -create the reclass
		&system_errors_logged("r.reclass input=$value{RASTER} output=$value{OUT_RASTER} < $TMP/$value{RECLASS_RULES}");

		# create a GRASS colr file for the new reclass raster
		&system_errors_logged("r.colors -wq map=$value{OUT_RASTER} color=rainbow");

		# Create gif reclass map
		$value{REGION} = "raster";
		&set_region;
		&get_coords;
		&adjust_region;
		&create_display_map;

		# Use a legend to convey reclass information
		$value{LEGEND} = "yes";

		# print out resultant display image
		&begin_html("Reclassification of Mapped Information", "FORM");
		&create_display_html;
		&end_form;
		&end_html;
	} #end of if block
}

#-------------------------------------------------------------------------
# NAME: buffer1 
# 	Gives select form of available maps for buffer.
#-------------------------------------------------------------------------
sub buffer1 { 

	&begin_html("Buffering of Mapped Information", "FORM");

	print << "ENDPRINT";
	This GIS tool allows you to create a new map
	by building concentric rings of known distance
	around categories in an existing map.  
	<p>
	<b>&#42;</b> View information on 
	<a href="$HTML_PATH/$GL_MATRIX" target="new">
	Raster Maps</a> available in GRASSLinks for buffering operations.
	<br> 
	<b>&#42;</b> Refer to the <a href="$GL_HELP#buffer" target="new">
	Help document</a> for further information.<br>
	<p>
	<hr>
	<p>
	<li><b>Select a single <a href="$GL_HELP#raster" target="new">
	Raster</a> Map for Buffer Operations:</b>
	<br>
ENDPRINT

	&make_select("RASTER", "8", "raster");
	print "<p>\n";

	&make_hidden ("ANALYSIS", "buffer2");
	&make_hidden ("PROJECT");
	&end_form ("Continue Buffer Operation");
	&end_html;
}

#-------------------------------------------------------------------------
# NAME: buffer2 
# 	Select categories from raster selected in buffer1 around
# 	which to buffer; select buffer distance(s), units, region, 
#	resolution  and new map name. 
#-------------------------------------------------------------------------
sub buffer2 { 

	my ($numcats, $rasttmp);

	# make sure a raster selected
	if (! $value{RASTER} ) {
		&error("No raster map selected!");
		return;
	}	

	# get description of the selected raster map
	$rasttmp = $RASTERS{ $value{RASTER} } ->{descShort};

	# begin creating html form
	&begin_html("Buffering of Mapped Information, continued", "FORM");

	print << "ENDPRINT";
	<b>1) Select categories</b> from the <b>$rasttmp</b> map 
	that you would like to create buffer rings around.
	<p>
	<li><b>Available Categories:</b><br>
ENDPRINT

	$numcats = &find_cats ("$value{RASTER}");
	if ($numcats < 1 ) {
		&error("Buffer Operation on map \"$rasttmp\" <br> failed due to insufficient number of categories in that map. <p>Please select a different map.");
		return;
	}

	&make_select_cats2("CAT","6","1","multiple");

	print '<p><hr><p>
	<b>2) Select the units for your buffering distances:</b>
	 ';
	&make_select("UNITS", "1", "bufUnits");

	print << "ENDPRINT";
	<p>
	<b>3) Select the buffering distances.</b><br>
	Enter the outer distance for all rings separated by commas;
	e.g., 1,2,5 would create 3 rings at distances of 1, 2 and 5 units from the 
	edge of all categories selected above.
	<p>
	<b>Distances:</b> <INPUT type="text" size=40 name="DISTANCES">
	<p><hr><p>
	<b>4) Select a region:</b>
ENDPRINT
	&make_select("REGION", "1", "region");

	print << "ENDPRINT";
	<br>
	<i>NB: a smaller region will run faster.</i>
	<p>
	<b>5) Select a resolution to use:</b>
ENDPRINT
	&make_select("RESOLUTION", "1", "resValues");

	print << "ENDPRINT";
	<br>
	<i>NB: a coarse resolution will run much faster, but
	the pixels of your output raster file will be comparatively large.</i>
	<p>
	<hr>
	<p>
	<b>5) Enter a new name for your map.</b><br>
	The buffer operation will create a new map.  A unique (but short)
	filename is necessary to avoid confusion with other users.
	<i>You must restrict punctuation to alpha_numeric characters,
	any others will be deleted.</i>
	<p>
	<b>Your Map Name:</b>
	<INPUT type="text" size=40 name="OUT_RASTER">
	<p><hr><p>
ENDPRINT

	&make_hidden("RASTER");
	&make_hidden("ANALYSIS", "buffer3");
	&make_hidden("PROJECT");
	&end_form("Create Buffer Map", "Reset");
	&end_html;
}

#-------------------------------------------------------------------------
# NAME: buffer3
# 	Create a reclass rules file and then buffer  based on 
# 	parameters in this file.
#-------------------------------------------------------------------------
sub buffer3 {

	my ($bufDesc, $catNo, $catName);
	$bufDesc = "<li>Buffer Ring(s) of <b>$value{DISTANCES} $value{UNITS}</b> ";
	$bufDesc .= " around the categories: ";

	# make sure buffer distances, and out_raster filename input
	if (! $value{DISTANCES} ) {
		&error("No valid buffer distance(s) entered!");
		return;	
	}
	if (! $value{OUT_RASTER} ) {
		&error("No valid file name for new buffer map entered!");
		return;	
	}

	
	#write to reclass rules file
	$value{RECLASS_RULES} = "$TMP/glbuffer.reclassrules.$$";

	@theCats = @{ $Values{CAT} };
        if (@theCats) { # if cat(s) selected
		$bufDesc .= "<b> ";
		open(RECLASS_RULES, ">$value{RECLASS_RULES}") || print STDERR "GL3 buffer3 unable to open reclass rules file .$value{RECLASS_RULES}.\n";

                for ( $i = 0; $i <= $#theCats; $i++) {
			($catNo, $catName) = split (/,/, $theCats[$i]);
			print RECLASS_RULES "$catNo = 1 origin of buffer\n";
			$bufDesc .= " $catName";
			if ($i > 0) { $bufDesc .= " , "; }
                } 

		$bufDesc .= "</b> ";
		close(RECLASS_RULES);

        } else {

		&error("No category/ies selected around which to buffer!");
		return;
	} 

	#do the grass reclass prior to buffering
	&system_errors_logged("r.reclass input=$value{RASTER} output=buff.recl < $value{RECLASS_RULES}");

	# set the region and resolution
	&set_region;
	&get_coords;
	&set_resolution;
	
	#do the buffering
	&system_errors_logged("r.buffer -q input=buff.recl output=$value{OUT_RASTER} distances=$value{DISTANCES} units=$value{UNITS}");
	&system_silent("g.remove rast=buff.recl");

	&adjust_region;
	&create_display_map("$bufDesc");

	# return html file
	&begin_html("Buffer of Mapped Information", "FORM");
	&create_display_html;
	&end_form;
	&end_html;
	&flush(STDERR);
}

#-------------------------------------------------------------------------
# NAME: combine1 
# 	Select two raster maps for which attribute categories will
# 	then be displayed in selection lists;
# 	based on which of these are selected, a new map can be created.
#-------------------------------------------------------------------------
sub combine1 {

	&begin_html("Coincidence of Mapped Information", "FORM");

	print << "ENDPRINT";
	This GIS tool allows you to create a new map that highlights the
	overlap of information in two existing maps.
	For example, one could create a map of tidal wetlands in Marin County
	by choosing Marin from the Counties map and all tidal wetlands from the
	Wetlands to 16 categories map.
	<p>
	<b>&#42;</b> View information on 
	<a href="$HTML_PATH/$GL_MATRIX" target="new">
	Raster Maps</a> available in GRASSLinks for Combine operations.
	<br> 
	<b>&#42;</b> Refer to the <a href="$GL_HELP#combine" target="new">
	Help document</a> for further information.
	<p>
	<hr>
	<p>
	<li><b>Select two different raster maps to combine</b>
	<br>
ENDPRINT
	&make_select("RASTER1", "5", "raster");
	print "<p>";
	&make_select("RASTER2", "5", "raster");
	print "<p>\n";
	&make_hidden("ANALYSIS", "combine2");
	&make_hidden("PROJECT");
	&end_form ("Continue Combine Operation");
	&end_html;
}
	
#-------------------------------------------------------------------------
# NAME: combine2 
# 	User selects categories to combine for rasters selected in
# 	combine1 form.
#-------------------------------------------------------------------------
sub combine2 {

	my ($rast1, $rast2);

	&begin_html("Coincidence of Mapped Information, continued", "FORM");

	if ( (! ($value{RASTER1} && $value{RASTER2}) ) || $value{RASTER1} eq $value{RASTER2}) {
		&error("You need to select two different input maps");
		return;
	}
	else { # proceed
		$rast1 = $RASTERS{ $value{RASTER1} } ->{descShort};
		$rast2 = $RASTERS{ $value{RASTER2} } ->{descShort};
		print << "ENDPRINT";
		<b>1) Select categories from each map that you would 
		like to combine.</b>
		<p>
		<dd><li><b>Categories for first map:</b> $rast1
ENDPRINT

		my $numcats = &find_cats("$value{RASTER1}");
		if ($numcats < 1 ) {
			&error("Combine Operation on map \"$rast1\" <br> will fail due to insufficient number of categories in that map. <p>Please select a different map.");
			return;
		}
		&make_select_cats2("CAT1","4","1","multiple");

		$numcats = &find_cats ("$value{RASTER2}");
		if ($numcats < 1 ) {
			&error("Combine Operation on map \"$rast2\" <br> will fail due to insufficient number of categories in that map. <p>Please select a different map.");
			return;
		}
		print "<p><dd><li><b>Categories for second map:</b> $rast2\n";
		&make_select_cats2("CAT2","4","1","multiple");

		print "<p><hr><p>\n";
		print "<b>2) Select a region:</b>";

		&make_select("REGION", "1", "region");

		print "<br><i>NB: a smaller region will run faster.</i><p>\n";
		print "<b>3) Select a resolution to use:</b>";
		&make_select("RESOLUTION", "1", "resValues");

		print << "ENDPRINT";
		<br>
		<i>NB: a coarse resolution will run much faster, but
		the pixels of your output raster file will be very large.</i>
		<p>
		<hr>	
		<p>
		<b>4) Enter a new name for your map.</b><br>
		The combine operation will create a new map.  A unique (but short) 
		filename is necessary to avoid confusion with other users.
		<i>NB: you must restrict punctuation to alpha_numeric characters;
		any others will be deleted.</i>
		<p>
		<b>Map Name:</b>
ENDPRINT
	
		print "<INPUT type=\"text\" size=40 name=\"OUT_RASTER\">";
		print "<p>\n";

		&make_hidden("ANALYSIS", "combine3");
		&make_hidden("PROJECT");
		&make_hidden("RASTER1");
		&make_hidden("RASTER2");
		&end_form ("Create the new Combine Map\n");
		&end_html;
	} #endif
} # end of sub combine2

#-------------------------------------------------------------------------
# NAME: combine3 
# 	Create and then return the new combined map request
#-------------------------------------------------------------------------
sub combine3 {

	my ($combineDesc, $catNo, $catName);

	# make sure a mapname entered for new combine map
	if (! $value{OUT_RASTER} ) {
		&error("You must enter a new name for the combine map!");
		return;
	}

	#get descriptive info on rasters selected for combine operation
	my $rast1 = $RASTERS{ $value{RASTER1} } ->{descShort};
	my $rast2 = $RASTERS{ $value{RASTER2} } ->{descShort};
	my $mapcalcFile = "$TMP/mapcalcrules.$$";

	#create mapcalc rules file needed by grass
	open(MAPCALC_RULES, "> $mapcalcFile");
	print MAPCALC_RULES "$value{OUT_RASTER} = if (";

	#do rules for CAT1
	@theCats = @{ $Values{CAT1} };
        if (@theCats) {# if cat(s) selected
		$combineDesc = "<li>Coincidence of the categories: <b>"; 
                for ( $i = 0; $i <= $#theCats; $i++) {
			($catNo, $catName) = split (/,/, $theCats[$i]);
			print MAPCALC_RULES "$value{RASTER1} == $catNo ";
			$combineDesc .= " $catName";
			if ($i < $#theCats ) { 
				print MAPCALC_RULES "|| "; 
				$combineDesc .= " , "; 
			}
                } 
		$combineDesc .= "</b> in the raster map <b>$value{RASTER1}</b>";
        } else {
		close(MAPCALC_RULES);
		&error("You must select category/ies for the map layer \"$rast1\" ");
		return;
	} 

	print MAPCALC_RULES ", if ( ";

	#do rules for CAT2
	@theCats = @{ $Values{CAT2} };
        if (@theCats) {# if cat(s) selected
		$combineDesc .= "<br>and the categories: <b>"; 
                for ( $i = 0; $i <= $#theCats; $i++) {
			($catNo, $catName) = split (/,/, $theCats[$i]);
			print MAPCALC_RULES "$value{RASTER2} == $catNo ";
			$combineDesc .= " $catName";
			if ($i < $#theCats ) { 
				print MAPCALC_RULES "|| "; 
				$combineDesc .= " , "; 
			}
                } 
		$combineDesc .= "</b> in the raster map: <b>$value{RASTER2}</b><br>";
        } else {
		close(MAPCALC_RULES);
		&error("You must select category/ies for the map layer \"$rast2\" ");
		return;
	} 

	print MAPCALC_RULES ",1) )\n";
	close(MAPCALC_RULES);

	#set the region and resolution
	&set_maxregion;
	&get_coords;
	&set_resolution;

	#do the combine operation via mapcalc
	&system_errors_logged("r.mapcalc < $mapcalcFile");

	&adjust_region;
	&create_display_map( "$combineDesc" );

	#return html
	&begin_html("Coincidence of Mapped Information", "FORM");
	&create_display_html;
	&end_form;
	&end_html;
	&flush(STDERR);
}

#################################################################
# HTML formatting subroutines					# 
#################################################################

#-------------------------------------------------------------------------
# NAME: begin_html 
#	prints out html header
# ARGS: $_[0] = Title to appear in <title> section, and at top of <body>
#	$_[1] = set to "FORM" if html is a form
# REQUIRES: That the variable $project{title} indicates the
#	title of the current GL project to be noted on the html form.
# 	SHOULD BE CONFIGURED FOR LOCAL GRASSLinks Site.
#-------------------------------------------------------------------------
sub begin_html {
	my ( $title, $form ) = @_;

	print "Content-type: text/html\n\n";

	print << "ENDPRINT";
	<HTML>
	<HEAD>
	<TITLE>GRASSLinks: $title</TITLE>
	</HEAD>
	<BODY bgcolor="#efefeg">
	<center>
	<H2>$GL_HEADER</H2>
	<p>
	<H3>$project{title}</H3>
	</center>
	<img src="$HTML_PATH/green_line.gif" width=100%>
	<p>
ENDPRINT

	if ( $form == "FORM") {
		print "<FORM ACTION=\"$GL_ACTION\" METHOD=\"POST\">\n";
	}

	print "<h2>$title</h2>\n";
}

#-------------------------------------------------------------------------
# NAME: end_form
#	Creates a submit and/or reset button on html form
# ARGS: $_[0] = Submit button text, (optional).
# ARGS: $_[1] = Reset button text, (optional).
#-------------------------------------------------------------------------
sub end_form { 

	my ($submit, $reset) = @_;

	if ( $submit ) { 
		print "<b><INPUT TYPE=\"submit\" VALUE=\"$submit\"></b>\n";
	}
	if ( $reset ) { 
		print " or <INPUT TYPE=\"reset\" VALUE=\"$reset\"><br>\n";
	}
	print "</FORM>\n\n";
}

#-------------------------------------------------------------------------
# NAME: end_html 
# 	Print Links and text to appear at end of each GL html page
# 	SHOULD BE CONFIGURED FOR LOCAL GRASSLinks Site.
#-------------------------------------------------------------------------
sub end_html { 

	&show_gladvanced;
	print << "ENDPRINT";
	<br><img src="$HTML_PATH/green_line.gif" width=100%>
	<p>
	<center>
	<sub>
	[
	<a href="$GL_URL">GRASSLinks Home</a>
	] [ 	
	<a href="$HTML_PATH/$GL_MATRIX">GRASSLinks Data</a>
	] [	
	<a href="$GL_HELP">GRASSLinks Help</a>
	] [	
	<a href="$GL_MAILTO">GRASSLinks Feedback</a>
	]
	</sub>
	</center>
	<p>
	</BODY>
	</HTML>
ENDPRINT

}

#-------------------------------------------------------------------------
# NAME: show_gladvanced 
# 	Prints out menu of advanced GRASSLinks options
#-------------------------------------------------------------------------
sub show_gladvanced {

	print << "ENDPRINT";
	<img src="$HTML_PATH/green_line.gif" width=100%>
	<p>
        <table border=1 cellpadding=10 width=100%>
        <tr align=center><td colspan=5>
        <a name="Advanced"><b>Advanced GRASSLinks Options</b></a><br>
	<i>Select any of the following to perform real-time
	GIS operations.</i>
	</td></tr>
        <tr>
        <td>
	<a href="$GL_ACTION?ANALYSIS=display1+PROJECT=$value{PROJECT}">
	Display<br> Operations</a></td>
        <td>
	<a href="$GL_ACTION?ANALYSIS=buffer1+PROJECT=$value{PROJECT}">
	Buffer<br> Operations</a></td>
        <td>
	<a href="$GL_ACTION?ANALYSIS=reclass1+PROJECT=$value{PROJECT}">
	Reclass<br> Operations</a></td>
        <td>
	<a href="$GL_ACTION?ANALYSIS=combine1+PROJECT=$value{PROJECT}">
	Combine<br> Operations</a></td>
        <td>
	<a href="$GL_ACTION?ANALYSIS=report1+PROJECT=$value{PROJECT}">
	Area<br> Calculations</a></td>
        </tr>
ENDPRINT
	if (-e "$HTML_FULLPATH/$GL_MATRIX" ) {
		print << "ENDPRINT";
		<tr align=center><td colspan=5>
		<a href="$HTML_PATH/$GL_MATRIX" target="new">
		View information on all GRASS data layers available via GRASSLinks
		</a></td></tr>
ENDPRINT
	}

        print "</table>\n";

} 

#-------------------------------------------------------------------------
# NAME: create_display_html
#	Creates html text for map display and a legend if requested.
# REQUIRES: $value{GIF} and $value{DISPLAY_DESC} files must exist
# ARGS: $_[0] = any query result text for display 
#	$_[1] = set to "NOTABLE" if utm coord table around gif image
#		not wanted. 
#-------------------------------------------------------------------------
sub create_display_html {
	
	my ($qMesg, $wantTable) = @_;

	# print map display description
	&print_file("$TMP/$value{DISPLAY_DESC}");

	# print query results
	if ( $qMesg ) {
		print "<p><b>Query Results:</b><br>\n";
		print "<ul>$qMesg</ul>\n";
	}

	# print out map gif in table or not
	if ( $wantTable eq "NOTABLE" ) {	
		print << "ENDPRINT";
		<p><hr><p>
		<center>
        	<input type=image src="$HTML_TMP/$value{GIF}" ismap
        	height=$value{ROWS} width=$value{COLS}>
        	</center><p>
ENDPRINT

	} else {
		# update coord info if needed	
		if (%coord) {
			foreach $i (north, south, east, west) {
				$value{$i} = $coord{$i};
			}
		}

		print << "ENDPRINT";
		<p><hr><p><center>
		<table border=0 CELLSPACING=4 CELLPADDING=2 >
		<tr align=center>
		<td></td>
		<td>UTM North: $value{north}</td></tr>
		<td></td>
		<tr>
		<td align=right>UTM West:<br> $value{west}</td>
		<td align=center>
       	 	<input type=image src="$HTML_TMP/$value{GIF}" ismap
       	 	height=$value{ROWS} width=$value{COLS}>
		</td>
		<td align=left>UTM East:<br> $value{east}</td></tr>
		<tr align=center>
		<td></td>
		<td>UTM South: $value{south}</td></tr>
		<td></td>
		</table>
        	</center><p>
ENDPRINT
	}

	# print out a legend table if user insists	
	if( $value{LEGEND} eq "yes" ) {
		if ( $value{ANALYSIS} eq "display2" || $value{ANALYSIS} eq "reclass3" ) {
			&create_legend;
		} else {
			&print_file("$TMP/$value{LEGEND_TABLE}");
		}
	}

	&write_zoomtext;
	&make_hidden_display;

} 

#-------------------------------------------------------------------------
# NAME: make_select 
#	Create select menu based on array lists read in in the read_gldata
#	and set_gloptions subroutines.
# ARGS: $_[0] = Variable name in name value pair, (required).
#	$_[1] = The size of the select list box, (required).
#	$_[2] = The type of variable, needed to identify all options
#		in the $GLfiles{} hash, (required).
#	$_[3] = Set to "MULTIPLE" if multiple options can be selected. 
# NOTE: The order in which options are displayed in select lists 
#	depends on the order in which they appear in the GLDATA_LIST or
#	in the set_gloptions subroutine.
#-------------------------------------------------------------------------
sub make_select { 

	my ($varName, $listSize, $varType, $multy) = @_;
	my ($fname, $fdesc);

	print  "<SELECT $multy NAME=\"$varName\" SIZE=\"$listSize\">";
	
        for $newrec ( @{ $GLfiles{$varType} } ) {
                $fname = $newrec->[2];
		$fdesc = $newrec->[3];
		if ($varType eq "region" ) {
			# set selected to $project{region}
			if ($fname eq $project{region} ) {
              		   print "<OPTION VALUE=\"$fname\" SELECTED>$fdesc\n";
			} else { 
              		   print "<OPTION VALUE=\"$fname\">$fdesc\n";
			}
		} else {	
			# "!!" in field 1 indicates default value
			if ($newrec->[1] =~ /\!\!/ ) {
              		   print "<OPTION VALUE=\"$fname\" SELECTED>$fdesc\n";
			} else { 
              		   print "<OPTION VALUE=\"$fname\">$fdesc\n";
			}
		} 
        } 

	# Add any user created raster files to a raster pick list
	if ($varType eq "raster" ) { 
		for ($i=1; $i<=$N_GL_USERS; $i++){
			$loctmp = "$ENV{GISDBASE}/$ENV{LOCATION_NAME}/grasslinks$i";
			&system_errors_logged("ls $loctmp/cell > $TMP/$$.$i");
			open(CELL, "$TMP/$$.$i");
			while(<CELL>){
				print "<OPTION VALUE=$_\@grasslinks$i>$_\n";
			}
			close(CELL);
			unlink("$TMP/$$.$i");
		}
	}
	
	print "</SELECT>";
} 

#-------------------------------------------------------------------------
# NAME: make_select_cats 
#	Create a select menu from an array @catlist created by sub find_cats
# ARGS: $_[0] = NAME, (required).
# 	$_[1] = SIZE of select list, (required).
#	#_[2] = default selected option, (optional). 
#	$_[3] = MULTIPLE, if multiple values can be selected, (optional).
# NOTE: This subroutine assumes the existance of the array @catlist
#	which contains the categories for the select list. This array
#	is created by the subroutine findcats().
#-------------------------------------------------------------------------
sub make_select_cats {

	my ($varName, $listSize, $default, $multy) = @_;
	my $counter=1;

	print "<dd>\n";
	print  "<SELECT $multy NAME=\"$varName\" SIZE=\"$listSize\">";

	foreach $i (@catlist) { 
		if ($default == $counter++ ) { 
        		print "<OPTION VALUE=\"$i\" SELECTED>$category{$i}\n";
		} else {
        		print "<OPTION VALUE=\"$i\">$category{$i}\n";
		}
	}

	print "</SELECT>\n";
}

#-------------------------------------------------------------------------
# NAME: make_select_cats2
#	Create select menu from @catlist and %category created by sub find_cats
# ARGS: $_[0] = NAME, (required).
# 	$_[1] = SIZE of select list, (required).
#	#_[2] = default selected option, (optional). 
#	$_[3] = MULTIPLE, if multiple values can be selected, (optional).
# NOTE: This subroutine assumes the existance of the array @catlist
#	which contains the categories for the select list. This array
#	is created by the subroutine findcats().
#	THIS SUBROUTINE IS DIFFERENT FROM make_select_cats IN THAT IT
#	uses both the category number and name to form the VALUE.
#	See &buffer3 or &combine3 to see how it is used.
#-------------------------------------------------------------------------
sub make_select_cats2 {

	my ($varName, $listSize, $default, $multy) = @_;
	my $theValue;
	my $counter=1;

	print "<dd>\n";
	print  "<SELECT $multy NAME=\"$varName\" SIZE=\"$listSize\">";

	foreach $i (@catlist) { 
		$theValue = "$i,$category{$i}";
		if ($default == $counter++ ) { #line no.=one to be selected
        		print "<OPTION VALUE=\"$theValue\" SELECTED>$category{$i}\n";
		} else {
        		print "<OPTION VALUE=\"$theValue\">$category{$i}\n";
		}
	}

	print "</SELECT>\n";
}

#-------------------------------------------------------------------------
# NAME: write_zoomtext 
#	Writes out html text for query, zoom and pan options
#	on the GL map display and query html form.
#-------------------------------------------------------------------------
sub write_zoomtext {

	my ($thesite, @sites);
	
	print << "ENDPRINT"; 
	<p><hr><p>
	<b>Query and Display Options: </b>
	<UL>
	<LI>Query the base raster map and any selected site maps 
	by clicking on the image at your query point.  
	This will quickly reload the existing image.  
	Note: images are only maintained on the server for one hour, 
	after that you will need to regenerate the image with the 
	display form.<br>
	<b>Query the selected raster base map: </b>
	<INPUT TYPE="radio" NAME="ZOOM" VALUE="0" CHECKED> 
ENDPRINT
	if (defined $value{SITES} ) {
		print << "ENDPRINT";
		<br>
		<b>Query nearest site in one of the displayed sites maps: 
		</b>
		<SELECT NAME="SITEQUERY">
		<OPTION VALUE="none" SELECTED> Select 
ENDPRINT
		@sites = @{ $Values{SITES} };
        	if (@sites) {# if site(s) selected
                	for ( $i = 0; $i <= $#sites; $i++) {
                        	last if $sites[$i] eq "none";
            			$thesite = $sites[$i];
		            	print "<OPTION VALUE=\"$thesite\">$SITES{ $sites[$i] }->{descShort}\n";
			} # end for
      			print "</SELECT><BR>\n";
         	}
	}

	print << "ENDPRINT";
	<p>
	<LI>Zoom in or out by first selecting a magnification, then 
	clicking on your new image center.  This will regenerate the image 
	from scratch. <br>
		<b>Zoom in: </b>
		<INPUT TYPE="radio" NAME="ZOOM" VALUE="2"> x2
		<INPUT TYPE="radio" NAME="ZOOM" VALUE="3"> x3
		<INPUT TYPE="radio" NAME="ZOOM" VALUE="5"> x5
		&#160 &#160 &#160 &#160
		&#160 &#160 &#160 &#160
		<b>Zoom out:</b>
		<INPUT TYPE="radio" NAME="ZOOM" VALUE="-2"> x1/2
		<INPUT TYPE="radio" NAME="ZOOM" VALUE="-3"> x1/3
		<INPUT TYPE="radio" NAME="ZOOM" VALUE="-5"> x1/5
		<br><br>
	
	<LI>Pan across the image.  Select a new center point.  This will
	recreate the image from scratch, allowing you to display areas beyond
	the current image.<br>
		<b>Pan:</b>
		<INPUT TYPE="radio" NAME="ZOOM" VALUE="1"> 
	</UL>
ENDPRINT
	
} 

#-------------------------------------------------------------------------
# NAME: make_hidden
#	Writes hidden variables to html file.
# ARGS: $_[0] = NAME of hidden variable, (required)
#	$_[1] = Value for hidden variabl, (optional - if not set, this
#		value is set to $value{NAME} 
#-------------------------------------------------------------------------
sub make_hidden { 
        my $varName = $_[0];
        my $varValue;

        if ($_[1]) {
                $varValue = $_[1];
        } else {
                $varValue = $value{$varName};
        }

        print "<INPUT TYPE=\"hidden\" NAME=\"$varName\" VALUE=\"$varValue\">\n";

}

#-------------------------------------------------------------------------
# NAME: make_hidden_display 
#	Write hidden variables for display3 subroutine.
#-------------------------------------------------------------------------
sub make_hidden_display { 

	my (@a, @sites);

	@a = ("GIF","REGION","PROJECT","GIFSIZE","COLS","ROWS","PMAP_SCRIPT","DISPLAY_DESC", "LEGEND", "LEGEND_TABLE");

	foreach $i (@a) {
		&make_hidden($i);
	}

	foreach $i ("north", "south", "east", "west", "ew", "ns") {
		# note: if no value for %coord hash, then set to
		# $value{north}, etc in sub make_hidden

		&make_hidden("$i", "$coord{$i}");
	}

	@sites = @{ $Values{SITES} };
	for ( $i = 0; $i <= $#sites; $i++) {
      		&make_hidden("SITES" , $sites[$i]);
   	}

	&make_hidden("ANALYSIS", "display3");
	&make_hidden("RASTER", "$value{OUT_RASTER}"); 

}  

#################################################################
# GRASS Helper Subroutines					#
#################################################################
#-------------------------------------------------------------------------
# NAME: get_filepath 
# 	Get path and filename of a GRASS map element file such as
# 	category, color, metadata for a grass raster file.
# ARGS: $_[0] = grass element for which to find file, (required).
#	$_[1] = grass raster file for which filename is sought. (required).
# RETURNS: The name of the file
#-------------------------------------------------------------------------
sub get_filepath { 

	my ($grassElement, $grassFile) = @_;
	my (@junky, $junk);

	@junky = `g.findfile element=$grassElement file=$grassFile`;
	($junk, $filename) = split (/=/, $junky[3]);
	$filename =~ s/'//g;

	return $filename;
}

#-------------------------------------------------------------------------
# NAME: find_cats 
#	Gets the unique category values for a grass raster file and
#	sticks them in array @catlist.
# ARGS: $_[0] = grass raster file
# RETURNS: Implicitly returns , the array @catlist - sets value for it.
#	Explicitly returns $totalcats, the number of categories.
#-------------------------------------------------------------------------
sub find_cats {
	
	my $raster = $_[0];
	my $theCatsFile;
	@catlist=();
	$totalcats = 0; 

	$theCatsFile = &get_filepath("cats", $raster);

	open (CATS, $theCatsFile) || print STDERR "GL find_cats can't open cats file for $raster.\n";
	while(<CATS>) {
		chop;
		@data = split(/:/);
		if ($data[0] && $data[1]) {
			$category{$data[0]} = $data[1];
			@catlist = (@catlist, $data[0]);
			$totalcats++;
		}
	}
	close(CATS);
	return $totalcats;
}

#-------------------------------------------------------------------------
# NAME: set_region 
# 	Sets grass region by which grass operations will be bound
# 	for grass operations that only use one raster at a time.
#-------------------------------------------------------------------------
sub set_region { 

	if ( $value{REGION} eq "raster" ) {
		if ( $value{RASTER} && $value{RASTER} ne "none" ) {
			&system_errors_logged("g.region raster=$value{RASTER}");
		} else {
			&system_errors_logged("g.region $DEFAULT_REGION");
		}
	} else {
		if ( $value{REGION} && $value{REGION} ne "none" ) {
			# set the region to one defined by user
			&system_errors_logged("g.region $value{REGION}");
		} else {
			&system_errors_logged("g.region $DEFAULT_REGION");
		}
	}

} 
		
#-------------------------------------------------------------------------
# NAME: set_maxregion 
#	Set region when more than one raster has been selected
#	to largest extent of the two rasters or to a specifically
#	set region in value{REGION}.
#-------------------------------------------------------------------------
sub set_maxregion {

	my %coord1;
	my %coord2;

	if (defined $value{REGION} && $value{REGION} ne "raster") { 
		&system_errors_logged("g.region $value{REGION}");
	}
	else {
		#set region to first raster, then get its extent coordinates
		&system_errors_logged("g.region raster=$value{RASTER1}");
		&get_coords;
		%coord1 = %coord;

		#set region to second raster, then get its extent coordinates	
		&system_errors_logged("g.region raster=$value{RASTER2}");
		&get_coords;
		%coord2 = %coord;

		foreach $i ("north", "east") {
			if ($coord2{$i} < $coord1{$i}) {
				$coord{$i} = $coord1{$i};
			} else {
				$coord{$i} = $coord2{$i};
			}
		}
		foreach $i ("south", "west") {
			if ($coord2{$i} > $coord1{$i}) {
				$coord{$i} = $coord1{$i};
			} else {
				$coord{$i} = $coord2{$i};
			}
		}
		foreach $i ("ew", "ns") {
			if ($coord2{$i} < $coord1{$i}) {
				$coord{$i}=$coord1{$i};
			} else { 
				$coord{$i}=$coord2{$i};
			}
		}
		&system_errors_logged("g.region n=$coord{north} s=$coord{south}\
		e=$coord{east} w=$coord{west} ewres=$coord{ew} nsres=$coord{ns}");
	}
}

#-------------------------------------------------------------------------
# NAME: get_coords 
#	Gets the coord values for the current GRASS WIND file.
#	Puts these coords in the global/static  hash %coord{}.
#-------------------------------------------------------------------------
sub get_coords { 

	my ($name, $value);
	%coord = ();
	open (WIND, "$LOCATION/WIND") || print STDERR "GL get_coords couldn't open WIND file = .$LOCATION/WIND.\n";

	while (<WIND>) {
		s/[:-]//g;
		s/resol//g;
		chop;
		($name, $value) = split(/ +/);
		$coord{$name} = $value;
	}
	close (WIND);
	&flush(STDERR);
}

#-------------------------------------------------------------------------
# NAME: adjust_region 
#	Figure out scaled dimensions of the image, based on region & gifsize.
#-------------------------------------------------------------------------
sub adjust_region {

	if (!$value{GIFSIZE}) {$value{GIFSIZE} = 400;}

	if ($coord{rows} > $coord{cols} ) {
		#portrait orientation
		$value{ROWS} = $value{GIFSIZE};
		$value{COLS} = int ($value{GIFSIZE} * $coord{cols} / $coord{rows});
	} else { 
		#landscape orientation
		$value{COLS} = $value{GIFSIZE};
		$value{ROWS} = int ($value{GIFSIZE} * $coord{rows} / $coord{cols});
	}
	# now adjust resolution of region based on target image
	# FORMERLY:
	# $save_ew = $coord{ew};
	# $save_ns = $coord{ns};
	# &system_errors_logged("g.region nsres=$save_ns ewres=$save_ew");

	# changed to Simon Cox's formula by plf 1/14/97
	$coord{ns} = ($coord{north} - $coord{south})/$value{ROWS};
   	$coord{ew} = ($coord{east} - $coord{west})/$value{COLS};
	&system_errors_logged("g.region nsres=$coord{ns} ewres=$coord{ew}");
	
}

#-------------------------------------------------------------------------
# NAME: set_resolution 
#	Resets region based on a resolution selection.
#-------------------------------------------------------------------------
sub set_resolution { 

	# set default res to coarse (5)
	$value{RESOLUTION} = 5 unless defined $value{RESOLUTION};

	# min base resolution is 100 unless user wants fine
	unless ( $value{RESOLUTION} == 1 ) {
		if ( ($coord{ew} + $coord{ns}) < 190 ) {
			$coord{ew} = 100;
			$coord{ns} = 100  
		}
	}

	$coord{ew}= int($coord{ew}*$value{RESOLUTION});
	$coord{ns}= int($coord{ns}*$value{RESOLUTION});

	&system_errors_logged("g.region ewres=$coord{ew} nsres=$coord{ns}");
}


#################################################################
# SUBROUTINES FOR CREATING GRASSLinks GIF Images 		#	
#################################################################

#-------------------------------------------------------------------------
# NAME: make_pmap 
# 	Create the pmap script needed by grass to create map gif.
# REQUIRES: $value{PMAP_SCRIPT} already defined by calling function.
# RETURNS:  $dispMsg containing text info about display map.
#-------------------------------------------------------------------------
sub make_pmap { 

	$dispMsg = "";
	my ($rastDesc,@vects,$num,$vcolor,$vectInfo,@sites,$siteInfo,$scolor);   

	open(PMAP_SCRIPT, ">$TMP/$value{PMAP_SCRIPT}");

	#write out raster information to PMAP scripts
	if ( $value{RASTER} ne "none" && $value{RASTER} ) {
		print PMAP_SCRIPT "raster $value{RASTER}\n";
		$rastDesc = $RASTERS{$value{RASTER}}->{descShort};
		$rastDesc = $value{RASTER} unless defined $rastDesc;
		$dispMsg .= "<li>Raster map: <b>$rastDesc</b></li>\n";
	}

	# write out vector information if vects selected 
	@vects = @{ $Values{VECTOR} }; 
	if (@vects) {
		for ( $i = 0; $i <= $#vects; $i++) {
			last if $vects[$i] eq "none";
			$num = $i+1; # because of numbering
			$vcolor = "VCOLOR"."$num";
			$vcolor = $value{$vcolor} or $vcolor = $DEFAULT_COLOR;
			print PMAP_SCRIPT "vector $vects[$i]\n";
			print PMAP_SCRIPT "    color $vcolor\n";
			#set vector width to 1
			print PMAP_SCRIPT "    width 1\n";
			print PMAP_SCRIPT "    end\n";
			$vectInfo = $VECTORS{ $vects[$i] }->{descShort};
			$dispMsg .= "<li>Vector map: <b>$vectInfo</b> in $vcolor</li>\n";
		} 
	} 
	
	# write out sites information if sites selected
	@sites = @{ $Values{SITES} };
	if (@sites) {
		for ( $i = 0; $i <= $#sites; $i++) {
			last if $sites[$i] eq "none";
			$num = $i+1; # because of numbering
			$scolor = "SCOLOR"."$num";
			$scolor = $value{$scolor} or $scolor = $DEFAULT_COLOR;
			print PMAP_SCRIPT "sites $sites[$i]\n";
			print PMAP_SCRIPT "    color $scolor\n";
			print PMAP_SCRIPT "    size 2\n";
			print PMAP_SCRIPT "    end\n";
			$siteInfo = $SITES{ $sites[$i] }->{descShort};
			$dispMsg .= "<li>Sites map: <b>$siteInfo</b> in $scolor</li>\n";
		} 
	} 

	# close up the scripts
	print PMAP_SCRIPT "end\n";
	close (PMAP_SCRIPT);

	return $dispMsg;

} 

#-------------------------------------------------------------------------
# NAME: create_gif 
#	Creates a gif in file $value{GIF} based on a valid PMAP_SCRIPT. 
# REQUIRES: PMAP_SCRIPT script ALREADY created as $value{PMAP_SCRIPT}, 
#	$value{GIF} and $value{GIFSIZE} already defined by calling subroutine.
#-------------------------------------------------------------------------
sub create_gif { 

	$ENV{WIDTH}=$value{GIFSIZE};
	$ENV{HEIGHT}=$value{GIFSIZE};
 	&system_silent("p.select ppm 1>/dev/null");
	&system_silent("p.map.new input=$TMP/$value{PMAP_SCRIPT} 1> /dev/null");

	#run ppm program to create gif
	&system_errors_logged("$PBM_PATH/ppmquant 256 < $ENV{MAPLP} 2> /dev/null  | $PBM_PATH/ppmtogif -interlace > $TMP/$value{GIF} 2> /dev/null");

}

#-------------------------------------------------------------------------
# NAME: create_display_map
#	Make gif and display it for reclass, combine, buffer operations
# ARGS: $_[0] = any additional text to add to display description
# NOTES: Assumes region settings taken care of by calling function.
#-------------------------------------------------------------------------
sub create_display_map { 
	
	my $addText = $_[0];
	my $baseRaster;

	$value{PMAP_SCRIPT} = "pmap_script.$$";
	$value{DISPLAY_DESC} = "display_desc.$$";
	$value{GIF} = "grasslinks.$$.gif";
	$value{GIFSIZE} = 400;

	open(PMAP_SCRIPT, ">$TMP/$value{PMAP_SCRIPT}") || print STDERR "Unable to open pmap script $TMP/$value{PMAP_SCRIPT} in sub create_display_map\n";
	print PMAP_SCRIPT "raster $value{OUT_RASTER}\n end\n";
	close (PMAP_SCRIPT);

	&create_gif;
	# raster map display description
	if ($value{RASTER} ) {
		$baseRaster = $RASTERS{ $value{RASTER} } ->{descShort}; 
	} else {
		$baseRaster = $RASTERS{ $value{RASTER1} } ->{descShort};
		if ($value{RASTER2}) {
			$baseRaster .= "</b> and <b>";
			$baseRaster .= $RASTERS{ $value{RASTER2} } ->{descShort}; 
		}
	}
	
	# Save display description	
	open(DISPLAY_DESC, ">$TMP/$value{DISPLAY_DESC}");
	print DISPLAY_DESC << "ENDPRINT";
	<b>Map Display Elements:</b><br>
	<ul>
	<li>New Raster map: <b>$value{OUT_RASTER}</b>
	<li>Based on the raster map(s): <b>$baseRaster</b></i>
	<li>Within the Region Setting: <b>$REGIONS{ $value{REGION} }->{descShort}</b>
	<p>
	$addText
	</ul>
ENDPRINT
	close (DISPLAY_DESC);

	# reset the value of $value{RASTER}
	if ( $value{OUT_RASTER} ) {
		$value{RASTER} = $value{OUT_RASTER};
	}

}

##################################################################
# GRASSLinks Legend subroutines
# ALL of these followng subroutines relate to creating legends for
# raster images; based on the raster's color and category files
# written primarily by James Ganong & require his lookup C program
##################################################################

#-------------------------------------------------------------------------
# NAME: create_legend 
# 	Get the categories information and the color information
# 	to create a legend.
# REQUIRES: a current, valid value for $value{RASTER}
# NOTES: THe call to &find_cats populates the array @catlist and the
#	hash %categories that are used by all the legend subroutines.
#-------------------------------------------------------------------------
sub create_legend {

	if (! $value{RASTER}  || $value{RASTER} eq "none" ) {
		print "<I>No valid raster map selected for legend creation<I><p>\n";
		return;
	}

	# get category information for the RASTER
	my $numcats = &find_cats ("$value{RASTER}");

	if ($numcats < 1 ) {
		print "<I>Sorry: not enough categories to create an effective legend</I><p>\n";
	}
	elsif ($numcats > $MAX_LEGEND_CATS) { 
		print "<I>Sorry: too many categories ($numcats) to create an effective legend</I><p>\n";

	} else { 
		# create the legend
		$value{LEGEND_TABLE} = "legend.table.$$";
		@output= `g.findfile element=colr file=$value{RASTER} mapset=\"\""`;
		$mapset_for_colortable=$output[1];
		$mapset_for_colortable =~ s/.*mapset='//s;
		$mapset_for_colortable =~ s/'.*//s;

		&get_colors;
		&make_gifs;
		&write_legend;
		&print_file("$TMP/$value{LEGEND_TABLE}");

		# following subroutine needs to be fixed!
		# &write_ranges_legend;
	}

} # end of create legend 

#-------------------------------------------------------------------------
# NAME: get_colors 
#	Gets color values for each category in a GRASS raster file.
# REQUIRES: The C program lookup installed locally. 
# RETURNS: Implicit. Populates 3 hashes - %red{}, %green{}, and %blue{}
# 	that are keyed based on the category value in $catlist[$i].
#-------------------------------------------------------------------------
sub get_colors {

	foreach $i (@catlist) {
		($red{$i}, $green{$i}, $blue{$i}) = split(/ /, `$PROG_PATH/lookup $value{RASTER} $mapset_for_colortable $i`);
		chomp($blue{$i});
	}

} 

#-------------------------------------------------------------------------
# NAME: make_gifs 
# 	Makes little gif boxes for each legend category.
# REQUIRES: A valid, current @catlist array and associated %red, 
#	%blue, and %green hashes created in &get_colors subroutine.
# CALLS: The subroutine &color2gif to create the actual gif images
#	for each category value.
# RETURNS: Implicit. Populates %gifbox{} hash, keyed by catlist category,
#	with the names of gif files for each category value.
#-------------------------------------------------------------------------
sub make_gifs { 

	my $theGif;

	# Saves and restores current region just once instead
	# of each time a legend gif is created
	&system_errors_logged("g.region save=saved_region");
	&system_errors_logged("g.region rast=$LEGEND_RASTER");

	foreach $i (@catlist) {
		$theGif = "$red{$i}.$green{$i}.$blue{$i}.gif";

		# If it doesn't exist, create it
		unless ( -e "$LGIF_FULLPATH/$theGif" ) {
			&color2gif($red{$i}, $green{$i},$blue{$i});
		}
		$gifbox{$i}="$red{$i}.$green{$i}.$blue{$i}.gif";
	}
	&system_errors_logged("g.region saved_region");	

} 

#-------------------------------------------------------------------------
# NAME: color2gif 
#	Create little square gifs for legend categories
# ARGS: red, green, and blue values
# REQUIRES: $LEGEND_RASTER and $LEGEND_COLR_TABLE defined, as well
#	as some legend default values. 
#-------------------------------------------------------------------------
sub color2gif { 

	my ($R, $G, $B ) = @_;

	$ENV{HEIGHT}= $LEGEND_GIF_PSIZE;
	$ENV{WIDTH}= $LEGEND_GIF_PSIZE;

	my $legend_gif="${R}.${G}.${B}.gif";
	my $theGif = "$LGIF_FULLPATH/$legend_gif";

	# create the tiny gif to represent legend color
	open(COLOR_TABLE, ">$LEGEND_COLR_TABLE") || print STDERR "cant open colortable in color2gif\n";

	print COLOR_TABLE "#1 first color\n";
	print COLOR_TABLE "0 0 0\n";
	print COLOR_TABLE "$R $G $B\n";
	print COLOR_TABLE "0 0 0\n";
	close COLOR_TABLE;

	# create a pmap script for the legend gif	
	my $SCRIPT="$TMP/legend.pmap_script.$$";
	open (SCRIPT, ">$SCRIPT") || print STDERR "GL in color2gif can't open script\n";
	print SCRIPT "raster $LEGEND_RASTER\n";
	print SCRIPT "end\n";
	close SCRIPT;

	&system_silent("p.select ppm 1>/dev/null");
	&system_silent("p.map.new input=$SCRIPT 1> /dev/null");
	&system_errors_logged("$PBM_PATH/ppmquant 256 < $ENV{MAPLP} 2> /dev/null  | $PBM_PATH/ppmtogif -interlace > $LGIF_FULLPATH/$legend_gif 2> /dev/null");

	&flush(STDERR);
}

#-------------------------------------------------------------------------
# NAME: print_legend 
#	Prints out legend in an html table based on values in array
#	@catlist and hashes %category{} and  %gifbox{}.
#-------------------------------------------------------------------------
sub print_legend {

	my $column = 1;

	print '
	<CENTER>
	<TABLE CELLSPACING=4 CELLPADDING=2>
	';

	foreach $i (@catlist) {
		if ($column==1) {
			print "<TR VALIGN=TOP>\n";
		}

		print "<TD><IMG SRC=\"$LGIF_PATH/$gifbox{$i}\"> $category{$i}</TD>\n";
		if ($column++ == $MAX_LEGEND_COLS) {
			print "</TR>\n"; 
			$column=1;
		}
	}

	print '
	</TABLE></CENTER>
	<p>
	';

} 

#-------------------------------------------------------------------------
# NAME: write_legend 
#	Writes legend in a table format to the file $value{LEGEND_TABLE} 
# REQUIRES: current, valid values for array @catlist, hash %gifbox{},
#	and $MAX_LEGEND_COLS variable.
#-------------------------------------------------------------------------
sub write_legend {

	my $column = 1;

	open(TABLE_TMP,"> $TMP/$value{LEGEND_TABLE}") || print STDERR "GL cant create legend table in write_legend\n";

	print TABLE_TMP "<CENTER>\n"; 
	print TABLE_TMP "<TABLE CELLSPACING=4 CELLPADDING=2>\n";

	foreach $i (@catlist) {
		if ($column==1) {
			print TABLE_TMP "<TR VALIGN=TOP>\n";
		}
		print TABLE_TMP "<TD> <IMG SRC=\"$LGIF_PATH/$gifbox{$i}\"> $category{$i}</TD>\n";
		if ($column++ == $MAX_LEGEND_COLS) {
			print TABLE_TMP "</TR>\n"; $column=1 ;
		}
	}
	print TABLE_TMP "</TABLE>\n";
	print TABLE_TMP "</CENTER><p>\n";
	close (TABLE_TMP);

	&flush(STDERR);

} 

#-------------------------------------------------------------------------
# NAME: write_ranges_legend 
#	This is a DRAFT subroutine that is not working and not used!
# 	Should be fixed, important	
#-------------------------------------------------------------------------
sub write_ranges_legend {
	#$colr = `g.filename element=colr file=$value{RASTER} mapset=""`;
	$colr =~ s/file=//;
	$colr =~ s/'//g;
	open (COLOR, "$colr") || die "Cannot open color file $colr";
	$maxranges=24;

	if ( <COLOR> =~ /%/ ) {
		# new style color table, must count ranges
		while (<COLOR>) {
			$number_of_fields = split;	
			if ( $number_of_fields != 2 ) {
				# if not two fields it is not a range
				next;
			}
			$first_field = @_[0];
			$second_field = @_[1];
			$number_of_fields = split(/:/,$first_field);
			$start_range=@_[0];
			$start_red=@_[1];
			if ($number_of_fields == 2) {
				$start_green = $start_red;
				$start_blue = $start_red;
			} else {
				$start_green=@_[2];
				$start_blue=@_[3];
			}

			$number_of_fields = split(/:/,$second_field);
			$end_range=@_[0];
			$end_red=@_[1];
			if ($number_of_fields == 2) {
				$end_green = $end_red;
				$end_blue = $end_red;
			} else {
				$end_green=@_[2];
				$end_blue=@_[3];
			}
			
			if ( $start_range == $end_range ) {
				# we don't want ranges of only one cat
				next;
			}

			if ( $totalranges++ > $maxranges ) {
		print "too many numeric ranges to create an effective ranges legend\n";
				return;
			} else {
				push (@ranges, "$start_range:$start_red:$start_green:$start_blue $end_range:$end_red:$end_green:$end_blue" );
				push (@rangesstack, "$start_range:$start_red:$start_green:$start_blue $end_range:$end_red:$end_green:$end_blue" );
				push (@start_cats, $start_range);
				push (@end_cats, $end_range);
				$filename="$start_range:$start_red:$start_green:$start_blue-$end_range:$end_red:$end_green:$end_blue" ;
				push(@filenames, $filename);
			}
		} #end of while COLOR?
			
		if ( $totalranges == 0 ) {
		return;
		}
	$column = 1;
	print "<TABLE>\n";
	foreach $i (@ranges) {
		$args = pop(@rangesstack);
		$start = pop(@start_cats);
		$end = pop(@end_cats);
		$file = pop(@filenames);
		$ENV{'GIF'}=$file;
		&system_errors_logged("$ENV{PROG_PATH}/ramp2gif $args");
		if ($column==1) {print "<TR VALIGN=TOP>\n";};
		print "<TD> <IMG SRC=\"$thumbnail_url_prefix/$file\"> $start-$end </TD>\n";
		if ($column++ == $MAX_LEGEND_COLS) {print "</TR>\n"; $column=1;}
	}
	print "</TABLE>\n";
	} #end of if
} #end of subroutine write_ranges_legend

