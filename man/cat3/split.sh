


split.sh <scripts>   GRASS Reference Manual    <scripts> split.sh



NAME
     split.sh -	Divides	the graphics monitor into two frames and
     then displays two maps in these frames.
     (GRASS Display Program)

SYNOPSIS
     split.sh mapname mapname [cmd=GRASS_command]
     [cmd2=GRASS_command] [view=horiz]

DESCRIPTION
     split.sh is a Bourne shell	(sh(1))	script that clears the
     entire graphics screen and	divides	it into	two display
     frames.  Map layers are then displayed in each of the two
     frames.  This command is very useful for visually comparing
     maps (raster, vector, and 3-d views) and can be used by
     other GRASS shell macros.	It is also useful for creating
     demos.  Program parameters	are given below.

     Parameters:

     view=horiz	       The graphics screen can be split	either
		       horizontally or vertically.  The	default
		       view splits the screen into two frames,
		       one on the left and one on the right (a
		       vertical	split).	 Some maps ( 3-d views)
		       are better represented with more	width
		       then height (horizontal split).	The first
		       map name	listed on the command line will
		       be displayed in the top or left window
		       (depending on whether the screen	was split
		       horizontally or vertically), and	The
		       second map will be displayed in the bottom
		       or right	window.

     cmd=GRASS_command The GRASS command used to display the
		       named mapnames.	If no command is
		       specified by the	user, d.rast is	used by
		       default.	 However, any GRASS display
		       command (e.g., d.3d, d.vect, etc...)  can
		       be entered.

     cmd2=GRASS_command
		       This command will be used to display map
		       data in the second frame	only.

		       If the user fails to specify the	values of
		       both cmd	and cmd2fR, split.sh will use the
		       default command (d.rast)	to display user-
		       specified map layer names in both frames.
		       If the user specifies only the value of
		       cmd on the command line,	then that command
		       will be executed	for both frames.  If the



GRASS 4.2		Baylor University			1






split.sh <scripts>   GRASS Reference Manual    <scripts> split.sh



		       user specifies the values of both cmd and
		       cmd2 on the command line, the cmd command
		       will be executed	in frame 1 and the cmd2
		       command will be executed	in frame 2.

EXAMPLES
     split.sh  soils  vegcover

     split.sh  soils  cmd2=d.legend "soils red"

     split.sh  elevation  vegcover  cmd=d.3d  view=horiz

NOTES
     split.sh leaves the frame that the	last map was drawn in as
     the active	frame.	The order in which the options (cmd,
     cmd2, view) are placed on the command line	doesn't	matter,
     but the order is important	for the	map names.

FILES
     This program is simply a shell script.  Users are encouraged
     to	make their own shell scripts using similar techniques.
     See $GISBASE/scripts/split.sh.

SEE ALSO
     d.3d, d.frame, d.rast, d.sites, d.vect

AUTHOR
     Michael Higgins, U.S. Army	Construction Engineering Research
     Laboratory


























GRASS 4.2		Baylor University			2



