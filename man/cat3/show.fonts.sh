


show.fonts.sh <scriptsGRASS Reference Manu<scripts> show.fonts.sh



NAME
     show.fonts.sh - Displays and names	available font types in
     the active	display	frame on the graphics monitor.
     (GRASS Shell Script)

SYNOPSIS
     show.fonts.sh

DESCRIPTION
     show.fonts.sh is a	UNIX Bourne shell macro	which runs the
     d.erase -a	command, then names and	displays the font types
     that can be selected using	d.font.	 This macro also runs the
     GRASS commands d.font and d.text.	See the	manual entry for
     d.font for	instructions on	choosing a font	type.

     No	program	arguments are required to run this program.

BUGS
     The font is set to	romans (Roman simplex) after running
     show.fonts.sh. There is no	mechanism to query the current
     font, so there is no way to automatically restore the font.
     The user will have	to reset the font type using d.font if
     romans is not desired.

FILES
     This program is simply a shell script stored under	the
     $GISBASE/scripts directory.  Users	are encouraged to examine
     the shell script programs stored here and to produce others
     for their own use.

SEE ALSO
     d.display,	d.erase, d.font, grass.logo.sh,	d.label,
     d.legend, d.paint.labels, d.text, d.title

AUTHOR
     James Westervelt, U.S. Army Construction Engineering
     Research Laboratory


















GRASS 4.2		Baylor University			1



