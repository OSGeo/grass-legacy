


v.out.arc <alpha>    GRASS Reference Manual	<alpha>	v.out.arc



NAME
     v.out.arc - Converts GRASS	vector files to	ARC/INFO's
     "Generate"	file format.
     (GRASS Vector Data	Export Program)

SYNOPSIS
     v.out.arc
     v.out.arc help
     v.out.arc type=name vect=name arc_prefix=name

DESCRIPTION
     v.out.arc is a GRASS data export program that converts files
     in	GRASS vector format to ARC/INFO's "Generate" file format.
     The companion program v.in.arc imports data in ARC/INFO's
     "Generate"	format and converts them to GRASS vector format.

     This program can be run either non-interactively or
     interactively.  The program will be run non-interactively if
     the user specifies	parameter values on the	command	line
     using the following format:

	  v.out.arc type=name vect=name	arc_prefix=name

     Alternately, the user can simply type:

	  v.out.arc

     on	the command line;  in this case, the program will prompt
     the user for parameter values.


     Parameters:

     type=name	       Coverage	(feature) type.
		       Options:	 polygon, line

     vect=name	       The name	of a GRASS vector file to be
		       converted to ARC/INFO format.

     arc_prefix=name   A prefix	to be assigned to the ARC/INFO-
		       format files output by v.out.arc.



INTERACTIVE MODE: USER PROMPTS
     v.out.arc will prompt the user to enter the name of a GRASS
     vector file to be exported	to ARC/INFO and	for a filename
     prefix to be used in naming the files created by the
     program.

     A GRASS vector file to be exported	to ARC/INFO must either
     contain only linear features (i.e., have only line	coverage)



GRASS 4.2		Baylor University			1






v.out.arc <alpha>    GRASS Reference Manual	<alpha>	v.out.arc



     or	contain	only area edge features	(i.e., have only polygon
     coverage).	 v.out.arc will	begin by asking	the user which
     type of coverage (line or polygon)	is to be imported:

	   COVERAGE TYPE
	  Enter	"polygon" or "line"
	  Hit RETURN to	cancel request
	  >

     The program then prompts the user for the name of the GRASS
     vector file to be converted to ARC/INFO format:

	   VECTOR (DIGIT) FILENAME
	  Enter	'list' for a list of existing binary vector files
	  Hit RETURN to	cancel request
	  >

     Next, the user is asked for a file-name prefix to be used in
     naming the	output ARC/INFO	Generate format	files:

	   ARC/INFO (GENERATE) FILENAME	PREFIX
	  Hit RETURN to	cancel request
	  >

     The filename prefix will be used to name the various files
     that will be created for export to	ARC/INFO.  When	labeled
     polygon coverage data are exported, three such files will be
     created:  a lines file with the suffix .lin, a label-points
     file with the suffix .lab,	and a label-text file with the
     suffix .txt.  When	line coverage data are exported, two such
     files will	be created:  a lines file with the suffix .lin,
     and a label-text file with	the suffix .txt.  Export of
     unlabeled polygon or line coverage	data will result in
     creation of a lines file (.lin suffix) only.  See the DATA
     FILE FORMATS section for more information on these	files.



EXAMPLE
     Linear features and polygon data are made up of the series
     of	arcs (aka, vectors) outlining their perimeters.	 The
     ARC/INFO Users' Guide, in its discussion of the Ungenerate
     command, explains how line	and polygon coverage data can be
     created from files	(like prefix.lin and prefix.pol)
     containing	the geographic coordinates of these arcs, and
     from files	(like prefix.lab) containing the geographic
     coordinates of label-points.  Below is an example which
     illustrates the creation, within ARC/INFO,	of a polygon
     coverage data file	(named soils) from the files soils.pol
     and soils.lab.

	Arc: GENERATE SOILS



GRASS 4.2		Baylor University			2






v.out.arc <alpha>    GRASS Reference Manual	<alpha>	v.out.arc



	Generate: INPUT	soils.pol
	Generate: LINES
	Generating lines ...
	Generate: INPUT	soils.lab
	Generate: POINTS
	Generating points ...
	Generate: QUIT
	Arc: _

     The above example would create a polygon coverage data file
     named soils with label-points. The	label-points would have
     ID	numbers	that correspond	to the GRASS category values for
     the polygons in the coverage.  The	INFO portion of	ARC/INFO
     can be used to associate these label-point	ID numbers with
     descriptive text from the soils.txt file.


DATA FILE FORMATS
     LINES FILE, also know as prefix.lin or prefix.pol file:
     This text file is a "Generate" format lines file.	The lines
     option of the ARC/INFO Generate command can be used to read
     this file into ARC/INFO. Each line	in the file has	a unique
     line-ID number.

	  101
	  223343.62 218923.15
	  223343.62 222271.06
	  259565.31 222271.06
	  259565.31 195577.37
	  END
	  102
	  237862.53 203392.37
	  244970.75 203744.28
	  253137.66 195577.37
	  259565.31 195577.37
	  END
	  103
	  237862.53 203392.37
	  237862.53 203744.28
	  223343.62 218392.37
	  END
	  104
	  239072.44 186200.56
	  237862.53 187410.50
	  237862.53 203392.37
	  END
	  END

     LABEL-POINTS FILE,	also known as prefix.lab file:
     This text file will be created by v.out.arc if the	vector
     file being	exported represents a polygon coverage.
     prefix.lab	consists of a list of label-point (x,y)



GRASS 4.2		Baylor University			3






v.out.arc <alpha>    GRASS Reference Manual	<alpha>	v.out.arc



     coordinates, each with a unique label-point ID number.

	  1 242777.81 211533.09
	  2 243458.37 199282.28
	  3 243458.37 195199.28

     LABEL-TEXT	FILE, also known as prefix.txt file:
     In	the case of polygon coverage data, this	file associates
     an	integer	category value and a category label ("attribute")
     text string (containing no	spaces)	with each label-point ID
     number.  In the case of line coverage data, this file
     associates	an integer category value and an attribute text
     string with each line-ID number.

     The first column is the row number	(which is arbitrary), the
     second column contains the	category value,	the third column
     holds the line or label-point ID number, and the fourth
     column contains the attribute text	string.

	  1 4 1	Coniferous
	  2 5 2	Deciduous
	  3 2 3	Rangeland

SEE ALSO
     v.in.arc, v.support

AUTHOR
     Dave Johnson
     DBA Systems, Inc.
     10560 Arrowhead Drive
     Fairfax, Virginia 22030

NOTICE
     This program is part of the alpha section of the GRASS
     distribution.  Unlike the code in the main	section	of GRASS,
     the alpha code has	not yet	been fully tested for one release
     cycle.


















GRASS 4.2		Baylor University			4



