


i.tape.mss <main>    GRASS Reference Manual	<main> i.tape.mss



NAME
     i.tape.mss	- An imagery function that extracts Multispectral
     Scanner (MSS) imagery data	from half-inch tape.
     (GRASS Image Processing Program)

SYNOPSIS
     i.tape.mss

DESCRIPTION
     i.tape.mss	is a program that extracts Multispectral Scanner
     (MSS) imagery data	from tape.

     This program must be run in a LOCATION_NAME with a	x,y
     coordinate	system (i.e., a	coordinate system with projection
     0).  For further information regarding this LOCATION_NAME
     refer to the manual entry for imagery.

     The first prompt in i.tape.mss asks the user for the tape
     device name.  This	is sometimes  /dev/rmt0	(for a half-inch
     tape with a tape density of 1600),	but this varies	with each
     machine.

     The next prompt is:

     Please mount and load tape, then hit RETURN -->


IMAGE IDENTIFICATION MENU
     The first menu in the program asks	the user for information
     about the data.

	       please enter the	following information

	 Tape Identification:				  __

	 Image Description:				  __

	 Title for the Extracted Raster	(Cell) Files:	  __

	 AFTER COMPLETING ALL ANSWERS, HIT <ESC> TO CONTINUE
		       (OR <Ctrl-C> TO CANCEL)


     This program automatically	enters the scene ID number and
     the date of the image into	the field for Tape
     Identification.  The sun angles are automatically entered
     into the field for	Image Description.








GRASS 4.2		Baylor University			1






i.tape.mss <main>    GRASS Reference Manual	<main> i.tape.mss



     The second	menu is:

			 MSS TAPE EXTRACTION
     please select the desired tape window (geographic region definition) to extract

		      first row: _______(1-2984)
		      last row:	_______(1-2984)

		      first col: _______(1-3548)
		      last col:	_______(1-3548)



	 AFTER COMPLETING ALL ANSWERS, HIT <ESC> TO CONTINUE
		       (OR <Ctrl-C> TO CANCEL)

     The numbers in parentheses	are the	total number of	rows and
     columns on	the tape including filler (zeros).  This
     information and additional	information can	also be	obtained
     by	running	the GRASS program i.tape.mss.h which reads the
     header information	on an MSS tape.	 Any subset of the image
     on	tape may be extracted.	For a discussion of row	and
     column extraction see the subheading titled ROW AND COLUMN
     EXTRACTION	below.

     The next menu is:

	   please make an x by the bands you want extracted

			       _____ 1
			       _____ 2
			       _____ 3
			       _____ 4

	 AFTER COMPLETING ALL ANSWERS, HIT <ESC> TO CONTINUE
		       (OR <Ctrl-C> TO CANCEL)

     MSS imagery has 4 bands, but the user may want to extract
     only a subset of these bands.  See	the subheading in this
     entry titled ROW AND COLUMN EXTRACTION.

     The user then is asked to enter the prefix/group for the
     band files	to be created.	This name will precede each band
     file extracted into GRASS.	 For example, if three bands are
     extracted the following three (raster) band files will
     result:

			  prefixname.1
			  prefixname.2
			  prefixname.3

     Whatever prefixname is specified will also	automatically



GRASS 4.2		Baylor University			2






i.tape.mss <main>    GRASS Reference Manual	<main> i.tape.mss



     become the	name for the imagery group file	being created.
     Each image	(i.e., each run	of i.tape.mss) should be given a
     unique prefix/group name.

     The extraction process will begin by first	skipping the
     number of specified files,	advancing to the starting row,
     and then reading the tape.	 The percent completion	of the
     extraction	is displayed on	the screen. If more than one tape
     is	required to store the image, the program will pause and
     inform the	user to	mount the next tape.

     The extracted (raster) band files will be listed as raster
     map layers	available in the current MAPSET	and may	be
     displayed using the GRASS commands	d.display, d.rast or
     i.points.

NOTES
     After extracting an image from tape, the geographic region
     definition	in the x,y coordinate LOCATION_NAME will be set
     based upon	the extracted rows and columns from the	tape.
     The relationship between the image	rows and columns and the
     geographic	coordinates of the region is discussed in the
     manual entry for imagery.

     This program is interactive and requires no command line
     arguments.

ROW AND	COLUMN EXTRACTION
     The display options in GRASS allow	the user to locate rows
     and columns on the	digital	image.	If enough disk space is
     available,	one band of an entire image, or	one band of a
     portion of	an image known to contain the area of interest,
     can be extracted and displayed.  The measurements option in
     d.display,	or d.where (following the use of d.rast) will
     echo x and	y coordinates to the screen.  (These coordinates
     will display negative numbers in the north-south direction,
     but ignoring the negative sign will yield the row number.)
     See the imagery manual entry for further explanation.

     If	a photograph of	the digital image is available,	the rows
     and columns to be extracted can be	determined from	it by
     associating inches	with the total number of known rows and
     columns in	the scene.  For	example, if the	total length of
     the photograph is 12 inches, the total number of rows on the
     tape is 2000, and the northwest corner of the area	of
     interest begins 2 inches from the top of the photo, then:

		    12"	/ 2000 rows = 2" / x rows
			     x = 333.333

     The northwest corner of the area of interest starts at row
     333.  The starting	row, ending row, starting column, and



GRASS 4.2		Baylor University			3






i.tape.mss <main>    GRASS Reference Manual	<main> i.tape.mss



     ending column can be calculated in	this manner.

SEE ALSO
     GRASS Tutorial: Image Processing

     d.display,	d.rast,	d.where, i.group, i.points, i.tape.mss.h,
     i.tape.other, i.tape.tm, imagery

AUTHOR
     Michael Shapiro, U.S. Army	Construction Engineering Research
     Laboratory












































GRASS 4.2		Baylor University			4



