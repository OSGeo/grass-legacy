


i.tape.mss(1)        GRASS Reference Manual         i.tape.mss(1)



NAME
     i.tape.mss -  An imagery function that extracts
                   Multispectral Scanner Imagery from half inch
                   tape
                   (GRASS Imagery Tool)

SYNOPSIS
     i.tape.mss

DESCRIPTION
     i.tape.mss is a program that extracts Multispectral Scanner
     (MSS) Imagery from tape.

     This program must be run in a LOCATION with a x,y coordinate
     system (i.e., a coordinate system with projection 0).  For
     further information regarding this LOCATION refer to the
     manual entry imagery[3].

     The first prompt in i.tape.mss asks the user for the tape
     device name.  This is sometimes  /dev/rmt0 (for a density of
     1600), but this varies with each machine.

     The next prompt is:

     Please mount and load tape, then hit RETURN -->


IMAGE IDENTIFICATION MENU
     The first menu in the program asks the user for information
     about the data.

               please enter the following information

         Tape Identification:                             __

         Image Description:                               __

         Title for the Extracted Cell Files:              __

         AFTER COMPLETING ALL ANSWERS, HIT <ESC> TO CONTINUE
                       (OR <Ctrl-C> TO CANCEL)


     This program automatically enters the scene ID number and
     the date of the image into the field for Tape
     Identification.  The sun angles are automatically entered
     into the field for Image Description.








GRASS 3.2                U.S. Army CERL                         1






i.tape.mss(1)        GRASS Reference Manual         i.tape.mss(1)



     The second menu is:

                         MSS TAPE EXTRACTION
           please select the desired tape window to extract

                      first row: _______(1-2984)
                      last row: _______(1-2984)

                      first col: _______(1-3548)
                      last col: _______(1-3548)



         AFTER COMPLETING ALL ANSWERS, HIT <ESC> TO CONTINUE
                       (OR <Ctrl-C> TO CANCEL)

     The numbers in parentheses are the total number of rows and
     columns on the tape including filler (zeros).  This
     information and additional information can also be obtained
     by running the GRASS program i.tape.mss.h which reads the
     header information on an MSS tape.  Any subset of the image
     on tape may be extracted.  For a discussion of row and
     column extraction see the subheading titled ROW AND COLUMN
     EXTRACTION below.

     The next menu is:

           please make an x by the bands you want extracted

                               _____ 1
                               _____ 2
                               _____ 3
                               _____ 4

         AFTER COMPLETING ALL ANSWERS, HIT <ESC> TO CONTINUE
                       (OR <Ctrl-C> TO CANCEL)

     MSS imagery has 4 bands, but the user may want to extract
     only a subset of these bands.  See the subheading in this
     entry titled ROW AND COLUMN EXTRACTION.

     The user then is asked to enter the prefix/group for the
     band cell to be created.  This name will precede each band
     extracted into GRASS.  For example, if 3 bands are extracted
     the following 3 band will result:

                          prefixname.1
                          prefixname.2
                          prefixname.3

     Whatever prefixname is specified will also automatically
     become the name for the imagery group file being created.



GRASS 3.2                U.S. Army CERL                         2






i.tape.mss(1)        GRASS Reference Manual         i.tape.mss(1)



     Each image (i.e., each run of i.tape.mss) should be given a
     unique prefix/group name.

     The extraction process will begin by first skipping the
     number of specified files, advancing to the starting row,
     and then reading the tape.  The percent completion of the
     extraction is displayed on the screen. If more than one tape
     is required to store the image, the program will pause and
     inform the user to mount the next tape.

     The extracted band will be listed as cell available in the
     current MAPSET and may be displayed using the GRASS commands
     display, Dcell or i.points.

NOTES
     After extracting an image from tape, the window in the x,y
     coordinate LOCATION will be set based upon the extracted
     rows and columns from the tape. The relationship between the
     image rows and columns and the window coordinates is
     discussed in the manual entry imagery[3].


ROW AND COLUMN EXTRACTION
     The display options in allow the user to locate rows and
     columns on the digital image.  If enough disk space is
     available, one band of an entire image, or one band of a
     portion of an image known to contain the area of interest,
     can be extracted and displayed.  The measurements option in
     display, or Dwhere (following the use of Dcell) will echo x
     and y coordinates to the screen.  (These coordinates will
     display negative numbers in the north-south direction, but
     ignoring the negative sign will yield the row number.)  See
     manual entry imagery[3] for further explanation.

     If a photograph of the digital image is available, the rows
     and columns to be extracted can be determined from it by
     associating inches with the total number of known rows and
     columns in the scene.  For example, if the total length of
     the photograph is 12 inches, the total number of rows on the
     tape is 2000, and the northwest corner of the area of
     interest begins 2 inches from the top of the photo, then:

                    12" / 2000 rows = 2" / x rows
                             x = 333.333

     The northwest corner of the area of interest starts at row
     333.  The starting row, ending row, starting column, and
     ending column can be calculated in this manner.


SEE ALSO
     GRASS Tutorial: Image Processing



GRASS 3.2                U.S. Army CERL                         3






i.tape.mss(1)        GRASS Reference Manual         i.tape.mss(1)



     display[1]
     Dcell[2D], Dwhere[2D]
     i.group[1], i.tape.mss.h[1]
     imagery[3]

AUTHOR
     Michael Shapiro, U.S. Army Construction Engineering Research
     Laboratory















































GRASS 3.2                U.S. Army CERL                         4



