


grass.to.arc(1G)     GRASS Reference Manual      grass.to.arc(1G)



NAME
     grass.to.arc - export GRASS vector files to ARC/INFO
     (Not available through grass3 menu)

SYNOPSIS
     grass.to.arc

DESCRIPTION
     grass.to.arc will export GRASS vectors files to ARC/INFO
     Generate format.

USER PROMPTS
     The vector file export program, grass.to.arc, will prompt
     you to enter the name of a GRASS vector file to be exported
     to ARC/INFO and for a file-name prefix to be used in naming
     the files created by the program.

     A GRASS vector file to be exported to ARC/INFO must be
     either a line coverage (must contain only lines) or a
     polygon coverage (must contain only area edges).  The
     grass.to.arc program will start out by asking you which type
     of coverage is to be imported, as follows:

           COVERAGE TYPE
          Enter "polygon" or "line"
          Hit RETURN to cancel request
          >

     You  will then be prompted for the name of the file for
     export to ARC/INFO as follows:

           VECTOR (DIGIT) FILENAME
          Enter 'list' for a list of existing binary vector files
          Hit RETURN to cancel request
          >

     The grass.to.arc program will then prompt you for a file-
     name prefix for the ARC/INFO Generate format files to be
     created as follows:

           ARC/INFO (GENERATE) FILENAME PREFIX
          Hit RETURN to cancel request
          >

     The file-name prefix will be used to name the various files
     that will be created for export to ARC/INFO.  In the case of
     a labelled polygon coverage, the following three files will
     be created: a lines file with the suffix .lin, a label-
     points file with the suffix .lab, and a label-text file with
     the suffix .txt.  In the case of a line coverage the
     following two files will be created: a lines file with the
     suffix or line coverage will result in a lines file (.lin



GRASS 3.2                U.S. Army CERL                         1






grass.to.arc(1G)     GRASS Reference Manual      grass.to.arc(1G)



     suffux) only.  See the DATA FILE FORMATS section for more
     information on these files.


EXAMPLE
     The description of the Generate command in the ARC/INFO
     Users Guide explains how to create coverages from files
     containing line coordinates, such as prefix.lin and
     prefix.pol, and files containing label-points, such as
     prefix.lab.  Following is an example of the creation of a
     polygon coverage named soils within ARC/INFO from files
     soils.pol and soils.lab:

        Arc: GENERATE SOILS
        Generate: INPUT soils.pol
        Generate: LINES
        Generating lines ...
        Generate: INPUT soils.lab
        Generate: POINTS
        Generating points ...
        Generate: QUIT
        Arc: _

     This would create a polygon coverage named soils with
     label-points. The label-points would have ID numbers that
     correspond to the GRASS attribute values for the polygons in
     the coverage.  The INFO portion of ARC/INFO can be used to
     associate these label-point ID numbers with descriptive text
     from the a soils.txt file.


DATA FILE FORMATS
     LINES FILE, also know as prefix.lin or prefix.pol file: This
     text file is a Generate format lines file.  The lines option
     of the ARC/INFO Generate command can be used to read this
     file into ARC/INFO.  Each line in the file has a unique
     line-ID number.

101
                                                                      223343.62   218923.15
                                                                      223343.62   222271.06
                                                                      259565.31   222271.06
                                                                      259565.31   195577.37
END
102
                                                                      237862.53   203392.37
                                                                      244970.75   203744.28
                                                                      253137.66   195577.37
                                                                      259565.31   195577.37
END
103
                                                                      237862.53   203392.37



GRASS 3.2                U.S. Army CERL                         2






grass.to.arc(1G)     GRASS Reference Manual      grass.to.arc(1G)



                                                                      237862.53   203744.28
                                                                      223343.62   218392.37
END
104
                                                                      239072.44   186200.56
                                                                      237862.53   187410.50
                                                                      237862.53   203392.37
END
END

LABEL-POINTS FILE, also known as prefix.lab
file:                                           This text file
will be created
by
grass.to.arc if the vector file being exported represents a
polygon                                      coverage.
prefix.lab consists of a list of label-point coordinates (x,y),
each             with a unique label-point ID number.


center;
l l l.
1                                                                                 242777.81
2                                                                                 243458.37
3                                                                                 243458.37

     LABEL-TEXT FILE, also known as prefix.txt file: In the case
     of a polygon coverage, this file associates an integer
     category number and an attribute text string (with no
     spaces) with each label-point ID number.  In the case of a
     line coverage, this file associates an integer category
     number and an attribute text string with each line-ID
     number.

     The first column is the row number (which is arbitrary), the
     second column contains the category number, the third column
     is the line or label-point ID number, and the forth column
     is the attribute text string.

                        1   4   1   Coniferous
                        2   5   2    Desiduous
                        3   2   3    Rangeland


AUTHOR
     Dave Johnson
     DBA Systems, Inc.
     10560 Arrowhead Drive
     Fairfax, Virginia 22030






GRASS 3.2                U.S. Army CERL                         3



