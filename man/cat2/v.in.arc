


vv..iinn..aarrcc <alpha>      GRASS Reference Manual     <alpha> vv..iinn..aarrcc



NNAAMMEE
     _v_._i_n_._a_r_c - Converts data in ARC/INFO format to GRASS's
     vector format, and stores output in the user's current GRASS
     mapset.
     _(_G_R_A_S_S _V_e_c_t_o_r _D_a_t_a _I_m_p_o_r_t _P_r_o_g_r_a_m_)

SSYYNNOOPPSSIISS
     vv..iinn..aarrcc
     vv..iinn..aarrcc hheellpp
     vv..iinn..aarrcc [--nn] ttyyppee==_n_a_m_e lliinneess__iinn==_n_a_m_e [ppooiinnttss__iinn==_n_a_m_e]
     [tteexxtt__iinn==_n_a_m_e]
         vveeccttoorr__oouutt==_n_a_m_e [iiddccooll==_v_a_l_u_e] [ccaattccooll==_v_a_l_u_e]
     [aattttccooll==_v_a_l_u_e]

DDEESSCCRRIIPPTTIIOONN
     The user may wish to use GRASS programs on data files that
     were created by other GISs.  To do this, the user must first
     convert data files in these systems' formats to GRASS's file
     format.  Bringing data from other systems into GRASS is
     termed file _i_m_p_o_r_t.  Sending GRASS data files out into other
     systems' formats is termed file _e_x_p_o_r_t.

     A variety of GRASS programs exist to import and export data
     to and from GRASS.  The _v_._i_n_._a_r_c program will convert vector
     data in ARC/INFO's "Generate" format to GRASS's vector file
     format, and bring it into the user's current GRASS mapset.
     The files to be imported to GRASS must first have been
     exported from ARC/INFO using the ARC/INFO _U_n_g_e_n_e_r_a_t_e
     command, and may represent either linear features ("line
     coverage") or areal features ("polygon coverage").  The
     _A_R_C_/_I_N_F_O _U_s_e_r_'_s _G_u_i_d_e describes how files containing linear
     and polygonal features can be exported from ARC/INFO, in a
     section detailing the use of the _U_n_g_e_n_e_r_a_t_e command.

     NNoottee::  The ARC coverage must be set to single precision
     before running _U_n_g_e_n_e_r_a_t_e.  If it is not, first copy it to
     another coverage that is set to single precision, then run
     _U_n_g_e_n_e_r_a_t_e.

     Once converted with the ARC/INFO _U_n_g_e_n_e_r_a_t_e command, the
     files to be imported into GRASS must be placed in a
     directory named _a_r_c in the user's current mapset. If the _a_r_c
     directory does not exist, it must be created (e.g., with the
     command _m_k_d_i_r _$_L_O_C_A_T_I_O_N_/_a_r_c) before copying the ARC-INFO
     files to be converted into it.

     (_v_._i_n_._a_r_c can be used to convert ARC-INFO data from other
     mapsets as well, since the program searches for the
     specified input file names in the _a_r_c directories, if any
     exist, in the mapsets in the user's current mapset search
     path.)




GRASS 4.1                 U.S. Army CERL                        1






vv..iinn..aarrcc <alpha>      GRASS Reference Manual     <alpha> vv..iinn..aarrcc



     NNoottee::  To use for text attributes, numeric fields with
     values >999 may contain no commas or TABs.  Also, the first
     record must have all fields filled.

OOPPTTIIOONNSS
     Program parameters and the flag have the following meanings.

     FFllaagg::

     --nn                Neatline.  Vectors representing a box
                       (neatline) around the input vector data
                       will be inserted into the output GRASS
                       vector file.

     PPaarraammeetteerrss::

     ttyyppee==_n_a_m_e         Coverage type.  Either polygon, or line.
                       Options:  polygon, line

     lliinneess__iinn==_n_a_m_e     ARC/INFO ungenerate lines file; ungenerate
                       format input file containing line or
                       polygon coordinates.

     ppooiinnttss__iinn==_n_a_m_e    ARC/INFO ungenerate label-points file;
                       ungenerate format input file containing
                       label-point coordinates; only applies to
                       'polygon' type data.

     tteexxtt__iinn==_n_a_m_e      ARC/INFO ungenerate label-text file;
                       ungenerate format input file containing
                       category numbers and (optionally)
                       attribute text.

     vveeccttoorr__oouutt==_n_a_m_e   Resultant GRASS vector output file.

     iiddccooll==_v_a_l_u_e       ID Number column in label-text file.
                       Number of label-text column containing
                       line-ID numbers.

     ccaattccooll==_v_a_l_u_e      GRASS category column in label-text file.
                       Number of label-text column containing
                       category values.

     aattttccooll==_v_a_l_u_e      GRASS attribute column in label-text file.
                       Number of label-text column containing
                       attribute text.

     This program can be run either non-interactively or
     interactively.  The program will run non-interactively if
     the user specifies the (optional) flag setting and needed
     parameter values on the command line, using the form:




GRASS 4.1                 U.S. Army CERL                        2






vv..iinn..aarrcc <alpha>      GRASS Reference Manual     <alpha> vv..iinn..aarrcc



          vv..iinn..aarrcc [--nn] ttyyppee==_n_a_m_e lliinneess__iinn==_n_a_m_e [ppooiinnttss__iinn==_n_a_m_e]
          [tteexxtt__iinn==_n_a_m_e]
              vveeccttoorr__oouutt==_n_a_m_e [iiddccooll==_v_a_l_u_e] [ccaattccooll==_v_a_l_u_e]
          [aattttccooll==_v_a_l_u_e]

     Alternately, the user can type:

          vv..iinn..aarrcc

     on the command line without program arguments;  in this
     case, the program will prompt the user for the flag setting
     and parameter values in the manner shown below.


     In ARC/INFO, three files are used to store polygon data:
     1) a _l_i_n_e_s _f_i_l_e, which contains coordinates of all the area
     edge lines;
     2) a _l_a_b_e_l_-_p_o_i_n_t _f_i_l_e, which contains coordinates of label-
     points (each of which has associated with it a unique label-
     point ID number).  One label-point is associated with each
     polygon defined in the _l_i_n_e_s _f_i_l_e;
     3) a _l_a_b_e_l_-_t_e_x_t _f_i_l_e, which associates each label-point ID
     number with a category number and category ("attribute")
     text.

     Linear feature data are stored in two files:
     1) a _l_i_n_e_s _f_i_l_e, which contains geographic coordinates
     defining lines, each with a line-ID number; and
     2) a _l_a_b_e_l_-_t_e_x_t _f_i_l_e, which associates each line-ID number
     with a category number and attribute text.

     These data files are described in further detail below,
     under the DATA FILE FORMATS section.

IINNTTEERRAACCTTIIVVEE MMOODDEE
     The program will prompt the user for the flag setting and
     parameter values if the user does not specify these on the
     command line.  First, the user will be asked to assign a
     name to the vector file to store program output:

           VECTOR (DIGIT) FILENAME
          Enter 'list' for a list of existing binary vector files
          Hit RETURN to cancel request
          >

     Next, the user is asked to specify the COVERAGE (feature)
     type to be imported into GRASS.  Valid coverage types are
     ppoollyyggoonn and lliinnee.

           COVERAGE TYPE
          Enter "polygon" or "line"
          Hit RETURN to cancel request



GRASS 4.1                 U.S. Army CERL                        3






vv..iinn..aarrcc <alpha>      GRASS Reference Manual     <alpha> vv..iinn..aarrcc



          >


     IMPORTING A POLYGON COVERAGE

     If the user chooses POLYGON coverage, he is asked if he
     wishes a neatline placed around his data.  (The existence of
     neatlines in the output file can facilitate subsequent
     patching of data files.)

           NEATLINE
          Do you want a neatline ?
          Enter "yes" or "no"
          >

     If the user types yyeess, vectors that box the data will be
     inserted into the GRASS vector output file (_v_e_c_t_o_r___o_u_t);
     otherwise, no neatline will be inserted into the output
     file.

     Next, the user is prompted for the name of an existing
     lines-file containing the geographic coordinates of the arcs
     forming polygon perimeters.  The lines-file is created with
     the ARC/INFO _U_n_g_e_n_e_r_a_t_e _L_I_N_E_S option, and is in the same
     format at the _p_r_e_f_i_x_._p_o_l file created by the _v_._o_u_t_._a_r_c
     program.  The user sees the following prompt:

           LINES FILENAME
          Enter name of the file created with the LINES
          option of the ARC/INFO Ungenerate command.
          Hit RETURN to cancel request
          >

     The next prompt for coverage type "polygon" asks for the
     name of an existing label-points file.  The label-points
     file is created with the _U_n_g_e_n_e_r_a_t_e _P_O_I_N_T_S option, and is in
     the same format as the _p_r_e_f_i_x_._l_a_b file created by the
     _v_._o_u_t_._a_r_c program.  The user sees the following prompt:

           LABEL-POINTS FILENAME
          Enter name of file created with the POINTS
          option of the ARC/INFO Ungenerate command.
          Hit RETURN if there is no such file
          >

     Finally, the program asks the user for the name of an
     existing label-text file.  This file associates each label-
     point ID number with a text string.  It is in the same
     format as the _p_r_e_f_i_x_._t_x_t file created by the _v_._o_u_t_._a_r_c
     program.

           LABEL-TEXT FILENAME



GRASS 4.1                 U.S. Army CERL                        4






vv..iinn..aarrcc <alpha>      GRASS Reference Manual     <alpha> vv..iinn..aarrcc



          Enter the name of a file that associates
          label-point ID numbers with text label strings
          Hit RETURN if there is no such file
          >

     _v_._i_n_._a_r_c then scans the label-text file to find the numbers
     of lines and columns, the column headers (if any), and the
     first three lines of actual data in the file.  It displays
     this information to standard output to help the user
     determine which columns will hold the ID, Category value,
     and Attribute text data in the new vector output file.  A
     sample of the program's output is shown below:

        The LABEL-TEXT file has been scanned. There are 132
        lines in the file and 8 columns in the file

        Column headers of the LABEL-TEXT file:
          rec# AREA PERIMETER SOILS# SOILS-ID SOIL-CODE DRAIN_CODE TXTUR-CODE

        Here are the first three lines :
             1   -2.30228E+07   19399.848     1      0      0      0      0
             2     81079.875    1678.826     2      1     15      3      3
             3    955952.500   10229.637     3      2     19      8      8

     The column of category values must contain only integer
     values.  The attribute text column can contain a floating
     point number, an integer, or a word (text string).

     Finally, the user is prompted to enter line ID, category
     value, and attribute text column numbers.


          Enter the number of the column that should be used
          for line IDs (probably the column with -ID) :

          Enter the number of the column that is to be used
          for GRASS category values:


          Enter the number of the column that should be used
          for GRASS attribute text:

     Once these column numbers have been entered, _v_._i_n_._a_r_c will
     begin converting the ARC/INFO "Generate" format files into
     GRASS vector file format.


     IMPORTING A LINE COVERAGE

     The user will also be prompted for input when importing
     ARC/INFO files containing linear features ("line coverage")
     data.  Like polygon data, linear features are constructed by



GRASS 4.1                 U.S. Army CERL                        5






vv..iinn..aarrcc <alpha>      GRASS Reference Manual     <alpha> vv..iinn..aarrcc



     the series of arcs (aka, vectors) defining their perimeters.
     If the user selects LINE coverage, the prompts seen by the
     user will be different in two respects from those for
     POLYGON coverage.  First, the user will not be asked whether
     or not a neatline is desired;  and second, no label-points
     file name will be requested.  In other respects, the
     treatment of LINE coverage is identical to that for POLYGON
     coverage.

     The user is prompted for the name of the lines-file
     containing the geographic coordinates of these arcs.  The
     lines-file must first have been created with the ARC/INFO
     _U_n_g_e_n_e_r_a_t_e _L_I_N_E_S option, and is in the same format as the
     _p_r_e_f_i_x_._l_i_n file created by the GRASS _v_._o_u_t_._a_r_c program.


DDAATTAA FFIILLEE FFOORRMMAATTSS
     Following are examples of the data files discussed above.

     LINES FILE, also known as _p_r_e_f_i_x_._l_i_n or _p_r_e_f_i_x_._p_o_l file.
     This type of file can be created in ARC/INFO by using the
     _l_i_n_e_s subcommand of the _U_n_g_e_n_e_r_a_t_e command.   Each line
     (aka, arc) is defined by a line-ID number, followed by a
     list of at least two easting and northing coordinate pairs,
     followed by a line with the word "END".  The file is
     terminated with the word "END".

     The line-ID number is important only for line coverage data.
     For a line coverage, the line-ID number is the number that
     associates each line with its attribute data.

           3
     711916.000000   4651803.000000
     711351.875000   4651786.000000
     END
           3
     709562.500000   4651731.000000
     709617.250000   4651624.000000
     709617.250000   4651567.000000
     709585.000000   4651503.000000
     709601.125000   4651470.000000
     709696.875000   4651503.000000
     709720.500000   4651574.000000
     709823.750000   4651575.000000
     709893.125000   4651741.000000
     END
           3
     710296.875000   4651491.000000
     710295.125000   4651470.000000
     710223.000000   4651454.000000
     710154.500000   4651463.000000




GRASS 4.1                 U.S. Army CERL                        6






vv..iinn..aarrcc <alpha>      GRASS Reference Manual     <alpha> vv..iinn..aarrcc



     END
     END

     LABEL-POINTS FILE, also known as _p_r_e_f_i_x_._l_a_b file.
     This type of file can be created in ARC/INFO using the _P_o_i_n_t_s
     option of the _U_n_g_e_n_e_r_a_t_e command.
     The first number on each line is a label-point ID number, and the
     following two numbers are (respectively) the easting and northing
     coordinate pair representing the geographic location of the label-point.

     1     711539.875000   4651743.000000
     2     711429.000000   4650632.000000
     3     711027.625000   4651736.000000
     4     711022.625000   4651519.000000
     5     710482.750000   4651494.000000
     6     710474.500000   4651667.000000
     7     709269.750000   4651018.000000
     8     709726.500000   4651604.000000
     9     708926.375000   4651195.000000
     10    708567.500000   4651644.000000
     11    708272.750000   4651407.000000
     END

     LABEL-TEXT FILE, also known as _p_r_e_f_i_x_._t_x_t file.
     The ARC/INFO _D_i_s_p_l_a_y command can be used to create this type
     of file.

     1    -2.30228E+07   19399.848    1    0    0   0
     2       81079.875    1678.826    2    1   15   3
     3      955952.500   10229.637    3    2   19   8
     4       41530.875     926.887    4    3   17   3
     5       87900.188    1900.909    5    4   13   3
     6      166125.125    3512.950    6    5   15   3
     7       29460.563     824.968    7    6   17   3
     8     1022769.875    9105.707    8    7   20   9
     9       51385.500    1075.638    9    8   17   3
     10     376834.875    4470.027   10    9    9   2
     11      65802.688    1575.088   11   10   16   3

NNOOTTEESS
     ARC/INFO data can be imported even if a label-points and/or
     a label-text file are missing;  however, the lines and/or
     areas imported will not be labeled.

     _v_._i_n_._a_r_c can handle label-text files both with and without
     header lines.

SSEEEE AALLSSOO
     _v_._o_u_t_._a_r_c_, _v_._s_u_p_p_o_r_t

AAUUTTHHOORR
     Dave Johnson



GRASS 4.1                 U.S. Army CERL                        7






vv..iinn..aarrcc <alpha>      GRASS Reference Manual     <alpha> vv..iinn..aarrcc



     DBA Systems, Inc.
     10560 Arrowhead Drive
     Fairfax, Virginia 22030

NNOOTTIICCEE
     This program is part of the _a_l_p_h_a section of the GRASS
     distribution.  Unlike the code in the _m_a_i_n section of GRASS,
     the _a_l_p_h_a code has not yet been fully tested for one release
     cycle.














































GRASS 4.1                 U.S. Army CERL                        8



