


Mimport.ll(2M)       GRASS Reference Manual        Mimport.ll(2M)



NAME
     Mimport.ll  - Creates a UTM-referenced from lat/long data
     (Map Development Tool)

SYNOPSIS
     Mimport.ll  if=infile of=outfile rows=# cols=# bpc=#[u]
                 latres=# lonres=# xx=lat,lon s=spheroid

DESCRIPTION
     Mimport.ll converts data referenced using latitude and
     longitude coordinates to GRASS format.  Mimport.ll is
     primarily used as the final program in converting DTED and
     DEM data to GRASS form, but is not limited to this use.
     Mimport.ll uses the user's current Only data that falls in
     the current will appear in the final cell

     Mimport.ll requires the user to enter nine inputs:

     if   The full pathname of a data file referenced using
          latitude-longitude values.

     of   The name of the resultant cell

     rows The number of rows of data in the input file.

     cols The number of columns of data in the input file.

     bpc  The number of bytes per column in the input file.  The
          default is 1, if not specified.  If the data is
          unsigned, append a u to the number.

     latres
          The latitude resolution of the data in arc-seconds.

     lonres
          The longitude resolution of the data in arc-seconds.

     xx   The latitude and longitude of any one corner of the
          input file.  xx specifies which corner and should NOT
          be specified as xx, but as one of: sw se nw ne

          The latitude and longitude are specified as: dd.mm.ssh
          where dd is degrees, mm is minutes, ss is seconds, and
          h is the hemisphere (n or s for latitudes, e or w for
          longitudes).

          For example,  to specify the southwest corner:
          sw=46.00.00n,120.00.00w

          Note: the latitude and longitude specified are for the
          center of the cell at the xx corner.




GRASS 3.2                U.S. Army CERL                         1






Mimport.ll(2M)       GRASS Reference Manual        Mimport.ll(2M)



     s    Specifies the name of the spheroid of the local datum
          to be used for converting from lat/lon to UTM
          coordinates.  The following spheroids can currently be
          specified:

               spheroid      semi-major     eccentricity
                 name           axis          squared

             australian      6378160.0     0.0066945419
             bessel          6377739.155   0.0066743722
             clark66         6378206.4     0.006768658
             clark80         6378249.145   0.0068035113
             everest         6377276.345   0.0066378466
             international   6378388.0     0.00672267
             wgs72           6378135.0     0.006694317778

          If your is registered to a spheroid which is not in
          this list, you may specify the spheroid parameters as
          follows (no spaces allowed):

             s=a=semi-major-axis,e=eccentricity-squared

EXAMPLE
     The command line:

     Mimport.ll if=rot.out of=import.out  rows=358  cols=301
                bpc=2 latres=3 lonres=3 sw=37.13.00n,103.45.00w
                s=wgs72

     reads data from the file "rot.out", converts the data, and
     stores it in the file "import.out". The data to be converted
     is made up of 358 rows and 301 columns, and has a resolution
     of 3x3 arc seconds.

NOTES
     In the conversion of DTED and DEM elevation data to form,
     Mimport.ll follows execution of the data rotation program
     Mrot90. Because the user can glean information on the number
     of rows and columns, the resolutions of the latitude and
     longitude, and the number of bytes per column from the
     header file produced by the tape extraction programs
     Mdted.extract and MdmaUSGSread, the user should recall that
     Mrot90 has rotated the files produced by the tape extraction
     programs 90 degrees; this means that the user should
     INTERCHANGE the numbers of rows and columns present in the
     header file for input to Mimport.ll. The number of rows
     shown in the tape extract header file now become the number
     of columns in the Mrot90 output file;  the number of columns
     shown in the tape extract header file are now the number of
     rows present in the Mrot90 output file.





GRASS 3.2                U.S. Army CERL                         2






Mimport.ll(2M)       GRASS Reference Manual        Mimport.ll(2M)



     The user should also note that the cell imported into GRASS
     will be based on the current
       The boundaries of this should therefore be checked before
     importing the cell Data outside of the will not be imported
     and missing data will become "no data".


SEE ALSO
     MdmaUSGSread[2M], Mdted.examine[2M], Mdted.extract[2M],
     Mrot90[2M]

AUTHOR
     Michael Shapiro, U.S. Army Construction Engineering Research
     Laboratory









































GRASS 3.2                U.S. Army CERL                         3



