


r.in.ll	<main>	     GRASS Reference Manual	   <main> r.in.ll



NAME
     r.in.ll - Converts	raster data referenced using latitude and
     longitude coordinates to a	UTM-referenced map layer in GRASS
     raster format.
     (GRASS Raster Data	Import Program)

SYNOPSIS
     r.in.ll
     r.in.ll help
     r.in.ll [-s] input=name output=name bpc=value
	corner=corner,lat,lon dimension=rows,cols
	res=latres,lonres spheroid=name

DESCRIPTION
     This program converts raster data referenced using	 latitude
     and  longitude  coordinates to a UTM-referenced map layer in
     GRASS raster format.  r.in.ll is primarily	used as	the final
     program in	converting DTED	and DEM	digital	elevation data to
     GRASS raster  format,  but	 is  not  limited  to  this  use.
     r.in.ll  uses the user's current geographic region	settings.
     Only data that falls within the  current  geographic  region
     will appear in the	final raster map layer.

     r.in.ll  requires	the   user   to	  enter	  the	following
     information:


COMMAND	LINE OPTIONS
     Flags:

     -s		       Signed  data  (high  bit	 means	 negative
		       value).


     Parameters:

     input=name	       Name  of	 an  existing  input  raster  map
		       layer.

     output=name       Name to be assigned to the  output  raster
		       map layer.

     bpc=value	       Number of bytes per cell.

     corner=corner,lat,lon
		       One corner latitude and longitude  of  the
		       input.
		       Format:
		       {nw|ne|sw|se},dd:mm:ss{N|S},ddd:mm:ss{E|W}

		       The latitude and	longitude  are	specified
		       as  dd.mm.ssH  where  dd	is degrees, mm is



GRASS 4.2		Baylor University			1






r.in.ll	<main>	     GRASS Reference Manual	   <main> r.in.ll



		       minutes,	ss  is	seconds,  and  H  is  the
		       hemisphere  (N  or S for	latitudes, E or	W
		       for longitudes).

		       For  example,  to  specify  the	southwest
		       corner:	corner=sw,46N,120W

		       Note:	the   latitude	 and	longitude
		       specified are for the center of the corner
		       cell.

     dimension=rows,cols
		       Number of rows and columns  in  the  input
		       file.

     res=latres,lonres Resolution of the input (in arc seconds).

     spheroid=name     Name of spheroid	to be used for coordinate
		       conversion.
		       Options:	   airy,   australian,	  bessel,
		       clark66,	   everest,    grs80,	 hayford,
		       international,  krasovsky,  wgs66,  wgs72,
		       wgs84

EXAMPLE
     The command line:

	  r.in.ll input=rot.out	output=import.out
	     dimension=358,301 bpc=2 res=3,3
	     corner=sw,37:13N,103:45W spheroid=wgs72

     reads data	from the file rot.out,	converts  the  data,  and
     stores  them  in  the  file  import.out.	The  data  to  be
     converted are made	up of 358 rows and 301 columns,	and  have
     a resolution of 3x3 arc seconds.

NOTES
     In	the conversion of DTED and DEM elevation data  to  raster
     map  layer	 format,  r.in.ll  follows  execution of the data
     rotation  program	m.rot90.   Because  the	 user  can  glean
     information   on	the  number  of	 rows  and  columns,  the
     resolutions of the	latitude and longitude,	and the	number of
     bytes  per	 column	from the header	file produced by the tape
     extraction	programs m.dted.extract	 and  m.dmaUSGSread,  the
     user  should  recall  that	 m.rot90  has  rotated	the files
     produced by the tape extraction programs  90  degrees;  this
     means  that  the user should INTERCHANGE the numbers of rows
     and columns present in the	header file for	input to r.in.ll.
     The number	of rows	shown in the tape extract header file now
     become the	number of columns in  the  m.rot90  output  file;
     the  number of columns shown in the tape extract header file
     are now the number	of rows	present	 in  the  m.rot90  output



GRASS 4.2		Baylor University			2






r.in.ll	<main>	     GRASS Reference Manual	   <main> r.in.ll



     file.

     The user should also note that the	raster map layer imported
     into  GRASS  will	be based on the	current	geographic region
     settings.	The boundaries of this geographic  region  should
     therefore	be checked before importing the	raster map layer.
     Data outside of the geographic region will	not  be	 imported
     and  missing  data	 will  be assigned the category	value "no
     data".

SEE ALSO
     m.dmaUSGSread, m.dted.examine, m.dted.extract, m.rot90

AUTHOR
     Michael Shapiro, U.S. Army	Construction Engineering Research
     Laboratory







































GRASS 4.2		Baylor University			3



