


r.out.pov <contrib>   GRASS Reference Manual  <contrib> r.out.pov



NAME
     r.out.pov - Converts a raster map layer into a height-field
     file for POVRAY.
     (GRASS Raster Data Export Program)

SYNOPSIS
     r.out.pov
     r.out.pov help
     r.out.pov [-h] map==name tga==name [hftype==value]
     [bias==value] [scale==value]

DESCRIPTION
     r.out.pov converts a user-specified raster map layer
     (map==name) into a height-field file for POVray (tga==name).
     The hftype==value option (where value is either 0 or 1)
     specifies the height-field type. When the user enters 0 the
     output will be actual heights. If entered 1 the cell-values
     will be normalized. If hftype is 0 (actual heights) the
     bias==value can be used to add or substract a value from
     heights.  Use scale==value to scale your heights by value.
     The GRASS program r.out.pov can be used to create height-
     field files for Persistence of Vision (POV) raytracer. POV
     can use a height-field defined in Targa (.TGA) image file
     format where the RGB pixel values are 24 bits (3 bytes). A
     16 bit unsigned integer height-field value is assigned as
     follows: RED = high byte, GREEN = low byte, BLUE = empty.

     Parameters:

     map==name	       Name of an existing raster map layer.

     tga==name	       Name of TARGA outputfile (one should add
		       the extension .tga).

     hftype==value     0=actual heights, 1=normalized heights.

     bias==value       Bias which is added or substracted to
		       heights.

     scale==value      Value to stretch or shrink elevations.

     r.out.pov can be run either non-interactively or
     interactively.  The program will be run non-interactively if
     the user specifies the name of a raster map layer and a name
     for tga (output), using the form

	  r.out.pov map==inname tga==outname

     where inname is the name of a raster map layer to be
     converted to POV format, and outname is the name of the
     outputfile. Further optional values can be entered.




GRASS 4.2.1		Baylor University			1






r.out.pov <contrib>   GRASS Reference Manual  <contrib> r.out.pov



     Alternately, the user can simply type r.out.pov on the
     command line, without program arguments.  In this case, the
     user will be prompted for parameter values using the
     standard GRASS parser interface described in the manual
     entry for parser.

	  r.out.pov map==elevation tga==out.tga

AUTHOR
     Klaus Meyer, GEUM.tec GbR, eMail: GEUM.tec@geum.de

NOTICE
     This program is part of the contrib section of the GRASS
     distribution.  As such, it is externally contributed code
     that has not been examined or tested by the Office of GRASS
     Integration.







































GRASS 4.2.1		Baylor University			2



