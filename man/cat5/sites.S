


sites.S		     GRASS Reference Manual		  sites.S



NAME
     sites.S - Description of the GRASS	s.menu "S" interface
     option.

DESCRIPTION
     This describes the	GRASS s.menu "S" statistical package
     interface option.	See also the manual entry for s.menu.

SPECIAL	ADJUSTMENT
     Due to the	fact that "S" references all subscripts	beginning
     with 1, and the GIS data begins with 0, it	was necessary to
     add 1 to the category numbers.  Therefore,	category value 0
     ("no data") becomes category value	1, category 1 becomes
     category 2, etc.

S DATA STRUCTURES
     Below are descriptions of the data	structures created by the
     interface.	 As a general comment, within "S", simply typing
     the data structure	name will display the values contained in
     the structure.  You may also find it interesting to display
     the data structures using the "S" function	dput().

cat.histo, (cat.#.histo)
     This structure contains the histogram for the categories in
     each map layer.  The histogram contains the number	of cells
     of	each category which occur in the user's	geographic region
     (see wind.n, et al., below).  It is a 2-dimensional integer
     array (matrix).  The first	subscript references the map
     layer.  The second	subscript references the category value.

     examples:

	cat.histo[2,5] is the cell count for layer 2, category 5.
	cat.histo[2,]  is the full histogram for layer 2.

     Note:  since more than one	layer may occur	in the data, and
     the number	of categories in each layer varies, it was
     necessary to create this structure	with sufficient	dimension
     to	hold the largest category value	for all	layers.	Histogram
     data for categories which do not occur are	set to NA ("S"
     notation for "no data").  However,	there is also an
     individual	histogram structure for	each layer:  cat.1.histo,
     cat.2.histo, etc.	These are simple vectors.


cat.name
     This structure contains the names for the categories in each
     map layer.	 It is a 2-dimensional character array (matrix).
     The first subscript references the	map layer.  The	second
     subscript references the category.

     Examples:



GRASS 4.2		Baylor University			1






sites.S		     GRASS Reference Manual		  sites.S



	cat.name[2,5] is the name of category 5	for layer 2.
	cat.name[2,]  are all category names for layer 2.

     Note:  since more than one	layer may occur	in the data, and
     the number	of categories in each layer varies, it was
     necessary to create this structure	with sufficient	dimension
     to	hold the largest category value	for all	layers.	 Names
     for categories which do not occur are set to ''.  However,
     there is also an individual category name structure for each
     layer:  cat.1.name, cat.2.name, etc.  These are simple
     vectors.

site.data
     This structure contains the data for each site.  It is a 3-
     dimensional integer array.	 The first subscript references
     the categories which occurred at the site.	 The second
     subscript references the site.  The third subscript
     references	the layer.

     Examples:

	site.data[,5,3]	is the data for	site 5 in layer	3.
	site.data[,2,]	is the data for	site 2 in all layers.
	site.data[,,1]	is all site data for layer 1.

     Note: the size of the first dimension will	be the number of
     cells in a	site, the size of the second dimension will be
     the number	of sites, the size of the third	dimension will be
     the number	of layers.

site.mode
     Since "S" does not	provide	a statistical mode function, this
     structure contains	the most frequently occurring category
     for each site in each layer.  It is a 2-dimensional integer
     array (matrix).  The first	subscript references the site.
     The second	subscript references the layer.

     Examples:

	site.mode[5,3] is the mode for site 5 in layer 3.
	site.mode[2,]  are the modes for site 2	in all layers.
	site.mode[,1]  are all site modes for layer 1.

     Note:  this 'mode'	is not the strict definition of	the mode.
     Since category 0 (which in	"S" is category	1) represents "no
     data" in the GIS databases, it was	excluded from the mode
     calculations (essentially as if it	had been NA).  For
     example, if the data for a	site are 1 1 1 1 2 2 3 2 3 1 1,
     the mode will be 2.

layer.name
     This structure contains the raster	map layer names.  It is	a



GRASS 4.2		Baylor University			2






sites.S		     GRASS Reference Manual		  sites.S



     2-dimensional array (i.e.,	a matrix).  The	first subscript
     references	the map	layer.	The second subscript selects
     either the	map layer name,	or the map layer title.


     Examples:

	layer.name[3,1]	is the name of layer 3.
	layer.name[3]	is also	the name of layer 3.
	layer.name[2]	is the name of layer 2.
	layer.name[3,2]	is the title for layer 3.


location
     This is a simple character	vector giving the GRASS	location
     from which	the data was extracted.


mapset
     This is a simple character	vector giving the GRASS	mapset.


nlayers
     This is a simple integer giving the number	of map layers.


nsites
     This is a simple integer giving the number	of sites.

site.e
     This is a simple integer vector giving the	geographic
     easting for each site.

site.n
     This is a simple integer vector giving the	geographic
     northing for each site.

site.name
     This is a simple character	vector giving the description for
     each site.

sitelist
     This is a simple character	vector giving the name and
     description of the	site lists file	from which the sites were
     taken.

wind.n,	wind.s,	wind.w,	wind.e
     These are simple real numbers giving the north, south, west,
     east of the mapset's current geographic region.

wind.res
     This is a simple real number giving the GRASS database



GRASS 4.2		Baylor University			3






sites.S		     GRASS Reference Manual		  sites.S



     resolution	(in meters).




S MACROS
     You may find the following	"S" macros helpful when
     referencing the 'site.data' and 'site.mode' structures,
     since they	allow you to specify parameters:


     MACRO site.data(site, layer)
     ({
	  site.data[ , site, layer]
     })
     END

     MACRO site.mode(site, layer)
     ({
	  site.mode[site, layer]
     })
     END

MACRO USAGE
     To	select the site.data for all sites for layer 2:

	  ?site.data(layer=2)

     to	select the site.data for site 4	in all layers:

	  ?site.data(site=4)

     to	select the site.mode for site 10 in layer 1:

	  ?site.mode(layer=1,site=10)


SORRY, BUT ...
     These macros are not provided by the interface.  To use
     these macros, you will have to type them into a text file
     and then, from "S", issue the command:

	  define("<file>")

SEE ALSO
     s.db.rim, s.in.ascii, s.menu, s.out.ascii,	s.surf.idw,
     sites.format, sites.occur,	sites.report








GRASS 4.2		Baylor University			4



