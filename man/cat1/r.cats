


r.cats <main>	     GRASS Reference Manual	    <main> r.cats



NAME
     r.cats - Prints category values and labels	associated with
     user-specified raster map layers.
     (GRASS Raster Program)

SYNOPSIS
     r.cats
     r.cats help
     r.cats map=name [cats=range[,range,...]]
     [fs=character|space|tab]

DESCRIPTION
     r.cats prints the category	values and labels for the raster
     map layer specified by map=name to	standard output.

     The user can specify all needed parameters	on the command
     line, and run the program non-interactively.  If the user
     does not specify any categories (e.g., using the optional
     cats=range[,range,...] argument), then all	the category
     values and	labels for the named raster map	layer that occur
     in	the map	are printed.  The entire map is	read, using
     r.describe, to determine which categories occur in	the map.
     If	a listing of categories	is specified, then the labels for
     those categories only are printed.	 The cats may be
     specified as single category values, or as	ranges of values.
     The user may also (optionally) specify that a field
     separator other than a space or tab be used to separate the
     category value from its corresponding category label in the
     output, by	using the fs=character|space|tab option	(see
     example below).  If no field separator is specified by the
     user, a tab is used to separate these fields in the output,
     by	default.

     The output	is sent	to standard output in the form of one
     category per line,	with the category value	first on the
     line, then	an ASCII TAB character (or whatever single
     character or space	is specified using the fs parameter),
     then the label for	the category.

     If	the user simply	types r.cats without arguments on the
     command line the program prompts the user for parameter
     values using the standard GRASS parser interface described
     in	the manual entry for parser.

EXAMPLES
     r.cats map=soils

	  prints the values and	labels associated with all of the
	  categories in	the soils raster map layer;

     r.cats map=soils cats=10,12,15-20




GRASS 4.2		Baylor University			1






r.cats <main>	     GRASS Reference Manual	    <main> r.cats



	  prints only the category values and labels for soils
	  map layer categories 10, 12, and 15 through 20;  and

     r.cats map=soils cats=10,20 fs= :

	  prints the values and	labels for soils map layer
	  categories 10	and 20,	but uses ":" (instead of a tab)
	  as the character separating the category values from
	  the category values in the output.

     Example output:

	  10:Dumps, mine, Cc
	  20:Kyle clay,	KaA

NOTES
     Any ASCII TAB characters which may	be in the label	are
     replaced by spaces.

     The output	from r.cats can	be redirected into a file, or
     piped into	another	program.

SEE ALSO
     UNIX Manual entries for awk and sort

     r.coin, r.describe, r.rast.what, r.support, and parser

AUTHOR
     Michael Shapiro, U.S. Army	Construction Engineering Research
     Laboratory

























GRASS 4.2		Baylor University			2



