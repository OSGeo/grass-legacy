


v.to.sites <main>    GRASS Reference Manual	<main> v.to.sites



NAME
     v.to.sites	- Converts point data in a binary GRASS	vector
     map layer into a GRASS site_lists file.
     (GRASS Vector Program)

SYNOPSIS
     v.to.sites
     v.to.sites	help
     v.to.sites	input=name output=name

DESCRIPTION
     The v.to.sites program extracts site data from a GRASS
     vector map	layer and stores output	in a new GRASS site_lists
     file.  The	resulting sites	map layer can be used with such
     programs as d.sites.  Only	site (point) features in the
     named vector map layer are	extracted and placed into the
     resulting site list.  Lines and areas in the vector file are
     ignored.

     The user can run the program non-interactively by specifying
     the names of an existing vector input map layer and a new
     site list file to be output on the	command	line.  The
     program will be run interactively if the user types
     v.to.sites	without	arguments on the command line.	In this
     case, the user will be prompted to	enter parameter	values
     through the standard user interface described in the manual
     entry for parser.


     Parameters:

     input=name	       Name of an existing binary vector map
		       layer from which	site data are to be
		       extracted.

     output=name       Name to be assigned to the resultant
		       site_lists file.

     If	any of the sites have been labeled in v.digit, then the
     resultant site list will contain category information.  If
     none of the sites are labeled, a binary (0/1) site	list file
     will be produced.

SEE ALSO
     d.sites, s.db.rim,	s.menu,	v.db.rim, v.digit and parser

AUTHOR
     Dave Gerdes, U.S. Army Construction Engineering Research
     Laboratory






GRASS 4.2		Baylor University			1



