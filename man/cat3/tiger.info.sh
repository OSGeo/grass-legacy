


tiger.info.sh <scriptsGRASS Reference Manu<scripts> tiger.info.sh



NAME
     tiger.info.sh - Provides tract number(s) and classification
     codes found within	a given	U.S. Census Bureau TIGER type1
     data file.
     (GRASS Shell Script)

SYNOPSIS
     tiger.info.sh help
     tiger.info.sh infile

DESCRIPTION
     tiger.info.sh is a	shell script which outputs tract
     number(s) and classification codes	found within the TIGER
     type1 data	file infile.  Output is	written	to standard out,
     and can be	captured in a file by redirecting output.  This
     information is useful when	querying (using	v.db.rim) from
     the master	binary vector file created by v.in.tig.rim.  It
     also provides tract number(s) which can be	used as	input to
     the command Gen.Maps.

     Parameters:

     infile	       Name of a TIGER type1 data file.

NOTES
     This command must be installed separately as part of the
     package of	routines dealing with the import of Census
     (TIGER) data.

SEE ALSO
     m.tiger.region, v.in.tig.rim, v.db.rim, Gen.Maps,
     Gen.tractmap

AUTHOR
     Marjorie Larson, U.S. Army	Construction Engineering Research
     Laboratory



















GRASS 4.2		Baylor University			1



