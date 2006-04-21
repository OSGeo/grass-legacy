#include <stdio.h>
#include <grass/gis.h>
#include <grass/glocale.h>

int get_type (void)
{
    char buffer[256] ;

    for (;;)
    {
	G_clear_screen() ;
	fprintf (stdout,_("Please choose one of the following\n"));
	fprintf (stdout,_("   A define an area\n")) ;
	fprintf (stdout,_("   C define a circle\n")) ;
	fprintf (stdout,_("   L define a line\n")) ;
	fprintf (stdout,_("   X exit (and create map)\n"));
	fprintf (stdout,_("   Q quit (without creating map)\n"));
	fprintf (stdout,"> ") ;
	if (!G_gets(buffer)) continue ;
	switch (buffer[0] & 0177)
	{
	    case 'l': case 'L': return ('L') ;
	    case 'a': case 'A': return ('A') ;
	    case 'c': case 'C': return ('C') ;
	    case 'q': case 'Q': return ('Q') ;
	    case 'x': case 'X': return ('X') ;
	}
    }
}
