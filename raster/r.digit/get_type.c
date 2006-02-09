#include <stdio.h>
#include <grass/gis.h>

int get_type (void)
{
    char buffer[256] ;

    for (;;)
    {
        G_clear_screen() ;
        fprintf (stdout,"Please choose one of the following\n");
        fprintf (stdout,"   A define an area\n") ;
        fprintf (stdout,"   C define a circle\n") ;
        fprintf (stdout,"   L define a line\n") ;
        fprintf (stdout,"   Q quit (and create map)\n");
        fprintf (stdout,"> ") ;
        if (!G_gets(buffer)) continue ;
        switch (buffer[0] & 0177)
        {
            case 'l': case 'L': return ('L') ;
            case 'a': case 'A': return ('A') ;
            case 'c': case 'C': return ('C') ;
            case 'q': case 'Q': return ('Q') ;
        }
    }
}
