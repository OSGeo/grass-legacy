/**********************************************************************
 *
 *   G_make_color (name, mapset, colors)
 *       char *name               name of map
 *       char *mapset             mapset containing name
 *       struct Colors *colors    struct to hold colors
 *
 *   Interactively prompts user for deciding which type of color
 *   lookup table is desired.
 *       Red, green, and blue color ramps
 *       Gray scale
 *       Rainbow colors
 *       Random colors
 *       Color wave
 *       Aspect colors
 *       Red through yellow to green
 *
 *   Returns -1 user canceled the request
 *            1 color table is ok
 **********************************************************************/

#include "gis.h"

G_make_colors (name, mapset, pcolr)
    char *name ;
    char *mapset ;
    struct Colors *pcolr ;
{
    char buff[128] ;
    int answ ;
    struct Range range;
    CELL min, max;
    struct Histogram histo;

    G_init_colors (pcolr);

    if (G_read_range (name, mapset, &range) < 0)
	return -1;

    min = range.nmin ? range.nmin : range.pmin;
    max = range.pmax ? range.pmax : range.nmax;


/* Prompting */
ASK:
    G_clear_screen() ;
    printf("\n\nColor table needed for file [%s] in mapset [%s].\n",
	    name, mapset) ;

    printf("\nPlease identify the type desired:\n") ;
    printf("    1:  Random colors\n") ;
    printf("    2:  Red, green, and blue color ramps\n") ;
    printf("    3:  Color wave\n") ;
    printf("    4:  Gray scale\n") ;
    printf("    5:  Gray scale (histogram contrast stretched)\n") ;
    printf("    6:  Aspect\n") ;
    printf("    7:  Rainbow colors\n") ;
    printf("    8:  Red through yellow to green\n");
    printf ("RETURN  quit\n");
    printf("\n> ") ;

    for(;;)
    {
	if(!G_gets(buff)) goto ASK ;
	G_strip (buff);
	if (*buff == 0) return -1;
	if(sscanf(buff,"%d",&answ) != 1) answ = -1;

	switch (answ)
	{
	case 1: return G_make_random_colors (pcolr, min, max);
	case 2: return G_make_color_ramp (pcolr, min, max);
	case 3: return G_make_color_wave (pcolr, min, max);
	case 4: return G_make_grey_scale (pcolr, min, max);
	case 5: if (G_read_histogram (name, mapset, &histo) <= 0)
		    goto ASK;
		G_make_histo_grey_scale (pcolr, &histo);
		G_free_histogram (&histo);
		return 1;
	case 6: return G_make_aspect_colors (pcolr, min, max);
	case 7: return G_make_rainbow_colors (pcolr, min, max);
	case 8: return G_make_red_yel_grn (pcolr, min, max);
	default:
	    printf("\n%s invalid; Try again > ", buff) ;
	    break;
	}
    }
}
