#include "gis.h"

#define NROWS 15
#define NCOLS 3

main(argc,argv) char *argv[];
{
    int i,j, cur,prev;
    int col;
    int line;
    int *colortable;
    unsigned char *temp, *red, *grn, *blu;
    char colornum[NROWS*NCOLS][10];
    char next[30], next_line[80];
    char name[40],*mapset;
    int ncolors;
    int white;
    CELL cat,min,max;
    long xnum;
    long startcolor, endcolor;
    long cn;
    int num;
    char title[80];
    struct Colors colors;
    char *Ppainter_name();

    G_gisinit (argv[0]);
    Pconnect();
    ncolors = Pncolors();
    white = Pcolornum(1.0,1.0,1.0);

    G_clear_screen();
    printf ("PAINT COLORS\n\n");

    mapset = G_ask_cell_old("Enter name of raster map needing color table",name);
    if (!mapset)
	exit(0);

    sprintf (title, "Please Enter %s Color Numbers (0-%d) for %s",
	Ppainter_name(), ncolors, name);


/* read color table. if none make an all white table */
    if(G_read_colors (name,mapset,&colors) < 0)
    {
	CELL ncats;
	ncats = G_number_of_cats (name, mapset);
	G_init_colors (&colors);
/* 	G_add_color_rule ((CELL)0, 255, 255, 255, ncats, 255, 255,255, &colors);*/
    }

/* translate the cell color table to paint colors */
    G_get_color_range (&min, &max, &colors);
    /* G_get_color_range() sets min to 1 if range for the map isn't set */

    if(min==1) min = 0;
    /* added by Olga apr,94 in order to allow editing of cat 0 */

    xnum = max - min + 1;

    num = xnum;		/* check for int overflow */
    if (num != xnum)
	G_fatal_error ("Too many colors");

    temp = (unsigned char *) G_malloc (num);
    red  = (unsigned char *) G_malloc (num);
    grn  = (unsigned char *) G_malloc (num);
    blu  = (unsigned char *) G_malloc (num);

    for (i = 0, cat = min; cat <= max; cat++, i++)
    {
	int r,g,b;
	G_get_color (cat, &r, &g, &b, &colors);
	red[i] = r;
	grn[i] = g;
	blu[i] = b;
    }
    Pcolortable (red, grn, blu, temp, num);
    free(red);
    free(grn);
    free(blu);
    G_free_colors (&colors);

    colortable = (int *) G_calloc (num, sizeof(int));
    for (i = 0; i < num; i++)
	colortable[i] = temp[i];
    free (temp);

/* commented out by Olga. Apr, 94 to allow users change color for cat 0 */
/*
    if (min == 0)
    {
	min++;
	colortable++;
	num--;
    }
    if (max == 0)
    {
	max--;
	num--;
    }
*/
    if (min > max || num <= 0)
    {
	Pdisconnect();
	printf ("No colors to modify\n");
	exit(0);
    }

    startcolor = min;
    while (1) 
    {
	V_clear() ;
	V_line (0, title);

	endcolor = startcolor+NCOLS*NROWS;
	if (endcolor > max+1)
	    endcolor = max+1 ;

	line = 0;
	col = 7;
	for (cn = startcolor; cn < endcolor; cn++) 
	{
/*	    if (cn == 0) continue;*/
	    i = cn - startcolor ;
	    sprintf (colornum[i], "%5ld", cn);
	    V_const (colornum[i], 's', line+2, col, 5) ;
	    V_ques  (&colortable[cn-min], 'i', line+2, col+7, 4) ;
	    line++;
	    if (line >= NROWS)
	    {
		line = 0;
		col += 15;
	    }
	}

	*next = 0;
	if (endcolor == 0) endcolor++;
	if (endcolor > max)
	    strcpy (next, "end");
	else
	    sprintf (next, "%ld", endcolor);

	sprintf (next_line, "%*s%*s  (of %ld)", 26, "Next color: ",5,"",max);
	line = NROWS+3;

	V_line (line, next_line);
	V_ques (next, 's', line, 26, 5);


	V_intrpt_ok();
	if(!V_call())
	{
	    Pdisconnect();
	    printf ("color table not changed\n");
	    exit(0);
	}

	if (*next == 0) break;
	if (strcmp (next, "end") == 0) break;
	if (sscanf (next, "%ld", &endcolor) != 1)
		continue;
	if (endcolor < min)
	    endcolor = min;
	if (endcolor > max)
	{
	    endcolor = max - NROWS*NCOLS + 1;
	}
	startcolor = endcolor ;
    }

    G_init_colors (&colors);
    i = 0;
    for(j = 0; j < num; j++)
    {
	cur = colortable[j];
	if(cur >= ncolors || cur < 0)
	    cur = white;
	if (j == 0)
	    prev = cur;
	if (prev != cur)
	{
	    add_colors (min,i,j,prev,&colors);
	    prev = cur;
	    i = j;
	}
    }
    add_colors (min,i,j,cur,&colors);
    /* G_set_color ((CELL)0, 255, 255, 255, &colors);*/

    Pdisconnect();

    G_write_colors (name,mapset,&colors);
    printf ("color table modifed\n");
}

add_colors (min,j,i,pnum,colors)
    CELL min;
    struct Colors *colors;
{
    float fr, fg, fb;
    int r, g, b;
    Pcolorvalue (pnum, &fr, &fg, &fb);
    r = fr * 256;
    g = fg * 256;
    b = fb * 256;
    G_add_color_rule ((CELL)(min+j), r,g,b, (CELL)(min+i-1), r,g,b, colors);
}
