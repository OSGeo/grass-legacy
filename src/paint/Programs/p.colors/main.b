/* this code won't compile under 4.0 */
#include "gis.h"

#define NROWS 15
#define NCOLS 3

main()
{
    int i;
    int col;
    int line;
    int *colortable;
    unsigned char *temp;
    char colornum[NROWS*NCOLS][10];
    char next[30], next_line[80];
    char name[40],*mapset;
    int ncolors;
    long min,max,xnum;
    long startcolor, endcolor;
    long cn;
    int num;
    char title[80];
    struct Colors color;
    char *Ppainter_name();

    G_gisinit ("PAINT");
    Pconnect();
    ncolors = Pncolors();

    G_clear_screen();
    printf ("PAINT COLORS\n\n");

    mapset = G_ask_cell_old("Enter Name of Layer Needing Color Table",name);
    if (!mapset)
	exit(0);

    sprintf (title, "Please Enter %s Color Numbers (0-%d) for %s",
	Ppainter_name(), ncolors, name);


/* read color table. if none make an all white table */
    if(G_read_colors (name,mapset,&color) < 0)
    {
	CELL ncats;
	ncats = G_number_of_cats (name, mapset);
	G_init_colors (&color);
	while (ncats > 0)
	    G_set_color (ncats--, 255, 255, 255, &color);
    }

/* translate the cell color table to paint colors */
    min = color.min;
    max = color.max;
    xnum = max - min + 1;

    num = xnum;		/* check for int overflow */
    if (num != xnum)
	G_fatal_error ("Too many colors");

    temp = (unsigned char *) G_malloc (num);
    Pcolortable (color.red, color.grn, color.blu, temp, num);
    G_free_colors (&color);

    colortable = (int *) G_calloc (num, sizeof(int));
    for (i = 0; i < num; i++)
	colortable[i] = temp[i];
    free (temp);

/* avoid category zero */
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
	    if (cn == 0) continue;
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
	if (endcolor == 0) endcolor++;
	if (endcolor < min)
	    endcolor = min;
	if (endcolor > max)
	{
	    endcolor = max - NROWS*NCOLS + 1;
	    if (endcolor == 0) endcolor++;
	}
	startcolor = endcolor ;
    }

    G_init_colors (&color);
    for(i = 0; i < num; i++)
    {
	if(colortable[i] >= ncolors || colortable[i] < 0)
	    G_set_color ((CELL)(min+i), 255, 255, 255, &color);
	else
	{
	    float red, grn, blu;
	    int r, g, b;
	    Pcolorvalue (colortable[i], &red, &grn, &blu);
	    r = red * 256;
	    g = grn * 256;
	    b = blu * 256;
	    G_set_color ((CELL)(min+i), r, g, b, &color);
	}
    }
    G_set_color ((CELL)0, 255, 255, 255, &color);

    Pdisconnect();

    G_write_colors (name,mapset,&color);
    printf ("color table modifed\n");
}
