#include <string.h>
#include <stdlib.h>
#include "vask.h"
#include "gis.h"
#include "Paintlib.h"

static int add_colors(CELL, int, int, int, struct Colors *);
static int add_d_colors(DCELL, DCELL, int, int, int, int, struct Colors *);

#define NROWS 15
#define NCOLS 3

int main(int argc,char *argv[])
{
    int i,j, nsteps, startstep, endstep, cur = 0,prev = 0;
    int col;
    int line;
    int *colortable;
    unsigned char *temp, *red, *grn, *blu;
    char colornum[NROWS*NCOLS][10];
    char next[30], next_line[80];
    char name[40],*mapset;
    int ncolors;
    int white, fp;
    CELL cat,min,max;
    DCELL val, dmin, dmax;
    int dbl_len;
    long xnum;
    long startcolor, endcolor;
    long cn;
    int num;
    char title[80];
    struct Colors colors;

    G_gisinit (argv[0]);
    Pconnect();
    ncolors = Pncolors();
    white = Pcolornum(1.0,1.0,1.0);

    G_clear_screen();
    fprintf (stdout,"PAINT COLORS\n\n");

    mapset = G_ask_cell_old("Enter name of raster map needing color table",name);
    if (!mapset)
	exit(0);

    fp = G_raster_map_is_fp(name, mapset);

    /* if the map is floating point ask how many floating point ranges
    will be assigned separate colors */
    if(fp)
    {
	V_clear();
	V_line (5, "     Enter the number of fp intervals to assign separate colors:");
	V_ques (&nsteps, 'i', 5, 65, 5);
	V_line (8, title);

        *title = 0;
        while (1)
        {
           V_intrpt_ok();
           if(!V_call())
	   {
	       Pdisconnect();
	       fprintf (stdout,"color table not changed\n");
	       exit(0);
	   }
           if (nsteps > 0)
	        break;
           sprintf (title, "     ** Non-positive values not allowed **");
        }
    }

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
    G_get_d_color_range(&dmin, &dmax, &colors);
    /* G_get_color_range() sets min to 1 if range for the map isn't set */

    if(min==1) min = 0;
    /* added by Olga apr,94 in order to allow editing of cat 0 */

    if(!fp)
       xnum = max - min + 1;
    else
       xnum = nsteps;

    num = xnum;		/* check for int overflow */
    if (num != xnum)
	G_fatal_error ("Too many colors");

    temp = (unsigned char *) G_malloc (num);
    red  = (unsigned char *) G_malloc (num);
    grn  = (unsigned char *) G_malloc (num);
    blu  = (unsigned char *) G_malloc (num);

    if(!fp)
    {
       for (i = 0, cat = min; cat <= max; cat++, i++)
       {
	   int r,g,b;
	   G_get_color (cat, &r, &g, &b, &colors);
	   red[i] = r;
	   grn[i] = g;
	   blu[i] = b;
       }
    }
    else
    {
       for (i = 0; i<nsteps; i++) 
       {
	   int r,g,b;
	   val = dmin + (double) i * (dmax-dmin) / (double)nsteps;
	   G_get_d_raster_color (&val, &r, &g, &b, &colors);
	   red[i] = r;
	   grn[i] = g;
	   blu[i] = b;
       }
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
    if ((!fp && min > max) || (fp && dmin > dmax) || num <= 0)
    {
	Pdisconnect();
	fprintf (stdout,"No colors to modify\n");
	exit(0);
    }

    startcolor = min;
    startstep = 0;
    while (1) 
    {
	V_clear() ;
	V_line (0, title);

	endcolor = startcolor+NCOLS*NROWS;
	if (endcolor > max+1)
	    endcolor = max+1 ;
	endstep = startstep + NCOLS*NROWS;
	if(endstep > nsteps)
	    endstep = nsteps;

	line = 0;
	col = 7;
	if(!fp)
	{
	    for (cn = startcolor; cn < endcolor; cn++) 
	    {
/*	        if (cn == 0) continue;*/
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
	     if (endcolor == 0) endcolor++;
	     if (endcolor > max)
	         strcpy (next, "end");
	     else
	         sprintf (next, "%ld", endcolor);

	     sprintf (next_line, "%*s%*s  (of %ld)",
                   26, "Next color: ",5,"",(long int)max);
             line = NROWS+3;

	}
	else
	{
            for (i = startstep ; i < endstep; i++)
	    {
	        val = dmin + (double) i * (dmax-dmin) / (double) nsteps;
	        sprintf (colornum[i-startstep], "%.2f", val);
		dbl_len = strlen(colornum[i-startstep]);
	        V_const (colornum[i-startstep], 's', line+2, col, dbl_len) ;
	        V_ques  (&colortable[i], 'i', line+2, col+dbl_len+2, 4) ;
	        line++;
	        if (line >= NROWS)
	        {
		    line = 0;
		    col += 10 + dbl_len;
	        }
             }
	     if (endstep == 0) endstep++;
	     if (endstep >= nsteps)
	         strcpy (next, "end");
	     else
	         sprintf (next, "%8f", dmin + (double) endstep * (dmax-dmin) /
					     (double) nsteps);

	     sprintf (next_line, "%*s%*s  (max value %8f)", 26, "Next value: ",8,"",dmax);
	     line = NROWS+3;

	}
	*next = 0;
	V_line (line, next_line);
	V_ques (next, 's', line, 26, 8);


	V_intrpt_ok();
	if(!V_call())
	{
	    Pdisconnect();
	    fprintf (stdout,"color table not changed\n");
	    exit(0);
	}

	if (*next == 0) break;
	if (strcmp (next, "end") == 0) break;
	if(!fp)
	{
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
	else
	{
	    if (sscanf (next, "%lf", &val) != 1)
		continue;
	    if (val < dmin)
		startstep = 0;
	    if (val > dmax)
	    {
		startstep = nsteps - NROWS*NCOLS;
		if(startstep < 0) startstep = 0;
	    }
	    else
	        startstep = (int) (val / ((dmax-dmin)/(double) nsteps));	
            endstep = startstep;
	} 
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
	    if(!fp)
	        add_colors (min,i,j,prev,&colors);
            else
		add_d_colors(dmin, dmax, nsteps, i, j, prev,&colors);
	    prev = cur;
	    i = j;
	}
    }
    if(!fp)
        add_colors (min,i,j,cur,&colors);
    else
        add_d_colors (dmin,dmax,nsteps, i,nsteps,cur,&colors);

    Pdisconnect();

    G_write_colors (name,mapset,&colors);
    fprintf (stdout,"color table modifed\n");

    exit(0);
}

int add_colors (CELL min, int j, int i, int pnum, struct Colors *colors)
{
    float fr, fg, fb;
    int r, g, b;
    Pcolorvalue (pnum, &fr, &fg, &fb);
    r = fr * 256;
    g = fg * 256;
    b = fb * 256;
    G_add_color_rule ((CELL)(min+j), r,g,b, (CELL)(min+i-1), r,g,b, colors);

    return 0;
}

int add_d_colors (DCELL min, DCELL max, int nsteps, int j, int i,
    int pnum, struct Colors *colors)
{
    DCELL v1, v2;
    float fr, fg, fb;
    int r, g, b;

    v1 = min + j * (max - min)/ (double) nsteps;
    v2 = min + i * (max - min)/ (double) nsteps;
    Pcolorvalue (pnum, &fr, &fg, &fb);
    r = fr * 256;
    g = fg * 256;
    b = fb * 256;
    G_add_d_raster_color_rule (&v1, r,g,b, &v2, r,g,b, colors);

    return 0;
}
