#include <string.h>
#include "gis.h"
#include "parms.h"
#include "misc.h"
#include "colormode.h"
#include "Paintlib.h"
#include "local_proto.h"

static int BLOCKSIZE;
static int BLOCKSPACE;
static int NBLOCKS;
static int TEXTSPACE;
static int FUDGE;
static int PRINTERWIDTH;

#define SKIP0
#undef PERCENT_COVER

int ctable (struct Categories *pcats,
    struct Colors *pcolr, struct Cell_stats *statf)
{
    unsigned char white;
    unsigned char black;
    double total;

    char buf[200];
    char temp1[40];
    char temp2[40];
    char *name;

    int prows, pcols;

    int b;
    int block;
    int c;
    int cols;
    CELL *index;
    int rr,cc;
    int left;
    int len;
    int lines;
    int maxname;
    int max;
    int nblocks;
    int r;
    int rows;
    CELL catnum;
    DCELL dmin, dmax, val;
    char dmin_str[20], dmax_str[20], out_str[50];
    long count;
    int cats_appearing;
    short *red_carry_below = 0;	/* color carryovers */
    short *grn_carry_below = 0;
    short *blu_carry_below = 0;
    short red_carry_right;
    short grn_carry_right;
    short blu_carry_right;


fprintf (stdout," ctable \n");
    white = WHITE ;
    black = BLACK ;

    Pnpixels(&prows, &pcols) ;
    if (prows) return 0;

    if (colormode == COLORMODE_DIFFUSION)
    {
	red_carry_below = (short *)G_malloc (pcols * sizeof (short));
	grn_carry_below = (short *)G_malloc (pcols * sizeof (short));
	blu_carry_below = (short *)G_malloc (pcols * sizeof (short));
    }

    PRINTERWIDTH = Pnchars();
    BLOCKSIZE    = Pblocksize();
    BLOCKSPACE   = Pblockspace();
    NBLOCKS      = Pnblocks();
    TEXTSPACE    = Ptextspace();
    FUDGE        = Ptextfudge();

    cats_appearing = 0;
    total = 0;
    index = NULL;
    G_rewind_cell_stats (statf);
    while (G_next_cell_stat (&catnum, &count, statf))
    {
#ifdef SKIP0
	if (catnum == 0)
	    continue;
#endif
	cats_appearing++;
	index = (CELL *) G_realloc (index, cats_appearing * sizeof (CELL));
	index[cats_appearing-1] = catnum;
	total += count;
    }
    if (cats_appearing <= 0)
	return 0;

    if (total <= 0) total = 1.0;

    rows  = (cats_appearing + 1 + NBLOCKS - 1) / NBLOCKS ;
    lines = rows * BLOCKSIZE + 1;

    Palpha();
    Ptext("");

    Praster ();
    Ppictsize (lines, pcols);

    block = 0;
    for (r = 0;  r < rows; r++)
    {
	if (colormode == COLORMODE_DIFFUSION)
	{
	    G_zero (red_carry_below, pcols * sizeof (short)); 
	    G_zero (grn_carry_below, pcols * sizeof (short)); 
	    G_zero (blu_carry_below, pcols * sizeof (short)); 
	}

        nblocks = cats_appearing + 1 - block;
        if (nblocks > NBLOCKS) nblocks = NBLOCKS;
        Praster ();

/* top line of block outlines */

        Prle_begin();

        left = pcols;
        for (b = 0; b < nblocks; b++)
        {
            Prle(black, BLOCKSIZE);
            Prle(white, BLOCKSPACE);
            left -= BLOCKSIZE + BLOCKSPACE;
	    if (FUDGE > 0 && b%FUDGE)
	    {
		Prle(white, 1);
		left--;
	    }
        }

        Prle (white, left);
        Prle_end () ;

/* body of the blocks with left, right borders */

        for (rr=0; rr < BLOCKSIZE-2; rr++)
        {
	    int i,c,red,grn,blu;
            Prle_begin() ;

            left = pcols;
	    i = 0;
            for (b = 0; b < nblocks; b++)
            {
		red_carry_right = 0;
		grn_carry_right = 0;
		blu_carry_right = 0;

		if(b == nblocks-1)
		{
		    G_get_null_value_color(&red, &grn, &blu, pcolr);
		    if (colormode == COLORMODE_DIFFUSION)
		    {
			red += red_carry_right + red_carry_below[i];
			grn += grn_carry_right + grn_carry_below[i];
			blu += blu_carry_right + blu_carry_below[i];
			red_carry_below[i] = red_carry_right = red_carryover(red)/2;
			grn_carry_below[i] = grn_carry_right = grn_carryover(grn)/2;
			blu_carry_below[i] = blu_carry_right = blu_carryover(blu)/2;
		    }
		    else if (colormode == COLORMODE_DITHER)
		    {
			unsigned char x;
			x = red; red_dither (&x, rr, 0, 1); red = x;
			x = grn; grn_dither (&x, rr, 0, 1); grn = x;
			x = blu; blu_dither (&x, rr, 0, 1); blu = x;
		    }
		    i++;
	 	    c = printer_color_number (red,grn,blu);
		    Prle((unsigned char)c, 1);
		    break;
                }
                catnum = index[block+b];
		if(parms.map_type != CELL_TYPE)
		     name = G_get_ith_d_raster_cat(pcats, catnum, 
						&dmin, &dmax);

                Prle(black, 1);

		for (cc = 0; cc < BLOCKSIZE-2; cc++)
		{
		    if(parms.map_type != CELL_TYPE)
		    {
		        val = dmin + (double) cc * (dmax-dmin) / 
		 				(double) (BLOCKSIZE-2);
                        G_get_d_raster_color(&val,  &red, &grn, &blu, pcolr);
                    }
                    else
		        G_get_color ((CELL)catnum, &red, &grn, &blu, pcolr);
		    if (colormode == COLORMODE_DIFFUSION)
		    {
			red += red_carry_right + red_carry_below[i];
			grn += grn_carry_right + grn_carry_below[i];
			blu += blu_carry_right + blu_carry_below[i];
			red_carry_below[i] = red_carry_right = red_carryover(red)/2;
			grn_carry_below[i] = grn_carry_right = grn_carryover(grn)/2;
			blu_carry_below[i] = blu_carry_right = blu_carryover(blu)/2;
		    }
		    else if (colormode == COLORMODE_DITHER)
		    {
			unsigned char x;
			x = red; red_dither (&x, rr, cc, 1); red = x;
			x = grn; grn_dither (&x, rr, cc, 1); grn = x;
			x = blu; blu_dither (&x, rr, cc, 1); blu = x;
		    }
		    i++;

		    if(parms.map_type == CELL_TYPE)
		         c = lookup_from_pattern(catnum,rr,cc);
                    else
			 c = -1;
		    if (c < 0)
			c = printer_color_number (red,grn,blu);
		    Prle((unsigned char)c, 1);
		}

                Prle(black, 1);
                Prle(white, BLOCKSPACE);
                left -= BLOCKSIZE + BLOCKSPACE;
		if (FUDGE > 0 && b%FUDGE)
		{
		    Prle(white, 1);
		    left--;
		}
            }

            Prle (white, left);
            Prle_end () ;
        }

/* bottom line of block outlines */

        Prle_begin();

        left = pcols;
        for (b = 0; b < nblocks; b++)
        {
            Prle(black, BLOCKSIZE);
            Prle(white, BLOCKSPACE);
            left -= BLOCKSIZE + BLOCKSPACE;
	    if (FUDGE > 0 && b%FUDGE)
	    {
		Prle(white, 1);
		left--;
	    }
        }

        Prle (white, left);
        Prle_end () ;

/* numbers below the blocks */

        Palpha();
        *buf = 0;
        for (b = 0; b < nblocks; b++)
        {
            sprintf(temp1, "%3ld%*s", (long)index[block++], TEXTSPACE, "");
            strcat (buf, temp1);
        }
        Ptext (buf);
    }

/* print the category names */

#define CATNUM_WIDTH 4
#define INTER_SPACING 4
#define EXTRA CATNUM_WIDTH + INTER_SPACING + 1
#define MIN_DOTS     2



    Palpha();
    Ptext("");

    maxname = 0;
    max = 0;
    G_rewind_cell_stats (statf);
    while (G_next_cell_stat (&catnum, &count, statf))
    {
        register int t;

#ifdef SKIP0
	if (catnum == 0)
	    continue;
#endif
        if(parms.map_type == CELL_TYPE)
 	    name = G_get_cat (catnum, pcats);
        else
	    name = G_get_ith_d_raster_cat(pcats, catnum, &dmin, &dmax);
        if ((len = strlen(name)) > maxname)
            maxname = len;
#ifdef PERCENT_COVER
        sprintf(temp2,"(%.1f)", (count/total)*100);
        t = strlen(temp2)+len+MIN_DOTS;
#else
	t = len;
#endif
        if (t > max) max = t;
    }


    cols = cats_appearing < 5 ? 1 : PRINTERWIDTH / (max + EXTRA) ;
    if (cols == 0) cols = 1;
    rows = (cats_appearing + 1 + cols) / cols ;

    for (r = 0; r < rows; r++)
    {
	*buf = 0;
        for (c = 0; c < cols; c++)
        {
            b = r + c*rows;
            if (b >= cats_appearing)
	    {
		/* draw n-data box */
		name = "no data";
		strcat (buf, name);
		len = strlen(name);
		G_get_stats_for_null_value(&count, statf);
#ifdef PERCENT_COVER
                sprintf(temp2,"(%.1f)", (count/total)*100);
	        len += strlen(temp2);
                while(len++ < max)
                    strcat(buf,"_");
#else
	        *temp2 = 0;
                while(len++ < max)
                strcat(buf," ");
#endif
                sprintf(temp1,"%s%*s",temp2, INTER_SPACING,"");
	        strcat (buf, temp1);
                Ptext (buf);
		break;
            }

	    catnum = index[b];
            if(parms.map_type == CELL_TYPE)
                sprintf (temp1, "%*ld ", CATNUM_WIDTH, (long) catnum);
            else
	    {
		sprintf(dmin_str, "%.10f", dmin);
		G_trim_decimal(dmin_str);
		if(dmin!=dmax)
		{
		   sprintf(dmin_str, "%.10f", dmax);
		   G_trim_decimal(dmax_str);
		   sprintf(out_str, "%s-%s", dmin_str, dmax_str);
		   sprintf (temp1, "%*s ", CATNUM_WIDTH, out_str);
                }
		else
		   sprintf (temp1, "%*s", 10, dmin_str);
            }
	    strcat (buf, temp1);

	    G_find_cell_stat (catnum, &count, statf);

            if(parms.map_type == CELL_TYPE)
 	        name = G_get_cat (catnum, pcats);
            else
	        name = G_get_ith_d_raster_cat(pcats, catnum, &dmin, &dmax);
            strcat (buf, name);
            len = strlen(name);
#ifdef PERCENT_COVER
            sprintf(temp2,"(%.1f)", (count/total)*100);
	    len += strlen(temp2);
            while(len++ < max)
                strcat(buf,"_");
#else
	    *temp2 = 0;
            while(len++ < max)
                strcat(buf," ");
#endif
            sprintf(temp1,"%s%*s",temp2, INTER_SPACING,"");
	    strcat (buf, temp1);
        }
        Ptext (buf);
    }
#ifdef PERCENT_COVER
    Ptext("");
    Ptext ("numbers in parentheses () are percentage cover");
#endif

    Praster ();
    Prle_begin ();
    Prle (white, pcols);
    Prle_end ();

    return 0;
}
