#include "include.h"
#include "display.h"
#include "D.h"
#include <unistd.h>
#include "raster.h"
#include "externs.h"

int 
execute (void)
{
    CELL *final_buf ;
    CELL *finbufptr ;
    CELL *mapbuf ;
    CELL *mapbufptr ;
    char *mapset ;
    char buff[128] ;
    char name[20] ;
    char saving ;
    double normal_factor ;
    CELL *outbufptr ;
    CELL *result_buf ;
    int fildes[MAX_MAPS] ;
    long *weights[MAX_MAPS] ;
    long *w;
    long weight;
    long min_weight[MAX_MAPS];
    long max_weight[MAX_MAPS];
    long min_value, max_value;
    CELL min_cat[MAX_MAPS];
    CELL max_cat[MAX_MAPS];
    CELL min, max, cat;
    int can_be_negative;
    int map ;
    int map_file ;
    int mod_row, mod_col ;
    int row, col ;
    int screen_row ;
    int t, b, l, r ;
    struct Cell_head window ;
    struct Colors colors ;
    int n, ncats;
    struct Histogram *histo;

/* Display current analysis to user */
    fprintf (stdout,"\nEXECUTE Step I: Display analysis request:\n") ;
    list_analysis(0) ;
    fprintf (stdout,"\nHit <RETURN> to continue ->") ;
    GETS ;

/* get window */
    G_get_window(&window) ;
    G_init_colors (&colors);	/* so G_free_colors() can be called anytime*/

/*
 * Open map files
 * Convert histogram into a weight array
 * Find min/max weights as well
 */
    can_be_negative = 0;
    for(map=0; map<MAX_MAPS; map++)
    {
	if (!mapdef[map].used)
	    continue;
	fildes[map] = G_open_cell_old(mapdef[map].name,mapdef[map].mapset) ;

	histo = &mapdef[map].histo;
	G_sort_histogram (histo);
	ncats = G_get_histogram_num (histo);

	min = 0;
	max = 0;
	for (n = 0; n < ncats; n++)
	    if (G_get_histogram_count (n, histo))
		break;
	if (n < ncats)
	    min = G_get_histogram_cat (n, histo);
	for (n = ncats-1; n >= 0; n--)
	    if (G_get_histogram_count (n, histo))
		break;
	if (n >= 0)
	    max = G_get_histogram_cat (n, histo);
	min_cat[map] = min;
	max_cat[map] = max;
	w = weights[map] = (long *) G_calloc ((int) (max-min+1), sizeof(long));
	for (cat = min; cat <= max; cat++)
	    w[cat-min] = 0;

	for (n = 0; n < ncats; n++)
	{
	    cat = G_get_histogram_cat (n, histo);
	    weight = G_get_histogram_count (n, histo);
	    if (cat >= min && cat <= max)
		w[cat-min] = weight;
	    if (analysis_type != ADD && weight < 0)
	    {
		weight = -weight;
		can_be_negative = 1;
	    }
	    if (n==0)
		min_weight[map] = max_weight[map] = weight;
	    else if (weight < min_weight[map])
		min_weight[map] = weight;
	    else if (weight > max_weight[map])
		max_weight[map] = weight;
	}
    }

/* allocate buffer space */
    mapbuf = G_allocate_cell_buf() ;
    final_buf = G_allocate_cell_buf() ;
    result_buf = G_allocate_cell_buf() ;


/* Find max and min possible values */
    fprintf (stdout,"\nEXECUTE Step II: Calculate the possible min and max weights:\n") ;
    min_value = (analysis_type == ADD) ? 0 : 1 ;
    max_value = (analysis_type == ADD) ? 0 : 1 ;


    for (map=0; map < MAX_MAPS; map++)
    {
	if (! mapdef[map].used)
	    continue;
	if (analysis_type == ADD)
	{
	    min_value += min_weight[map];
	    max_value += max_weight[map];
	}
	else
	{
	    min_value *= min_weight[map];
	    max_value *= max_weight[map];
	}
    }
    if ((analysis_type != ADD) && can_be_negative)
    {
	if (min_value < max_value)
	    min_value = -max_value;
	else
	{
	    max_value = min_value;
	    min_value = -min_value;
	}
    }
    fprintf (stdout,"\n    Minimum possible total weight = %ld\n", min_value) ;
    fprintf (stdout,  "    Maximum possible total weight = %ld\n\n", max_value) ;
    fprintf (stdout,  "    This results in a possible range of %ld categories\n\n", max_value-min_value+1) ;

    while (1) 
    {
	normalize = 0;
	fprintf (stdout,"   Hit RETURN to accept this default, or enter a number greater\n") ;
	fprintf (stdout,"   than zero to specify an alternate number of potential categories > ") ;
	GETS;
	if (*input_buf == 0)
		break;
	if(sscanf(input_buf,"%ld", &normalize) != 1)
		continue;
	if (normalize>0)
		break ;
    }

    if (normalize)
	normal_factor = (double)normalize / (double)(max_value - min_value + 1) ;

    fprintf (stdout,"\nEXECUTE Step III: Get Output File Name:\n") ;
/* Get a new map name to store results */

    G_set_ask_return_msg("just see analysis results and not save the results") ;
    mapset = G_ask_cell_new("Enter new map name", name) ;

    if (mapset == NULL)
    {
	fprintf (stdout,"\nResults will be displayed only\n\n") ;
	sleep(1) ;
	saving = 0 ;
    }
    else
    {
	if ( (map_file = G_open_cell_new(name) ) == -1)
	{
	    fprintf (stdout,"    error: can't open cell file %s\n", name) ;
	    G_free(mapbuf) ;
	    G_free(result_buf) ;
	    G_free(final_buf) ;
	    return(-1) ;
	}
	fprintf (stdout,"\nResults will be saved in %s\n\n", name) ;
	saving = 1 ;
    }

    fprintf (stdout,"\nEXECUTE Step IV: Do the Analysis:\n") ;

/* Set up graphics if on console */
    if (at_console())
    {
	R_color (D_translate_color("black")) ;
	D_erase_window() ;
	if (normalize)
	    make_colors (&colors, (CELL)0, (CELL)normalize) ;
	else
	    make_colors (&colors, (CELL)min_value ,(CELL)max_value) ;
	D_set_colors(&colors);
	D_get_screen_window(&t, &b, &l, &r) ;
	D_cell_draw_setup(t, b, l, r) ;
	R_flush();
    }
/* else scale the tty map */
    else
    {
	mod_row = (window.rows + 1)/(20) + 1 ;  /* about 4 */ 
	mod_col = (window.cols   + 1)/(70) + 1 ;  /* about 4 */
    }


/* Setup to catch interrupt signal */
    set_signals(1) ;
    signalflag.interrupt = 0 ;
    fprintf (stdout,"     Hit %s to abort\n", G_unctrl(G_intr_char())) ;

/* Do the requested analysis */

/* LOOP for ROWS   */
    for (screen_row=0, row=0; row<window.rows; row++)
    {
	if(! saving)
	{
	    row = screen_row ;
	    if (row < 0 || row >= window.rows)
		break ;
	}

	if (signalflag.interrupt) 
	    break ;

/* Clear answer buffer */
	outbufptr = result_buf ;
	switch (analysis_type)
	{
	case ADD:
		for (col=0; col<window.cols; col++, outbufptr++)
			*outbufptr = 0 ;
		break ;
	case MULT:
		for (col=0; col<window.cols; col++, outbufptr++)
			*outbufptr = 1 ;
		break ;
	}

/* LOOP for MAPS  */
	for(map=0; map<MAX_MAPS; map++)
	{
	    if (!mapdef[map].used)
		continue;

	/* LOOP for COLS */
	    if (G_get_map_row(fildes[map], mapbuf, row)  < 0)
	    {
		sprintf(buff,"Reading row [%d] of [%s] in [%s]",
			row, mapdef[map].name, mapdef[map].mapset) ;
		G_fatal_error(buff) ;
	    }

	    outbufptr = result_buf ;
	    mapbufptr = mapbuf ;
	    min = min_cat[map];
	    max = max_cat[map];
	    w = weights[map];

	    switch(analysis_type)
	    {
	    case ADD:
		col = window.cols;
		while (col-- > 0)
		{
		    cat = *mapbufptr++;
		    if (cat >= min && cat <= max)
			*outbufptr += w[cat-min];

		    *outbufptr++;
		}
		break ;
	    case MULT:
		col = window.cols;
		while (col-- > 0)
		{
		    cat = *mapbufptr++;
		    if (cat >= min && cat <= max)
			*outbufptr *= w[cat-min];
		    else
			*outbufptr = 0;
		    *outbufptr++;

		}
		break ;
	    }
	}

/* adjust results so that lowest possible result = 0 */
/*
	outbufptr = result_buf ;
	for (col=0; col<window.cols; col++, outbufptr++)
	    *outbufptr -= min_value ;

*/
/* normalize if necessary */
	outbufptr = result_buf ;
	finbufptr = final_buf ;
	col = window.cols;
	if (normalize)
	{
	    while (col-- > 0)
		*finbufptr++ = (CELL)(*outbufptr++ * normal_factor) ;
	}
	else
	{
	    while (col-- > 0)
		*finbufptr++ = (CELL)(*outbufptr++) ;
	}

/* print out some results */
	if (at_console()) 
	{
	    screen_row = D_draw_cell(row, final_buf, &colors) ;
	    R_flush() ;
	}
	else
	{
	    if(!(row%mod_row))
	    {
		finbufptr = final_buf ;
		for(col = 0 ; col <= window.cols ; col++, finbufptr++)
		{
		    if(!(col%mod_col))
			fprintf (stdout,"%d", ((int)(*finbufptr))%10) ;
		}
		fprintf (stdout,"\n") ;
	    }
	    screen_row = (row/mod_row + 1) * mod_row ;
	}

/* print analysis row out if saving */
	if (saving)
	    G_put_raster_row(map_file, final_buf, CELL_TYPE);
    }

    for(map=0; map<MAX_MAPS; map++)
    {
	if (!mapdef[map].used) continue;
	G_close_cell(fildes[map]) ;
	G_free (weights[map]);
    }

    if(saving)
    {
	if (signalflag.interrupt) 
	    G_unopen_cell (map_file);
	else
	{
	    G_close_cell(map_file) ;
	    write_supp(name, mapset) ;
	}
    }

    G_free_colors (&colors);
    G_free(mapbuf) ;
    G_free(result_buf) ;
    G_free(final_buf) ;
    set_signals(0) ;
    return(0) ;
}
