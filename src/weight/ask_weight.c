/*
 * Section of weight that prompts for weights
 */

#include "include.h"

#define NLINES 10

ask_weights (name)
    char *name;
{
    struct Histogram *histo ;
    struct Categories pcats ;
    long cat_num[NLINES] ;
    long weight[NLINES] ;
    char label[NLINES][80] ;
    char next[20];
    char next_line[80];
    int i ;
    int atnum ;
    int line;
    int first ;
    int last ;
    int ncats;
    int map;
    CELL cat;
    long x;

    for (map = 0; map < MAX_MAPS; map++)
	if (strcmp (name, mapdef[map].name) == 0)
		break;
    if (map >= MAX_MAPS)
    {
	printf ("    map <%s> not chosen yet\n", name);
	return -1;
    }
    if (G_read_cats (name, mapdef[map].mapset, &pcats) < 0)
    {
	printf ("    error:  <%s> category file not available\n", name);
	return -1;
    }
    histo = &mapdef[map].histo;

    first = 0;
    ncats = G_get_histogram_num (histo);

    while (first >= 0 && first < ncats) 
    {
	V_clear() ;
	V_line ( 0, "   ENTER WEIGHTS FOR THESE CATEGORIES" ) ;
	V_line ( 2, "   CATEGORY NAME                                                    WEIGHT" ) ;

	last = first+NLINES <= ncats ? first+NLINES : ncats ;

	atnum = 0;
	line = 5;
	for (i=first; i < last; i++) {
	    cat_num[atnum] = cat = G_get_histogram_cat (i, histo);
	    weight[atnum] = G_get_histogram_count (i, histo);
	    dots (G_get_cat(cat,&pcats), label[atnum], 65);
	    V_line  (line, label[atnum]) ;
	    V_const (&cat_num[atnum], 'l', line, 68, 5) ;
	    V_ques  (&weight[atnum], 'l', line, 74, 5) ;
	    atnum++;
	    line++;
	}

	line += 2;
	*next = 0;
	if (last >= ncats || last < 0)
	    strcpy (next, "end");
	else
	    sprintf (next, "%ld", (long) G_get_histogram_cat (last, histo));

	sprintf (next_line, "%26s%5s  (highest %ld)",
		"Next category: ","",
		(long) G_get_histogram_cat (ncats-1, histo));
	V_line (line, next_line);
	V_ques (next, 's', line, 26, 5);

	V_intrpt_ok();
	if(!V_call())
	    break;

	atnum = 0;
	for (i = first; i < last; i++)
	{
	    cat = G_get_histogram_cat (i, histo);
	    G_set_histogram (cat, weight[atnum++], histo);
	}

	if (*next == 0) break;
	if (strcmp (next, "end") == 0) break;
	if (sscanf (next, "%ld", &x) != 1)
		continue;
	for (last = 0; last < ncats-1; last++)
	{
	    if (x <= G_get_histogram_cat (last, histo))
		break;
	}
	first = last ;
    }
    G_free_cats (&pcats);
    G_sort_histogram (histo);
}
