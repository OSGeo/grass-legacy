#include <stdio.h>
#include <ctype.h>
#include "digit.h"
#include "btree.h"
#include "ism.h"

#ifndef LINE_LABELED
#define LINE_LABELED(p) (LINE_ALIVE(p) && (p)->att)
#endif

char *malloc ();

int intcmp ();

static int Line_lines = 0;	/* number of line lines        */
static int Labelled_lines = 0;	/* number of labelled lines    */
static int Polygons = 0;	/* number of polygons          */
static int Labelled_polys = 0;	/* number of labelled polygons */

static struct lineray *larray=NULL; /* pointer to array of info from tree */

struct lineray *line_tree2array ();


/*
**
**  Btree is referenced by Category.
**  Data value is initially a number that is sequential and unique for
**   each category.  if the automatic assignment option is selected, 
**   then this will be used to generate a unique color/style for each
**   category.  Otherwise it will not be used.
**   After the color/style/(size) is selected, these will be stored
**   in byte fields within the data value, specified by the above defines.
*/
line_types (map, out)
    struct Map_info *map;
    FILE *out;
{
    int i, cnt=0;
    BTREE B;
    int *res;
    int data = 0;
    P_ATT *Att;

    /*printf ("Working on lines\n");*/

    if (!map->n_lines)
	return (0);

    btree_create (&B, intcmp, 50);	/* init BTree */

    for (i = 1 ; i <= map->n_atts ; i++)
    {
	Att = &(map->Att[i]);
	if (ATT_ALIVE (Att) && Att->type == LINE)
	{
	    if (!btree_find (&B,&Att->cat,&res))
	    {
		data++;		/* increment the unique identifier */
		btree_update (&B, &(Att->cat), sizeof (Att->cat), 
				&data, sizeof (data));
	    }
	    cnt++;
	}
    }

    Labelled_lines = data;
    
    ask_lines (map, out, &B, Labelled_lines);

    btree_free (&B);
}


#define NUM_COLORS 8
#define NUM_STYLES 9	/* ignore railroads */
#define NUM_PATTERNS 21

ask_lines (map, out, B, labeled)
    struct Map_info *map;
    FILE *out;
    BTREE *B;
{
    int color, style, size;
    int cat, *catp, data, *datap;
    unsigned char *p;
    int num;
    struct P_line *Line;
    int prev;
    int i, j;

    if (labeled)
    {
	/*
	if (pnl_yesno ("Enter your own values?"))
	    goto user_edit;
	    */
	
	btree_rewind (B);
	p = (unsigned char *) &data;
	if (labeled <= NUM_COLORS)	/* 1to1 mapping of colors */
	{
	    while (btree_next (B, &catp, &datap))
	    {
		cat = *catp;
		num = *datap;
		p[LINE_COLOR] = num;
		p[LINE_STYLE] = num;
		p[LINE_WEIGHT] = 1;
		p[LINE_YN] = 0;
		btree_update (B, &cat, sizeof (cat), &data, sizeof (data));
	    }
	}
	else
	{
	    while (btree_next (B, &catp, &datap))
	    {
		num = *datap;
		cat = *catp;
		p[LINE_COLOR] = rand()%(NUM_COLORS-1) + 1;	/* no zero */

		/* just use one style by default */
		/*p[LINE_STYLE] = rand()%(NUM_STYLES-1) + 1;*/
		p[LINE_STYLE] = 1;

		p[LINE_WEIGHT] = 1;
		p[LINE_YN] = 0;
		btree_update (B, &cat, sizeof (cat), &data, sizeof (data));
	    }
	}


	larray = line_tree2array (B, labeled);
user_edit:

	if (larray == NULL)
	{
	    larray = (struct lineray *) 
		malloc (labeled * sizeof (struct lineray));
	}
	    
	if (User_Entry)
	    do_user_line_panel (map, larray, labeled);

	/*
	** and finally write it out
	*/
	for (i = 1 ; i <= map->n_lines ; i++)
	{
	    Line = &(map->Line[i]);
	    if (Line->type != LINE)
		continue;

	    if (!LINE_LABELED (Line))	/* TODO */
		continue;

#ifdef OLD
	    if (!btree_find (B,&(map->Att[Line->att].cat),&datap))
	    {
/*DEBUG...*/ fprintf (stderr, "Cat NOT found: %d\n", map->Att[Line->att].cat);
		continue;
	    }
	    p = (unsigned char *) datap;
#endif

	    for (j = 0 ; j < labeled ; j++)
		if (map->Att[Line->att].cat == larray[j].att)
		    break;
	    if (j >= labeled)
	    {
		fprintf(stderr, "Cat NOT found: %d\n", map->Att[Line->att].cat);
		continue;
	    }
	    p = (unsigned char *) &(larray[j].flags);
		
	    color = p[LINE_COLOR];
	    style = p[LINE_STYLE];
	    size = p[LINE_WEIGHT];

	    fprintf (out, "SETPEN %d %d 1\n", color, color);
	    fprintf (out, "SRFLNE %d %d 0\n", style, size);
	    dump_line (out, map, i, G_get_cat (larray[j].att, &Cats));
	}
    }
    else
    {
	if (!map->n_llines || !pnl_yesno ("None of the lines are labelled.  Use them anyway?"))
	    return 0;
	/* 
	style = prompt_val (1, 10, 1, "Enter line STYLE [1-10](default 1):");
	color = prompt_val (1,  8, 1, "Enter line COLOR [1-8] (default 1):");
	style = 1;
	color = 1;
	*/
	ask_line_nolabel (map, &style, &color, &size);

	fprintf (out, "SETPEN %d %d 1\n", color, color);
	for (i = 1 ; i <= map->n_lines ; i++)
	{
	    if (map->Line[i].type != LINE)
		continue;
	    fprintf (out, "SRFLNE %d %d 0\n", style, size);
	    dump_line (out, map, i, "");
	}
    }
}

#define WITHIN(low,mid,hi) ((low) <= (mid) && (mid) <= (hi))

/*
** prompt for positive value
**  returns value or -1 on no answer
*/
prompt_val (low, high, def, str)
    int low, high, def;
    char *str;
{
    char buf[BUFSIZ];
    int num;

    while (1)
    {
	printf (str);
	gets (buf);
	G_squeeze (buf);

	if (*buf == 0 && WITHIN (low, def, high))
	    return def;

	if (!isdigit (*buf))
	    goto oops;

	num = atoi (buf);
	if (WITHIN (low, num, high))
	    return num;
oops:
	printf ("\nPlease try again:\n\n");
    }
}

yesno (str)
    char *str;
{
    char buf[BUFSIZ];

    while (1)
    {
	printf (str);
	gets (buf);
	G_squeeze (buf);
	switch (*buf) {
	    case 'y':
	    case 'Y':
		return 1;
		break;
	    case 'n':
	    case 'N':
		return 0;
		break;
	}
	printf ("\nPlease try again\n\n");
    }
}

#ifdef FOO2
    btree_create (&B, strcmp, 10);
    if (btree_find (&B,key,&d))
    btree_update (&B, key, strlen(key)+1, data, strlen(data)+1);
    btree_rewind (&B);
    while (btree_next (&B, &k, &d))
    btree_free (&B);
#endif

intcmp (a, b)
    int *a, *b;
{
    if (*a < *b)
	return -1;
    if (*a > *b)
	return 1;
    return 0;
}


struct lineray *
line_tree2array (B, labeled)
    BTREE *B;
    int labeled;
{
    struct lineray * p;
    int *k, *d;
    int i;

    p = (struct lineray *) malloc (labeled * sizeof (struct lineray));
    if (NULL == p)
	fprintf (stderr, "Out of memory\n"), exit (1);

    btree_rewind (B);
    i = 0;
    while (btree_next (B, &k, &d))
    {
	if (i >= labeled)
	{
	    i++; /* force error mesg */
	    break;
	}
	p[i].att = *k;
	p[i].flags = *d;
	i++;
    }
    if (i != labeled)
	fprintf (stderr, "tree2array: COUNT MIS-MATCH %d %d\n", labeled, i);

    return (p);
}
