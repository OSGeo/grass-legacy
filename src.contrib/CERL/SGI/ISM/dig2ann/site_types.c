#include <stdio.h>
#include <ctype.h>
#include "digit.h"
#include "btree.h"
#include "ism.h"


/*TODO   need to provide polygon holes also */



static int Labelled_sites = 0;	/* number of labelled sites    */
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
site_types (map, out)
    struct Map_info *map;
    FILE *out;
{
    int i, cnt=0;
    BTREE B;
    int *res;
    int data = 0;
    P_ATT *Att;

    /*printf ("Working on Sites.\n");*/

    btree_create (&B, intcmp, 50);	/* init BTree */

    for (i = 1 ; i <= map->n_atts ; i++)
    {
	Att = &(map->Att[i]);
	if (ATT_ALIVE (Att) && Att->type == DOT)
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

    Labelled_sites = data;
    
    ask_sites (map, out, &B, Labelled_sites);

    btree_free (&B);
}


#define NUM_COLORS   MAX_SITE_COLOR

ask_sites (map, out, B, labeled)
    struct Map_info *map;
    FILE *out;
    BTREE *B;
{
    int color, pattern, size, post;
    int cat, *catp, data, *datap;
    unsigned char *p;
    int num;
    struct P_line *Line;
    int prev;
    int i, j;

    if (labeled)
    {
	/*
	if (yesno ("Enter your own values?"))
	    return 0;
	*/
	
	btree_rewind (B);
	p = (unsigned char *) &data;
	if (labeled <= MAX_SITE_SYMBOL)	/* 1to1 mapping of patterns */
	{
	    while (btree_next (B, &catp, &datap))
	    {
		cat = *catp;
		num = *datap;
		p[SITE_COLOR] = (num%NUM_COLORS)+1;
		p[SITE_SYMBOL] = num;
		p[SITE_SIZE] = 1;
		p[SITE_YN] = 0;
		btree_update (B, &cat, sizeof (cat), &data, sizeof (data));
	    }
	}
	else
	{
	    while (btree_next (B, &catp, &datap))
	    {
		num = *datap;
		cat = *catp;
		p[SITE_COLOR] = (rand()%NUM_COLORS)+1;
		p[SITE_SYMBOL] = (rand()%MAX_SITE_SYMBOL)+1;
		p[SITE_SIZE] = 1;
		p[SITE_YN] = 0;
		btree_update (B, &cat, sizeof (cat), &data, sizeof (data));
	    }
	}

	larray = line_tree2array (B, labeled);

        if (larray == NULL)
        {
            larray = (struct lineray *)
                malloc (labeled * sizeof (struct lineray));
        }

	if (User_Entry)
	    do_user_site_panel (map, larray, labeled);


	/*
	** and finally display it
	*/
	for (i = 1 ; i <= map->n_lines ; i++)
	{
	    Line = &(map->Line[i]);

	    if (Line->type != DOT || !LINE_LABELED (Line))	/* TODO */
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


	    color = p[SITE_COLOR];
	    pattern = p[SITE_SYMBOL];
	    size  = p[SITE_SIZE];
	    post  = p[SITE_YN];

	    fprintf (out, "SETPEN %d %d 1\n", color, color);
	    fprintf (out, "SRFSYM %d %d %f\n", pattern, size, 6, 0.);
	    dump_site (out, map, i, G_get_cat (larray[j].att, &Cats));
	}
    }
    else
    {
	int cnt = 0;

	for (i = 1 ; i <= map->n_lines ; i++)
	{
	    Line = &(map->Line[i]);
	    if (Line->type == DOT)
		cnt++;
	}
	if (!cnt)
	    return 0;

	if (!pnl_yesno ("None of the sites are labelled.  Use them anyway?"))
	    return 0;

	ask_site_nolabel (map, &pattern, &color, &size);

	/*
	pattern= prompt_val (0, 21, 0, "Enter site SYMBOL [1-33](default 1):");
	color = prompt_val (1,  8, 1, "Enter site COLOR [1-8] (default 1):");
	*/

	fprintf (out, "SETPEN %d %d 1\n", color, color);
	for (i = 1 ; i <= map->n_lines ; i++)
	{
	    Line = &(map->Line[i]);
	    if (Line->type != DOT)
		continue;

	    fprintf (out, "SRFSYM %d %d %d %f\n", pattern, size, 6, 0.);
	    dump_site (out, map, i, "\"\"");
	}
    }
}
