static char rcsid[]="$Header$";
#include <string.h>
#include "rule.h"

int reclass (char *old_name, char *old_mapset, char *new_name,
    RULE *rules, struct Categories *cats, char *title, struct Reclass *new)
{
    struct History hist;
    int is_reclass;
    FILE *fd;
    char buf[256];

	strcpy (new->name, old_name);
	strcpy (new->mapset, old_mapset);
	_reclass (rules, cats, new);


    if (title == NULL)
	sprintf (title = buf, "Reclass of %s in %s", new->name, new->mapset);
    G_set_cats_title (title, cats);
    if (G_write_vector_cats (new_name, cats) == -1)
    {
	sprintf (buf, "%s - unable to create category file", new_name);
	G_fatal_error (buf);
	exit(1);
    }
    G_free_cats (cats);

    return 0;
}

int _reclass (RULE *rules, struct Categories *cats, struct Reclass *new)
{
    RULE *r;
    register CELL i;
    register CELL n;
    int first;
    long num;

/* first find the min,max cats */
    first = 1;
    for (r = rules; r; r = r->next)
    {
	for (i = r->lo; i <= r->hi; i++)
	{
	    n = i;
    /* assign min,max */
	    if (first)
	    {
		new->min = new->max = n;
		first = 0;
	    }
	    else
	    {
		if (n < new->min) new->min = n;
		if (n > new->max) new->max = n;
	    }
	}
    }

 /* make sure we have at least one entry */
    if (first) new->min = new->max = 0;

 /* allocate reclass table */
    new->type = RECLASS_TABLE;
    num = new->max - new->min + 1;
    if (num != (int) num)		/* make sure don't overflow int */
	G_fatal_error ("Too many categories");

    new->num = num;
    new->table = (CELL *) G_calloc (new->num, sizeof(CELL));
    for (i = 0; i < new->num; i++)
	new->table[i] = 0;

/* generate the new reclass lookup table */
    for (r = rules; r; r = r->next)
	for (i = r->lo; i <= r->hi; i++)
	{
	    n = i;
	    new->table[n-new->min] = r->new;
	    if (cats->num < r->new)
		cats->num = r->new;
	}
/*    for (i = 0; i < new->num; i++)
	fprintf (stdout,"i=%d new=%d\n",i,new->table[i]);
    fprintf (stdout, "new->min = %d new-> max = %d \n", new->min, new->max);*/

    return 0;
}
