/**** init.c ****/
/*
**  Written by David Gerdes  Fall 1992
**  US Army Construction Engineering Research Lab
**  Copyright  David Gerdes  USA-CERL  1992
*/


#include "cutter.h"

/* this code manages the mem allocation for the table structures.  */

/*
**  Note that this is only designed to be called once in a program's life
*/

struct table_base *
table_init ()
{
    struct table_base *T;

    T = (struct table_base *) G_malloc (sizeof (struct table_base));

    T->t_link = (char *) link_init (sizeof (struct t_data));

    T->Table = (struct t_data *) link_new(T->t_link);
    T->Table->next = NULL;

    return T;
}

struct t_data *
table_new (T)
    struct table_base *T;
{
    struct t_data *p, *t;

    p = (struct t_data *) link_new (T->t_link);

#ifdef FOO2
    /* I don't know why this was here, other than to make 
    ** the debug output look pretty.  It is MUCH more efficient to
    ** build the table from the near end than the far end of the list.
    */

    for (t = T->Table ; t->next != NULL ; t = t->next)
	;

    t->next = p;
    p->next = NULL;
#else
    p->next = T->Table->next;
    T->Table->next = p;
#endif

    return p;
}

table_cleanup (T)
    struct table_base *T;
{
    link_cleanup (T->t_link);
}

cutter_init (Map)
    struct Map_info *Map;	/* Cutter Map_info */
{
    int i;

    P_AREA *Area;

    TPoints = Vect_new_line_struct ();


    /*
    ** make sure that Cutter_bbox includes all polygons w/in cutter map
    */
    for (i = 1 ; i <= Map->n_areas ; i++)
    {
	Area = &(Map->Area[i]);

	if (i == 1)
	{
	    Cutter_bbox.N = Area->N;
	    Cutter_bbox.S = Area->S;
	    Cutter_bbox.E = Area->E;
	    Cutter_bbox.W = Area->W;
	    continue;
	}

	if (Area->N < Cutter_bbox.N) Cutter_bbox.N = Area->N;
	if (Area->S < Cutter_bbox.S) Cutter_bbox.S = Area->S;
	if (Area->E < Cutter_bbox.E) Cutter_bbox.E = Area->E;
	if (Area->W < Cutter_bbox.W) Cutter_bbox.W = Area->W;
    }
}
