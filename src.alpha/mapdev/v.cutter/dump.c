/**** dump.c ****/
/*
**  Written by David Gerdes  Fall 1992
**  US Army Construction Engineering Research Lab
**  Copyright  David Gerdes  USA-CERL  1992
*/


#include "cutter.h"

dump_table (table)
    struct t_data *table;
{
    struct t_data *p;

fprintf (stderr, "(A_POLY[LINE][SEG] L_ndx Isle Dir)(B_POLY[LINE][SEG] L_ndx Isle Dir) (x,Y) INTR IN_OUT\n");

    p = table->next;
    while (p)
    {
	dump_table_entry (p);
	p = p->next;
    }
}

dump_table_entry (p)
    struct t_data *p;
{

fprintf (stderr,"(%3d[%2d][%2d]%3d %1d %2d) (%3d[%2d][%2d]%3d %1d %2d) (%9.2lf,%9.2lf) %d %d\n",
    p->i[0].poly, p->i[0].line, p->i[0].segment,
    p->i[0].l_index, p->i[0].subpoly, p->i[0].dir,
    p->i[1].poly, p->i[1].line, p->i[1].segment,
    p->i[1].l_index, p->i[1].subpoly, p->i[1].dir,
    p->x, p->y, p->inter, p->in_out);
}

/*
(A_POLY[LINE][SEG] L_index Dir)(B_POLY[LINE][SEG] L_index Dir) (x,Y) INTR IN_OUT
*/
