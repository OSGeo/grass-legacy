/**** alloc.c ****/
/*
**  Written by David Gerdes  Fall 1992
**  US Army Construction Engineering Research Lab
**  Copyright  David Gerdes  USA-CERL  1992
*/


#include "cutter.h"

/* allocate room for  'num'   struct point_t w/in struct poly_info
**   returns -1 on out of memory 
*/
alloc_poly_t (poly, n_polys, alloc_points)
    struct poly_t *poly;
    int n_polys;
    int alloc_points;  /* Hack, for label.c */
{
    register int i;
    int alloced;
    char *p;
    char old_num;

    old_num = poly->n_polys;

    if (n_polys <= old_num)
	return 0;

    alloced = poly->n_alloced;
    /* alloc_space will just return if no space is needed */
    if(!(p = 
        dig__alloc_space(n_polys, &alloced, 10, (char *) poly->spoly,
        sizeof(struct sub_poly))))
    {
	return (dig_out_of_memory ());
    }
    poly->spoly = (struct sub_poly *) p;
    
    poly->n_alloced = alloced;

    /* initialize each new allocated sub_poly */
    if (n_polys > old_num)
    {
	for (i = old_num ; i < n_polys ; i++)
	{
	    poly->spoly[i].Points = alloc_points ? Vect_new_line_struct():NULL;
	    poly->spoly[i].info = NULL;
	    poly->spoly[i].num = 0;
	    poly->spoly[i].n_alloced = 0;
	}
    }

    return 0;
}


alloc_poly_t_lines (spoly, n_lines)
    struct sub_poly *spoly;
    int n_lines;
{
    int alloced;
    char *p;

    alloced = spoly->n_alloced;
    /* alloc_space will just return if no space is needed */
    if(!(p = 
        dig__alloc_space(n_lines, &alloced, 20, (char *) spoly->info,
        sizeof(struct poly_info))))
    {
	return (dig_out_of_memory ());
    }
    spoly->info = (struct poly_info *) p;
    
    spoly->n_alloced = alloced;

    return 0;
}



/* returns 0 or -1 on out of memory */
alloc_intersections (ip, num)
    struct intersects *ip;
    int num;
{
    int alloced;
    char *p;

    alloced = ip->n_alloced;
    /* alloc_space will just return if no space is needed */
    if(!(p = 
        dig__alloc_space(num, &alloced, 20, (char *) ip->points,
        sizeof(struct ipoints))))
    {
	return (dig_out_of_memory ());
    }
    ip->points = (struct ipoints *) p;
    
    ip->n_alloced = alloced;

    return 0;
}
