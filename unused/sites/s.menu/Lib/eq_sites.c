#include "gis.h"
#include "site.h"

int equal_sites (SITE_LIST *a,SITE_LIST *b)
{
    int equal;


    rewind_site_list (a);
    rewind_site_list (b);

    equal = cmp_sites (a, b);

    rewind_site_list (a);
    rewind_site_list (b);

    return equal;
}

int cmp_sites (SITE_LIST *a,SITE_LIST *b)
{
    double a_north, a_east;
    double b_north, b_east;
    char *a_desc;
    char *b_desc;

    if (strcmp(b->name, a->name) != 0)
	return 0;
    if (strcmp(b->desc, a->desc) != 0)
	return 0;
    while (next_site (a, &a_north, &a_east, &a_desc))
    {
	if (!next_site (b, &b_north, &b_east, &b_desc))
	    return 0;
	if (a_north != b_north) return 0;
	if (a_east  != b_east ) return 0;
	if (strcmp (a_desc, b_desc) != 0)
		return 0;
    }
    return next_site (b, &b_north, &b_east, &b_desc) == 0;
}
