#include <string.h>
#include "gis.h"
#include "site.h"

/* site list manipulation routines
 *
 * initialize_site_list:   marks site list as empty
 * free_site_list:         releases all storage allocated for the list
 * add_site:               adds a new site to the list
 * rewind_site_list:       moves pointer to first site in list
 * next_site:              get info for next site , advance pointer
 */

int add_site (SITE_LIST *site_list, double north,double east, char *desc)
{
    SITE *new;

    new = (SITE *) G_malloc (sizeof(SITE));
    new -> north = north;
    new -> east  = east;
    new -> desc  = G_malloc (strlen(desc)+1);
    strcpy (new->desc, desc);
    new -> next  = 0;

    if (site_list -> last)
	site_list -> last -> next = new;
    site_list -> last = new;
    if ( ! site_list -> first)
	site_list -> first = new;

    return 0;
}

int initialize_site_list (SITE_LIST *site_list)
{
    site_list -> first = 0;
    site_list -> last  = 0;
    site_list -> cur   = 0;
    site_list -> name[0] = 0;
    site_list -> desc[0] = 0;

    return 0;
}

int rewind_site_list(SITE_LIST *site_list)
{
    site_list -> cur = site_list -> first;

    return 0;
}

int next_site (SITE_LIST *site_list, double *north, double *east, char **desc)
{
    if ( ! site_list -> cur)
	return 0;

    *north  = site_list -> cur -> north;
    *east   = site_list -> cur -> east;
    *desc   = site_list -> cur -> desc;

/* advance the list */
    site_list -> cur = site_list -> cur -> next;

    return 1;
}

int free_site_list(SITE_LIST *site_list)
{
    while (site_list -> first)
    {
	site_list -> cur = site_list -> first -> next;
	G_free (site_list -> first -> desc);
	G_free (site_list -> first);
	site_list -> first = site_list -> cur;
    }
    site_list -> last = 0;

    return 0;
}
