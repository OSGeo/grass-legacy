/*************************************************************************
 *
 *  G__mapset_name(n)
 *      given the index, return the char string name of the n'th mapset
 *      from the mapset_name[] list. The first call will initialize
 *      the list.
 *
 * Internal routines
 *
 *   get_list_of_mapsets()
 *      sets up the mapset_name[] list from the mapset
 *
 * Data structures
 *   mapset_name[]  list of mapset names
 *   nmapset        number of names in the above list
 *
 *************************************************************************/

#include <string.h>
#include "gis.h"

static char **mapset_name ;
static char **mapset_name2 ;
static int nmapset = 0;
static int nmapset2 = 0;
static int new_mapset(char *);
static int get_list_of_mapsets(void);

char *G__mapset_name (int n)
{
/*
 * first call will detect no mapsets in list
 * and go look up the list
 */
    if (nmapset == 0)
	get_list_of_mapsets();
/*
 * must not run off the bounds of the list
 */
    if (n < 0 || n >= nmapset)
	return ( (char *) NULL);

    return mapset_name[n];
}

static int get_list_of_mapsets()
{
    char name[30];
    FILE *fd;

/*
 * the list of mapsets is in SEARCH_PATH file in the mapset
 */
    mapset_name = NULL;
    if((fd = G_fopen_old ("","SEARCH_PATH",G_mapset())))
    {
	while (fscanf (fd, "%s", name) == 1)
	    if (G__mapset_permissions (name) >= 0)
		new_mapset(name);
	fclose (fd);
    }
/*
 * if no list, then set the list to the current mapset followed
 * by PERMANENT
 */
    if (!nmapset)
    {
	char *perm;
	char *cur;

	cur = G_mapset();
	perm = "PERMANENT";

	new_mapset (cur);
	if (strcmp(perm, cur) != 0 && G__mapset_permissions (perm) >= 0)
	    new_mapset (perm);
    }

    return 0;
}

static int new_mapset(char *name)
{
/*
 * extend mapset name list and store name
 * note: assumes G_realloc will become G_malloc if mapset_name == NULL
 */
    nmapset++;
    mapset_name = (char **) G_realloc ((char *) mapset_name, nmapset * sizeof (char *));
    mapset_name[nmapset-1] = G_store (name);

    return 0;
}


int G__create_alt_search_path()
{
    nmapset2 = nmapset;
    mapset_name2 = mapset_name;

    nmapset = 0;

    return 0;
    get_list_of_mapsets();
}

int G__switch_search_path()
{
    int n;
    char **names;

    n = nmapset2;
    names = mapset_name2;

    nmapset2 = nmapset;
    mapset_name2 = mapset_name;

    nmapset = n;
    mapset_name = names;

    return 0;
}

int G_reset_mapsets()
{
    nmapset=0;

    return 0;
}
