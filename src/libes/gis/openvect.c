/* ***DEPRECATED*** */

#include "gis.h"
/*************************************************************
 *
 *  G_fopen_vector_old (name, mapset)
 *      char *name            map file name
 *      char *mapset          mapset containing map "name"
 *
 *  opens the existing vector file 'name' in the 'mapset'
 *
 *************************************************************
 *
 *  G_fopen_vector_new (name)
 *      char *name            map file name
 *
 *  opens a new vector file 'name' in the current mapset
 *
 *************************************************************/

/*
**  4.0, must go through Vectlib to open vector files
*/
#ifdef OLDCODE
#include "G.h"
FILE *
G_fopen_vector_old (name, mapset)
    char *name;
    char *mapset;
{
    return G_fopen_old ("dig", name, mapset);
}

FILE *
G_fopen_vector_new (name)
    char *name;
{
    return G_fopen_new ("dig", name);
}
#endif
