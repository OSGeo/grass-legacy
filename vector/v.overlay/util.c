/* ****************************************************************************
 *
 *  MODULE: v.overlay 
 *
 *  AUTHOR(S): Radim Blazek
 *  
 ******************************************************************************/
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include "gis.h"
#include "dbmi.h"
#include "Vect.h"
#include "local.h"

/* Compare by cat */
static int cmp ( const void *pa, const void *pb )
{
    ATTR *p1 = (ATTR*) pa;
    ATTR *p2 = (ATTR*) pb;

    if ( p1->cat < p2->cat ) return -1;
    if ( p1->cat > p2->cat ) return 1;
    return 0;
}

ATTR *find_attr ( ATTRIBUTES *attributes, int cat )
{
    ATTR *attr, key;

    G_debug ( 3, "find_attr() cat = %d", cat);

    key.cat = cat;
    
    attr = bsearch ( &key, attributes->attr, attributes->n, sizeof(ATTR), cmp ); 
    
    return attr;
}
