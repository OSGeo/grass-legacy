#include "tree.h"

/* ========================================================================= */
/* cover_number  determines what category number to use for the current      */
/* cover job.                                                                */

int 
cover_number (struct Node *node)
{
    int ncats ;

    if ((ncats = G_number_of_cats( node->name, node->mapset)) < 0)
	return(1) ;

    return(ncats + 1) ;
}
