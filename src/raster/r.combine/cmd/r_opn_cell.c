#include "tree.h"

/* =========================================================================
 * rcr_opn_cell recursively opens the map files for leaf and overlay nodes
 */

rcr_opn_cell(node) struct Node *node ;
{
    int status ;
    if (node == NULL)
	    return ;

    switch(node->oper)
    {
    case LEAF_OPR:
	    status = C_open_cell_old(node) ;
	    break ;

    case NAM_OPR:
	    node->mapset = G_mapset() ;

	    node->cellfd = G_open_cell_new(node->name) ;
	    if(node->cellfd < 0)
	    {
		printf("  Can't open raster file [%s in %s]\n", 
			node->name, node->mapset) ;
		status = 0 ;
		break ;
	    }

	    /* unary operator has left child only */
	    status = rcr_opn_cell(node->left) ;

	    break ;

    case COV_OPR:
	    if(*(node->name))
	    {
		status = C_open_cell_old(node) ;
		if (! status)
		    break ;
		    
		node->new_cat = G_number_of_cats (node->name, node->mapset) + 1 ;
	    }

	    /* unary operator has left child only */
	    status = rcr_opn_cell(node->left) ;

	    break ;

    case OV1_OPR:
    case OV2_OPR:
    case OV3_OPR:
    case OV4_OPR:

	    if(*(node->name))
	    {
		status = C_open_cell_old(node) ;
		if (! status)
		    break ;
	    }

	    /* unary operator has left child only */
	    status = rcr_opn_cell(node->left) ;

	    break ;

    case GRP_OPR:
	    /* unary operator has left child only */
	    status = rcr_opn_cell(node->left) ;
	    break ;

    default:
	    status = rcr_opn_cell(node->left) ;
	    status = rcr_opn_cell(node->rite) ;
	    break ;
    }
    /* everything went okay if it got this far */
    return(status) ;
}
/* ========================================================================= */

C_open_cell_old(node)
    struct Node *node ;
{
    node->mapset = G_find_cell(node->name, "") ;
    if(node->mapset == NULL)
    {
	printf("  Can't find raster file [%s]\n", node->name) ;
	return 0 ;
    }

    node->cellfd = G_open_cell_old(node->name, node->mapset) ;
    if(node->cellfd < 0)
    {
	printf("  Can't open raster file [%s in %s]\n", 
		node->name, node->mapset) ;
	return 0 ;
    }
    return 1 ;
}
