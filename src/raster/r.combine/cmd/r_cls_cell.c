#include "tree.h"

/* =========================================================================
 * rcr_cls_cell recursively closes the map files for leaf, name, and overlay
 * nodes
 */
rcr_cls_cell(node) struct Node *node ;
{
    if (node == NULL)
	    return ;
    switch(node->oper)
    {
    case LEAF_OPR:
	    G_close_cell(node->cellfd) ;
	    break ;

    case NAM_OPR:
	    G_close_cell(node->cellfd) ;

	    /* unary operator has left child only */
	    rcr_cls_cell(node->left) ;
	    break ;

    case OV1_OPR:
    case OV2_OPR:
    case OV3_OPR:
    case OV4_OPR:
	    if ( *(node->name) )
		    G_close_cell(node->cellfd) ;

	    /* unary operator has left child only */
	    rcr_cls_cell(node->left) ;
	    break ;

    case COV_OPR:
	    G_close_cell(node->cellfd) ;

	    /* unary operator has left child only */
	    rcr_cls_cell(node->left) ;
	    break ;

    case GRP_OPR:
	    /* unary operator has left child only */
	    rcr_cls_cell(node->left) ;
	    break ;

    default:
	    rcr_cls_cell(node->left) ;
	    rcr_cls_cell(node->rite) ;
	    break ;
    }
    return ;
}
/* ========================================================================= */
