#include "tree.h"

/* ========================================================================= 
 * rcr_alc_lbuf recursively allocates the line buffers for parse tree nodes
 */

rcr_alc_lbuf(node) struct Node *node ;
{
    if (node == NULL)
	return ;

    switch(node->oper)
    {
    case LEAF_OPR:
	    node->cbuf = G_allocate_cell_buf() ;
	    break ;
    case NAM_OPR:
	    node->cbuf = G_allocate_cell_buf() ;
	    rcr_alc_lbuf(node->left) ;
	    break ;
    case OV1_OPR:
    case OV2_OPR:
    case OV3_OPR:
    case OV4_OPR:
	    node->cbuf = G_allocate_cell_buf() ;
	    rcr_alc_lbuf(node->left) ;
	    break ;
    case COV_OPR:
	    node->cbuf = G_allocate_cell_buf() ;
	    rcr_alc_lbuf(node->left) ;
	    break ;
    case GRP_OPR:
	    node->cbuf = G_allocate_cell_buf() ;
	    rcr_alc_lbuf(node->left) ;
	    break ;
    default:
	    node->cbuf = G_allocate_cell_buf() ;
	    rcr_alc_lbuf(node->left) ;
	    rcr_alc_lbuf(node->rite) ;
	    break ;
    }
    return ;
}
