#include "tree.h"

/* ========================================================================= 
 * rcr_fre_lbuf recursively frees the allocated line buffers 
 */

rcr_fre_lbuf(node) struct Node *node ;
{
    if (node == NULL)
	    return ;
    switch(node->oper)
    {
    case LEAF_OPR:
	    free(node->cbuf) ;
	    break ;
    case NAM_OPR:
	    free(node->cbuf) ;
	    rcr_fre_lbuf(node->left) ;
	    break ;
    case OV1_OPR:
    case OV2_OPR:
    case OV3_OPR:
    case OV4_OPR:
	    free(node->cbuf) ;
	    rcr_fre_lbuf(node->left) ;
	    break ;
    case COV_OPR:
	    free(node->cbuf) ;
	    rcr_fre_lbuf(node->left) ;
	    break ;
    case GRP_OPR:
	    free(node->cbuf) ;
	    rcr_fre_lbuf(node->left) ;
	    break ;
    default:
	    free(node->cbuf) ;
	    rcr_fre_lbuf(node->left) ;
	    rcr_fre_lbuf(node->rite) ;
	    break ;
    }
    return ;
}
