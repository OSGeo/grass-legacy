#include "tree.h"

/* ========================================================================= */
/* rcr_pr_tree recursively prints the parse tree of the map expression      */
int e_tabs = -1 ;
rcr_pr_tree(node) struct Node *node ;
{
    e_tabs++ ;

    if (node == NULL)
        return ;

    switch(node->oper)
    {
    case LEAF_OPR:
        do_tabs(e_tabs) ;
        fprintf(stderr, "name: %s\n", node->name) ;
        do_tabs(e_tabs) ;
        fprintf(stderr, "address: %d\n", node) ;
        do_tabs(e_tabs) ;
        fprintf(stderr, "ltot: %d\n", node->ltot) ;
        do_tabs(e_tabs) ;
        fprintf(stderr, "rtot: %d\n", node->rtot) ;
        do_tabs(e_tabs) ;
        fprintf(stderr, "oper: %d\n", node->oper) ;
        break ;

    case NAM_OPR:
    case COV_OPR:
    case OV1_OPR:
    case OV2_OPR:
    case OV3_OPR:
    case OV4_OPR:
        /* unary operator has left child only */
        do_tabs(e_tabs) ;
        fprintf(stderr, "name: %s\n", node->name) ;
        do_tabs(e_tabs) ;
        fprintf(stderr, "address: %d\n", node) ;
        do_tabs(e_tabs) ;
        fprintf(stderr, "ltot: %d\n", node->ltot) ;
        do_tabs(e_tabs) ;
        fprintf(stderr, "rtot: %d\n", node->rtot) ;
        do_tabs(e_tabs) ;
        fprintf(stderr, "oper: %d\n", node->oper) ;
        rcr_pr_tree(node->left) ;
        break ;

    case GRP_OPR:
        do_tabs(e_tabs) ;
        fprintf(stderr, "name: %s\n", node->name) ;
        do_tabs(e_tabs) ;
        fprintf(stderr, "address: %d\n", node) ;
        do_tabs(e_tabs) ;
        fprintf(stderr, "ltot: %d\n", node->ltot) ;
        do_tabs(e_tabs) ;
        fprintf(stderr, "rtot: %d\n", node->rtot) ;
        do_tabs(e_tabs) ;
        fprintf(stderr, "oper: %d\n", node->oper) ;
        rcr_pr_tree(node->left) ;
        break ;

    default:
        do_tabs(e_tabs) ;
        fprintf(stderr, "name: %s\n", node->name) ;
        do_tabs(e_tabs) ;
        fprintf(stderr, "address: %d\n", node) ;
        do_tabs(e_tabs) ;
        fprintf(stderr, "ltot: %d\n", node->ltot) ;
        do_tabs(e_tabs) ;
        fprintf(stderr, "rtot: %d\n", node->rtot) ;
        do_tabs(e_tabs) ;
        fprintf(stderr, "oper: %d\n", node->oper) ;
        rcr_pr_tree(node->left) ;
        rcr_pr_tree(node->rite) ;
        break;
    }
    e_tabs-- ;
    return ;
}
/* ========================================================================= */
do_tabs(j)
{
    int i ;
    for(i = 0 ; i < j ; i++) 
    {
    fprintf(stderr, "\t") ;
    }
}
