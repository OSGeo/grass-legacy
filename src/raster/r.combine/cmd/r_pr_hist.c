#include <stdio.h>
#include "tree.h"
#define OPEN    i=depth; while(i--) fprintf (stdout,"    "); printf("(") 
#define CLOSE   i=depth; while(i--) fprintf (stdout,"    "); printf(")\n"); check_lines()

static int lines_out ;
int 
start_line_count (void)
{
    lines_out = 0 ;

    return 0;
}

int 
check_lines (void)
{
    char buff[64] ;
    if (lines_out++ < 19)
        return(0) ;
    fprintf (stdout,"\nHit RETURN for more   ") ;
    fgets(buff,64,stdin) ;
    fprintf (stdout,"\n") ;
    lines_out = 1 ;

    return 0;
}


int 
rcr_pr_hist (struct Node *node, int depth)
{
    int i ;
    int first ;
    int min, max;

    if (node == NULL)
        return 0;
    OPEN ;
    switch(node->oper)
    {
    case LEAF_OPR:
        fprintf (stdout,"%s)\n", node->name) ;
        check_lines() ;
        break ;

    case NAM_OPR:
        fprintf (stdout,"NAME %s\n", node->name) ;
        check_lines() ;
        rcr_pr_hist(node->left, depth+1) ;
        break ;

    case COV_OPR:
        fprintf (stdout,"COVER %s\n", node->name) ;
        check_lines() ;
        rcr_pr_hist(node->left, depth+1) ;
        break ;

    case OV1_OPR:
        fprintf (stdout,"OVER %s red\n", node->name) ;
        check_lines() ;
        rcr_pr_hist(node->left, depth+1) ;
        break ;

    case OV2_OPR:
        fprintf (stdout,"OVER %s yellow\n", node->name) ;
        check_lines() ;
        rcr_pr_hist(node->left, depth+1) ;
        break ;

    case OV3_OPR:
        fprintf (stdout,"OVER %s blue\n", node->name) ;
        check_lines() ;
        rcr_pr_hist(node->left, depth+1) ;
        break ;

    case OV4_OPR:
        fprintf (stdout,"OVER %s gray\n", node->name) ;
        check_lines() ;
        rcr_pr_hist(node->left, depth+1) ;
        break ;

    case GRP_OPR:
        check_lines() ;
        fprintf (stdout,"GROUP") ;
        min = node->group.min;
        max = node->group.max;
        first = min-1 ;
        for(i=min;i<=max;i++)
        {
        if(node->group.table[i-min])
        {
            if (first < min)
            first = i ;
        }
        else
        {
            if(first >= min)
            {
            if (first == i-1)
                fprintf (stdout," %d", first) ;
            else
                fprintf (stdout," %d-%d", first, i-1) ;
            }
            first = min-1 ;
        }
        }
        if(first >= min)
        {
        if (first == i-1)
            fprintf (stdout," %d", first) ;
        else
            fprintf (stdout," %d-%d", first, i-1) ;
        }
        fprintf (stdout,"\n") ;
        check_lines() ;
        rcr_pr_hist(node->left, depth+1) ;
        break ;

    case AND_OPR:
        fprintf (stdout,"AND\n") ;
        check_lines() ;
        rcr_pr_hist(node->left, depth+1) ;
        rcr_pr_hist(node->rite, depth+1) ;
        break ;

    case  OR_OPR:
        fprintf (stdout,"OR\n") ;
        check_lines() ;
        rcr_pr_hist(node->left, depth+1) ;
        rcr_pr_hist(node->rite, depth+1) ;
        break ;
    
    default:
        break ;
    }
    if(node->oper != LEAF_OPR)
    {
    CLOSE ;
    }
    /* everything went okay if it got this far */

    return 0;
}

/* ========================================================================= */
