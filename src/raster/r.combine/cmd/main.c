#include "tree.h"
struct Node *yytree ;
struct Node *e_expr_list[512] ;

main(argc, argv) char **argv ;
{
    struct Node *tree ;
    int i, outcome ;
    int j ;

    G_gisinit(argv[0]) ;

    init_comb(argc,argv) ;

    for(i = 0 ; i < 512 ; i++)
    {
	fprintf(stderr, "[%d]: ", i+1) ;

	/* call the parser
	 * yyparse returns MAP_EXPR or HEAD_EXPR or EXIT_EXPR if ok
	 * yyparse returns ERR_EXPR if there was an error
	 */
	outcome = yyparse() ;

	if(outcome == MAP_EXPR)
	{
	    int early_break ;
printf ("MAP EXPRESSION\n");

	    tree = yytree ;

	    e_expr_list[i] = tree ;

    /* calculate decendants in the tree	   */
	    rcr_sum_tree(tree) ;

	    /* recr_pr_tree(tree) ; */

    /* open all the leaf cell files in tree     */
	    if(rcr_opn_cell(tree) == 0)
		    goto error_opn_cell     ;
    
    /* make space for all the buffers in the parse tree */
    /* all buffers are as wide as the current user window */
	    rcr_alc_lbuf(tree) ;

    /* evaluate the expression		  */
	    early_break = eval_tree(tree) ;

    /* Free the allocated line buffers */
	    rcr_fre_lbuf(tree) ;

    /* close all the leaf data files in tree     */
	    rcr_cls_cell(tree)      ;

	    if (early_break)
		    goto message ;

    /* make all the needed support files	 */
	    rcr_wr_supp(tree)       ;

	    goto message ;

error_opn_cell:
	    printf("map raster file open problem\n") ;
	    printf("  possibly a mis-use of history\n") ;

message:
	    printf("evaluated a map expression\n") ;
	}
	else if(outcome == HEAD_EXPR)
	{
		printf("evaluated a head expression\n") ;
	}
	else if(outcome == WIN_EXPR)
	{
		printf("evaluated a window expression\n") ;
	}
	else if(outcome == STAT_EXPR)
	{
		printf("evaluated a statistics expression\n") ;
	}
	else if(outcome == CATS_EXPR)
	{
		printf("evaluated a categories expression\n") ;
	}
	else if(outcome == HIST_EXPR)
	{
		printf("evaluated a history expression\n") ;
		start_line_count() ;
		for (j=0; j<i; j++)
		{
			printf("Expression %d:\n", j+1) ;
			check_lines() ;
			if (e_expr_list[j] != NULL)
				rcr_pr_hist(e_expr_list[j], 1) ;
			else
			{
				printf("     non-analysis expression\n") ;
				check_lines() ;
			}
		}
	}
	else if(outcome == ERAS_EXPR)
	{
		printf("evaluated an erase expression\n") ;
		if(at_console())
		{
			R_color(D_translate_color("black")) ;
			D_erase_window() ;
			R_flush();
		}
	}
	else if(outcome == HELP_EXPR)
	{
		printf("evaluated a help expression\n") ;
	}
	else if(outcome == EXIT_EXPR)
	{
		printf("bye\n") ;
		if (at_console())
			R_close_driver() ;
		exit(0) ;
		break ;
	}
	else if(outcome == ERR_EXPR)
	{
		clear_input() ;
		printf("Don't understand.  Type (help) for help\n") ; ;
	}
	else printf("got some other return code: %d\n", outcome) ;;
    }
}
/* ========================================================================= */
