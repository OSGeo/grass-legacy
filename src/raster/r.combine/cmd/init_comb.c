/* init_comb handles all start up tasks associated with combine */

#include "tree.h"

static int use_graphics = 1 ;

init_comb(argc,argv)
    char *argv[];
{
    struct Flag *s;

    s = G_define_flag();
    s->key = 's';
    s->description = "Use symbols (instead of graphics)";
    s->answer = 0;

    if (G_parser(argc,argv)) exit(1);

    use_graphics = !s->answer;

    if(at_console())
    {
	R_open_driver() ;

	D_setup(0);

	D_set_cell_name("combine result") ;
    }

    /* initialize the mapset window */
    if(init_proj_win() == 0)
	yyerror("Can't initialize given mapset") ;

    if(at_console())
    {
	printf("\nAnalyses will be displayed in color during execution\n");
    }
    else
    {
	printf("\nAnalyses will be displayed with symbols during execution\n");
    }

    /* Letem know help is available */
    printf("\nHelp is available for %s: enter (help)\n", G_program_name()) ;
}

at_console()
{
    return(use_graphics) ;
}
