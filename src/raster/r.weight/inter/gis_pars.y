%{
#include "stdio.h"
#include "display.h"
#include "D.h"
#include <stdlib.h>
#include "raster.h"
#include "include.h"
%}

%union
{
        long            long_val;
        int              int_val;
        char            *str_val;
}

%token  <int_val>   ERAS_TKN
%token  <int_val>    LST_TKN
%token  <int_val>   COLR_TKN
%token  <int_val>    PRT_TKN
%token  <int_val>    ASG_TKN
%token  <int_val>   ANAL_TKN
%token  <int_val>   UNCH_TKN
%token  <int_val>   CHOS_TKN
%token  <int_val>    REC_TKN
%token  <int_val>    SAV_TKN
%token  <int_val>    MAP_TKN
%token  <int_val>   CATS_TKN
%token  <int_val>   LINE_TKN
%token  <int_val>   EXEC_TKN
%token  <int_val>   QUIT_TKN
%token  <int_val>   HELP_TKN
%token  <int_val>  PAREN_TKN
%token  <int_val>    ADD_TKN
%token  <int_val>   MULT_TKN
%token  <str_val>    NAM_STR
%token  <long_val>  LONG_NUM

%start  weight_cmnd

%%
weight_cmnd:    list_expr
					{ return(LIST_EXPR) ; }
        |       print_expr
					{ return(PRNT_EXPR) ; }
        |       assign_expr
					{ return(ASSGN_EXPR) ; }
        |       choose_expr
					{ return(CHOOS_EXPR) ; }
        |       unchoose_expr
					{ return(UNCHOOS_EXPR) ; }
        |       color_expr
					{ return(COLR_EXPR) ; }
        |       save_expr
					{ return(SAVE_EXPR) ; }
        |       recover_expr
					{ return(RECOV_EXPR) ; }
        |       execute_expr
					{ return(EXECUT_EXPR) ; }
        |       quit_expr
					{ return(QUIT_EXPR) ; }
        |       misc_expr
					{ return(MISC_EXPR) ; }
        |       help_expr
					{ return(HELP_EXPR) ; }
        |       null_expr
					{ return(NULL_EXPR) ; }
        |       error
					{ return(ERR_EXPR) ; }
	;

list_expr:      LST_TKN MAP_TKN LINE_TKN
					{ list_maps() ; }
	|	LST_TKN CATS_TKN NAM_STR LINE_TKN
					{ list_cats($3) ; }
	|	LST_TKN NAM_STR CATS_TKN LINE_TKN
					{ list_cats($2) ; }
	|	LST_TKN ANAL_TKN LINE_TKN
					{ list_analysis(0) ; }
	;

color_expr:	COLR_TKN NAM_STR LINE_TKN
					{ select_colors ($2) ; }
	;

print_expr: 	PRT_TKN ANAL_TKN LINE_TKN
					{ list_analysis(1) ; }
	;

assign_expr:    ASG_TKN NAM_STR LONG_NUM LONG_NUM LINE_TKN
					{ assign_single($2, $3, $4) ; }
           |    ASG_TKN NAM_STR LINE_TKN
					{ ask_weights($2) ; }
           |    ASG_TKN NAM_STR LONG_NUM LONG_NUM LONG_NUM LINE_TKN
					{ assign_mult($2, $3, $4, $5) ; }
	;

unchoose_expr:    UNCH_TKN NAM_STR LINE_TKN
					{ unchoose_map($2) ; }
           |    UNCH_TKN NAM_STR NAM_STR LINE_TKN
				{
					unchoose_map($2) ;
					unchoose_map($3) ;
				}
           |    UNCH_TKN NAM_STR NAM_STR NAM_STR LINE_TKN
				{
					unchoose_map($2) ;
					unchoose_map($3) ;
					unchoose_map($4) ;
				}
           |    UNCH_TKN NAM_STR NAM_STR NAM_STR NAM_STR LINE_TKN
				{
					unchoose_map($2) ;
					unchoose_map($3) ;
					unchoose_map($4) ;
					unchoose_map($5) ;
				}
           |    UNCH_TKN NAM_STR NAM_STR NAM_STR NAM_STR NAM_STR LINE_TKN
				{
					unchoose_map($2) ;
					unchoose_map($3) ;
					unchoose_map($4) ;
					unchoose_map($5) ;
					unchoose_map($6) ;
				}
           |    UNCH_TKN NAM_STR NAM_STR NAM_STR NAM_STR NAM_STR NAM_STR LINE_TKN
				{
					unchoose_map($2) ;
					unchoose_map($3) ;
					unchoose_map($4) ;
					unchoose_map($5) ;
					unchoose_map($6) ;
					unchoose_map($7) ;
				}
	;

choose_expr:    CHOS_TKN NAM_STR LINE_TKN
					{ choose_map($2) ; }
           |    CHOS_TKN NAM_STR NAM_STR LINE_TKN
				{
					choose_map($2) ;
					choose_map($3) ;
				}
           |    CHOS_TKN NAM_STR NAM_STR NAM_STR LINE_TKN
				{
					choose_map($2) ;
					choose_map($3) ;
					choose_map($4) ;
				}
           |    CHOS_TKN NAM_STR NAM_STR NAM_STR NAM_STR LINE_TKN
				{
					choose_map($2) ;
					choose_map($3) ;
					choose_map($4) ;
					choose_map($5) ;
				}
           |    CHOS_TKN NAM_STR NAM_STR NAM_STR NAM_STR NAM_STR LINE_TKN
				{
					choose_map($2) ;
					choose_map($3) ;
					choose_map($4) ;
					choose_map($5) ;
					choose_map($6) ;
				}
           |    CHOS_TKN NAM_STR NAM_STR NAM_STR NAM_STR NAM_STR NAM_STR LINE_TKN
				{
					choose_map($2) ;
					choose_map($3) ;
					choose_map($4) ;
					choose_map($5) ;
					choose_map($6) ;
					choose_map($7) ;
				}
	;

save_expr:      SAV_TKN NAM_STR LINE_TKN
					{ save($2) ; }
			|	SAV_TKN LINE_TKN
					{ save(NUL_STR) ; }
	;

recover_expr:   REC_TKN NAM_STR LINE_TKN
					{ recover($2) ; }
			|   REC_TKN LINE_TKN
					{ recover(NUL_STR) ; }
	;

execute_expr:   EXEC_TKN LINE_TKN
					{ execute() ; }
	;

misc_expr:      ADD_TKN LINE_TKN
					{ set_to_add() ; }
			|   MULT_TKN LINE_TKN
					{ set_to_mult() ; }
			|   ERAS_TKN LINE_TKN
					{ if (at_console())
					    {
						R_color(D_translate_color("black")) ;
						D_erase_window() ;
						R_flush() ;
					    }
					}
			|   LST_TKN SAV_TKN ANAL_TKN LINE_TKN
					{ system("ls -l") ; }
			|   LST_TKN REC_TKN ANAL_TKN LINE_TKN
					{ system("ls -l") ; }
			|   LST_TKN SAV_TKN LINE_TKN
					{ system("ls -l") ; }
			|   LST_TKN REC_TKN LINE_TKN
					{ system("ls -l") ; }
	;

quit_expr:      QUIT_TKN LINE_TKN
			{;}
	;

help_expr:      HELP_TKN LINE_TKN
					{ G_gishelp("WEIGHT", "general") ; }
			|	HELP_TKN COLR_TKN LINE_TKN
					{ G_gishelp("WEIGHT", "COLR") ; }
			|	HELP_TKN LST_TKN LINE_TKN
					{ G_gishelp("WEIGHT", "LST") ; }
			|	HELP_TKN PRT_TKN LINE_TKN
					{ G_gishelp("WEIGHT", "PRT") ; }
			|	HELP_TKN ASG_TKN LINE_TKN
					{ G_gishelp("WEIGHT", "ASG") ; }
			|	HELP_TKN ANAL_TKN LINE_TKN
					{ G_gishelp("WEIGHT", "ANAL") ; }
			|	HELP_TKN UNCH_TKN LINE_TKN
					{ G_gishelp("WEIGHT", "UNCH") ; }
			|	HELP_TKN CHOS_TKN LINE_TKN
					{ G_gishelp("WEIGHT", "CHOS") ; }
			|	HELP_TKN REC_TKN LINE_TKN
					{ G_gishelp("WEIGHT", "REC") ; }
			|	HELP_TKN SAV_TKN LINE_TKN
					{ G_gishelp("WEIGHT", "SAV") ; }
			|	HELP_TKN MAP_TKN LINE_TKN
					{ G_gishelp("WEIGHT", "MAP") ; }
			|	HELP_TKN CATS_TKN LINE_TKN
					{ G_gishelp("WEIGHT", "CATS") ; }
			|	HELP_TKN EXEC_TKN LINE_TKN
					{ G_gishelp("WEIGHT", "EXEC") ; }
			|	HELP_TKN QUIT_TKN LINE_TKN
					{ G_gishelp("WEIGHT", "QUIT") ; }
			|	HELP_TKN HELP_TKN LINE_TKN
					{ G_gishelp("WEIGHT", "HELP") ; }
			|	HELP_TKN ADD_TKN LINE_TKN
					{ G_gishelp("WEIGHT", "ADD_MULT") ; }
			|	HELP_TKN MULT_TKN LINE_TKN
					{ G_gishelp("WEIGHT", "ADD_MULT") ; }
			|	HELP_TKN NAM_STR LINE_TKN
					{ G_gishelp("WEIGHT", $2) ; }
	;

null_expr:      LINE_TKN
			{;}
	;

%%
/* ========================================================================= */
int yyerror(char *message )
{
        printf ("What?  Type help for help (%s)\n", message);

	return 0;
}
/* ========================================================================= */
