%{
#include <stdlib.h>
#include <string.h>
#include "tree.h"
#include "lvw.h"
#include "local_proto.h"

struct Cell_head *get_cur_win() ;
extern struct Node *yytree ;
extern struct Node *e_expr_list[512] ;
extern FILE *yyin ;

struct Group e_yygroup[256]   ;/* limit of 256 nested group expressions */
int   e_yygrp_i=0      ;/* index into current array              */
%}

%union
{
	int              int_val;
	struct Node     *nod_val;
	char            *str_val;
}

%token  <int_val>       AND_TKN
%token  <int_val>        OR_TKN
%token  <int_val>       NOT_TKN
%token  <int_val>       GRP_TKN
%token  <int_val>       CATS_TKN
%token  <int_val>       EXPR_TKN
%token  <int_val>       RANGE_TKN
%token  <int_val>       NAM_TKN
%token  <int_val>       OVR_TKN
%token  <int_val>       COV_TKN
%token  <int_val>       WIN_TKN
%token  <int_val>       BYE_TKN
%token  <int_val>       ERA_TKN
%token  <int_val>       HST_TKN
%token  <int_val>       HLP_TKN
%token  <str_val>       NAM_STR
%token  <int_val>       INUMBER
%token  <int_val>       LP RP SEMI

%type   <nod_val>       map_expr

%start  map_cmnd

%%
map_cmnd:       map_expr
		{
			yytree = $1 ;
			return(MAP_EXPR) ;
		}
	|       win_expr
		{
			return(WIN_EXPR) ;
		}
	|       cats_expr
		{
			return(CATS_EXPR) ;
		}
	|       help_expr
		{
			return(HELP_EXPR) ;
		}
	|       hist_expr
		{
			return(HIST_EXPR) ;
		}
	|       eras_expr
		{
			return(ERAS_EXPR) ;
		}
	|       exit_expr
		{
			return(EXIT_EXPR) ;
		}
	|       error
		{
			return(ERR_EXPR) ;
		}
	;
map_expr:	LP NAM_TKN NAM_STR map_expr RP
		{
			$$ = make_node(NAM_OPR,     $4, NUL_EXP,      $3) ;
		}
	|       LP OVR_TKN NAM_STR INUMBER map_expr RP
		{
			$$ = make_node(OVR_OPR+$4,     $5, NUL_EXP,      $3) ;
		}
	|       LP OVR_TKN INUMBER NAM_STR map_expr RP
		{
			$$ = make_node(OVR_OPR+$3,     $5, NUL_EXP,      $4) ;
		}
	|       LP OVR_TKN INUMBER map_expr RP
		{
			$$ = make_node(OVR_OPR+$3,     $4, NUL_EXP,    NULL) ;
		}
	|       LP COV_TKN NAM_STR map_expr RP
		{
			$$ = make_node(COV_OPR,     $4, NUL_EXP,      $3) ;
		}
	|       LP AND_TKN  map_expr map_expr RP 
		{
			$$ = make_node(AND_OPR,      $3,      $4,   "and") ;
		}
	|       LP OR_TKN  map_expr map_expr  RP
		{
			$$ = make_node( OR_OPR,      $3,      $4,    "or") ;
		}
	|       LP NOT_TKN  map_expr RP
		{
			$$ = make_node(GRP_OPR,      $3, NUL_EXP, "group") ;
			init_group (&$$->group);
			mark_group (&$$->group, 0, 0);
		}
	|       LP GRP_TKN
		{
			/* GRAB CONTROL EARLY */
			/* multiple group arrays necessary for nested expr */
			init_group(&e_yygroup[e_yygrp_i]);

			/* set index to next array for nested expression   */
			e_yygrp_i++ ;
		}
			grp_expr map_expr  RP
		{
			$$ = make_node(GRP_OPR,      $5, NUL_EXP, "group") ;

			/* set index back for this array */
			e_yygrp_i-- ;

			/* set the 'which group' table,min,max in the node itself */
			$$->group.max   = e_yygroup[e_yygrp_i].max;
			$$->group.min   = e_yygroup[e_yygrp_i].min;
			$$->group.table = e_yygroup[e_yygrp_i].table;
		}
	|       LP EXPR_TKN INUMBER RP
		{
			$$ = e_expr_list[$3 - 1] ;
		}
	|       LP NAM_STR RP
		{
			$$ = make_node(LEAF_OPR, NUL_EXP, NUL_EXP,      $2) ;
		}
	|       NAM_STR
		{
			$$ = make_node(LEAF_OPR, NUL_EXP, NUL_EXP,      $1) ;
		}
	;

win_expr:   WIN_TKN NAM_STR
		{
			if(get_win($2) == 0)
				yyerror("window error (map name doesn't exist)");
		}
	|       WIN_TKN
		{
			write_window( get_cur_win()) ;
		}
	;

grp_expr:       /* empty */
		{
		}
	|       grp_expr INUMBER
		{
			mark_group (&e_yygroup[e_yygrp_i-1], $2, $2);
		}
	|       grp_expr INUMBER RANGE_TKN INUMBER
		{
			mark_group (&e_yygroup[e_yygrp_i-1], $2, $4);
		}
	;

cats_expr:      CATS_TKN NAM_STR
		{
			if(get_cats($2) == 0)
				yyerror("categories error");
		}
	;

hist_expr:      HST_TKN
		{
		}
	;

eras_expr:      ERA_TKN
		{
		}
	;

help_expr:     LP HLP_TKN RP
		{
			G_gishelp("COMBINE",NULL) ;
		}
		|		HLP_TKN
		{
			G_gishelp("COMBINE",NULL) ;
		} /* next removed because twice included 
		     (Andreas Lange, 10/2000): 
		|		HLP_TKN HLP_TKN
		{
			G_gishelp("COMBINE",NULL) ;
		} */
		|		HLP_TKN NAM_STR
		{
			G_gishelp("COMBINE",$2) ;
		}
		|		HLP_TKN AND_TKN
		{
			G_gishelp("COMBINE","AND") ;
		}
		|		HLP_TKN OR_TKN
		{
			G_gishelp("COMBINE","OR") ;
		}
		|		HLP_TKN NOT_TKN
		{
			G_gishelp("COMBINE","NOT") ;
		}
		|		HLP_TKN GRP_TKN
		{
			G_gishelp("COMBINE","GRP") ;
		}
		|		HLP_TKN CATS_TKN 
		{
			G_gishelp("COMBINE","CATS") ;
		}
		|		HLP_TKN EXPR_TKN 
		{
			G_gishelp("COMBINE","EXPR") ;
		}
		|		HLP_TKN RANGE_TKN 
		{
			G_gishelp("COMBINE","RANGE") ;
		}
		|		HLP_TKN NAM_TKN 
		{
			G_gishelp("COMBINE","NAM") ;
		}
		|		HLP_TKN OVR_TKN 
		{
			G_gishelp("COMBINE","OVR") ;
		}
		|		HLP_TKN COV_TKN 
		{
			G_gishelp("COMBINE","COV") ;
		}
		|		HLP_TKN WIN_TKN 
		{
			G_gishelp("COMBINE","WIN") ;
		}
		|		HLP_TKN BYE_TKN 
		{
			G_gishelp("COMBINE","BYE") ;
		}
		|		HLP_TKN ERA_TKN 
		{
			G_gishelp("COMBINE","ERA") ;
		}
		|		HLP_TKN HST_TKN 
		{
			G_gishelp("COMBINE","HST") ;
		}
		|		HLP_TKN HLP_TKN 
		{
			G_gishelp("COMBINE","HLP") ;
		}
	;

exit_expr:      BYE_TKN
		{
		}
	;
%%
/* ========================================================================= */
int yyerror(message) char *message ;
{
/* Andreas Lange: i uncommeted the next section for debugging 10/2000 
   and commented it again ...
	fprintf (stderr, "parser: %s, try again:\n", message); */
	
	return 0;
}
/* ========================================================================= */

struct Node *
make_node (oper, left, rite, name)
int oper ;
struct Node *left, *rite ;
char *name ;
{
	struct Node *node ;

	node = (struct Node *)falloc (1, sizeof (struct Node),
		"falloc: make_node") ;

	node->oper = oper ;
	node->left = left ;
	node->rite = rite ;
	if (name)
		strcpy(node->name,name) ;
	else
		*node->name = '\0' ;

	/* fprintf(stderr, "making node for %s\n", name) ; */

	return (node) ;
}
/* ========================================================================= */
