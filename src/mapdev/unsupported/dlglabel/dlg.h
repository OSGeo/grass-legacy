/*  @(#)dlg.h	1.1  5/4/87  */
#include <stdio.h>

#define SOLID				0
#define DOTTED				1

struct node
{
	double x ;
	double y ;
	int n_lines ;
	int n_atts ;
	int *lines ;
	int *atts ;
} ;

struct area
{
	double x ;
	double y ;
	int n_lines ;
	int n_atts ;
	int n_isles ;
	int *lines ;
	int *atts ;
	double N ;
	double S ;
	double E ;
	double W ;
} ;

struct line
{
	int start_node ;
	int end_node ;
	int left_area ;
	int right_area ;
	int n_coors ;
	int n_atts ;
	FILE *file ;
	long offset ;
	int *atts ;
	double N ;
	double S ;
	double E ;
	double W ;
} ;

FILE *dlg ;
FILE *dlg_tmp;
FILE *dlg_new;

int tot_nodes ;
int tot_areas ;
int tot_lines ;
int orig_nodes ;
int orig_areas ;
int orig_lines ;

int	universe_defined ;		/*  tells me if there is a universe box  */

struct line *line ;
struct area *area ;
struct node *node ;
double *coors ;
