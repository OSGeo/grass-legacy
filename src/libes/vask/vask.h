#include	<signal.h>
#include	<curses.h>
#define 	MAX_ANSW	80
#define 	MAX_CONST	80
#define 	MAX_LINE	23

/**
*** target contains all the variable type that we will want to
*** be looking for.
**/
union target {
	char *c ;
	short *h ;
	int *i ;
	long *l ;
	float *f ;
	double *d ;
	double for_alignement;
};

/**
*** ans_rec contains all the information needed to go
*** and get an answer from the screen.
**/
struct ans_rec {
	union target targetptr ; 	/* pointer to value	*/
	int   var_type         ;	/* value type		*/
	int   row              ;	/* row position		*/
	int   col              ;	/* column position	*/
	int   length           ;	/* length of entry	*/
	int   decimal_places   ;	/* number of decimal places */
};

/**
*** page is an array of pointers to lines on a page.
**/
struct page {
	char *line[MAX_LINE] ;
};

struct V__
{
	struct	ans_rec	usr_answ[MAX_ANSW];
	struct	ans_rec	constant[MAX_CONST] ;

	struct	page	page;

	int	NUM_ANSW;
	int	NUM_CONST;
	int	NUM_LINE ;

	int	decimal_places;
	char    interrupt_msg[80];
} V__;

/* one of the library routines (V_call) will define this struct */
extern struct V__ V__ ;
