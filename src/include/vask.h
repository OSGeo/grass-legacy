#include	<signal.h>
#include	<curses.h>

#ifdef MAKE_VASK_LIB
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

#endif

int V_float_accuracy (int);
int V_call(void);
int V_intrpt_ok(void);
int V_intrpt_msg(const char *);
int V_clear(void);
int V_const(void *,char,int,int,int);
int V_error(char *);
int V_exit(void);
int V_init(void);
int V_line(register int,register char *);
int V_ques(void *,char,int,int,int);
int V__dump_window(void);
int V__remove_trail(int,char *);
int V__trim_decimal(char *);
