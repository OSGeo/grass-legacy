#define TELLME(x)	fprintf(stderr,"V_call %d\n",x) ;
#
/***********************************************************************

$Id$

Modified by Jacques Bouchard and Markus Neteler 6/99 to make cursor
keys working. Exit now with ESC-CR.


NAME:       V_call()

FUNCTION:   Interactively allow the user to enter answers into all
            available fields (as previously defined).  
            Answer fields have been created with calls to V_ques()
            Information fields have been created using V_const()
            General text has been created with calls to V_line()

USAGE:      V_call()

PARAMETERS:

RETURNS:    1 user entered ESC to continue
        0 user entered ctrl-C to cancel

ALGORITHM:  
        |   Zero out screen answer locations
        |   Initial curses screens
        |   Display text, constants, and answer fields
        |       Write text      (results from V_line() calls)  to curses window
        |       Write constants (results from V_const() calls) to curses window
        |       Write answers   (results from V_ques() calls)  to curses window
        |   Take commands from the keyboard
        |       switch on commands:
        |           case CR: case NL: case UP: case ESC:
        |               switch on answer type
        |                   case string
        |                       remove trailing non-alphanumeric characters
        |                       copy answer to target denoted in V_ques() call
        |                       blank out screen line
        |                       copy target to curses window
        |                   case integer
        |                       run atoi on answer, putting results in target 
        |                           denoted in V_ques() call
        |                       blank out screen line
        |                       printf target to curses window
        |                   case long
        |                       run atol on answer, putting results in target 
        |                           denoted in V_ques() call
        |                       blank out screen line
        |                       printf target to curses window
        |                   case float, double
        |                       run sscanf on answer, putting results in target 
        |                           denoted in V_ques() call
        |                       blank out screen line
        |                       printf target to curses window
        |                   default:
        |                       do nothing
        |               if ESC+CR return from V_call()
        |               if UP  shift to previous question
        |               if CR or NL  shift to next question
        |           case BS:   Move cursor back one column in current question
        |           case FS:   Move cursor forward one column in current 
        |               question
        |           case RPLT: Replot the current screen image
        |           case DUMP: Dump (append) the current window to the user's 
        |               home dir.
        |           default:   If an alphanumeric, put that char on the screen 
        |               and in the current answer field
        |   call V_exit  (erase screen and exit curses)

CALLS:      
        V_init()        Vask routine to initialize curses
        V_exit()        Vask routine to exit curses
        V__dump_window()   V_ask
        V__remove_trail()   V_ask
        addch()         curses routine
        addstr()        curses routine
        move()          curses routine
        putc()          curses routine
        refresh()       curses routine
        getch()         curses routine
        wrefresh()      curses routine
        sprintf()       UNIX
        strcpy()        UNIX

***********************************************************************/
#include <stdlib.h>
#include <string.h>
#include "vask.h"

static int centered(char *);
static int fmt(char *,int,double);

/* define the V__ struct defined in vask.h */
struct V__ V__ ;

#define DUMP	001
#define BELL	007
#define BS		010
#define FS		014
#define NL		012
#define UP		013
#define CR		015
#define RPLT	022
#define ESC		033
#define CTRLC		003

#define TARGET	V__.usr_answ[at_answer].targetptr
#define ROW		V__.usr_answ[at_answer].row
#define COL		V__.usr_answ[at_answer].col
#define LENGTH	V__.usr_answ[at_answer].length
#define TYPE	V__.usr_answ[at_answer].var_type
#define ANSWER	scr_answ[at_answer].position
#define RELINE	move(ROW, COL) ;                       \
				for (incr2=0;incr2<LENGTH; incr2++)    \
					addch('_')  ;                      \
				move(ROW, COL) 

/* flag ctrl-c is to be allowed */

static int interrupts_ok = 0;			/* mod shapiro */

int V_call() 
{
    int incr ;
    int incr2 ;
    int num_answers ;
    int at_answer   ;
    int at_constant ;
    int ans_col     ;
    char newchar    ;
    char lastchar = 0;
    int c;
    struct { char position[80]; } scr_answ[MAX_ANSW] ;
    int y,x;		/* shapiro */
    char temp[100];


/* Zero out screen answer locations */
    for(incr=0; incr<MAX_ANSW; incr++)
	    for(incr2=0; incr2<80; incr2++)
		    scr_answ[incr].position[incr2] = 000 ;

/* Initialize the curses windows */
    V_init() ;

/* Display text              */
    for (incr=0; incr<MAX_LINE; incr++) 
    {
	move (incr, 0) ;
	addstr(V__.page.line[incr]) ;
    }

/* Display constants   */
    for (at_constant=0; at_constant < V__.NUM_CONST; at_constant++) 
    {
	move(V__.constant[at_constant].row, V__.constant[at_constant].col) ;
	switch (V__.constant[at_constant].var_type) 
	{
	case 's':
		addstr(V__.constant[at_constant].targetptr.c) ;
		break ;
	case 'i':
		sprintf(temp,"%d", *V__.constant[at_constant].targetptr.i) ;
		addstr (temp) ;
		break ;
	case 'l':
		sprintf(temp,"%ld", *V__.constant[at_constant].targetptr.l) ;
		addstr (temp) ;
		break ;
	case 'f':
		fmt (temp, V__.constant[at_constant].decimal_places,
		    (double)*V__.constant[at_constant].targetptr.f) ;
		addstr (temp) ;
		break ;
	case 'd':
		fmt (temp, V__.constant[at_constant].decimal_places,
		    (double)*V__.constant[at_constant].targetptr.d) ;
		addstr (temp) ;
		break ;
	default:
		break ;
	}
    }

/* Display answer locations  */
    for (at_answer=0; at_answer < V__.NUM_ANSW; at_answer++) 
    {
	/* clear ANSWER */
	for (incr=0; incr<80; incr++)
	    scr_answ[at_answer].position[incr] = 000 ;

	switch (TYPE) 
	{
	case 's':
		strcpy(ANSWER, TARGET.c) ;
		RELINE ;
		for (incr=0; incr<LENGTH; incr++) 
		{
		    if ( *(TARGET.c + incr) == '\000')
			while (incr++ < LENGTH) 
			    addch('_') ;
		    else addch( *(TARGET.c + incr) ) ;
		}
		break ;
	case 'i':
		sprintf(ANSWER,"%d",*TARGET.i) ;
		RELINE ;
		addstr (ANSWER);
		break ;
	case 'l':
		sprintf(ANSWER,"%ld",*TARGET.l) ;
		RELINE ;
		addstr (ANSWER);
		break ;
	case 'f':
		fmt (ANSWER, V__.usr_answ[at_answer].decimal_places, (double)*TARGET.f);
		RELINE ;
		addstr (ANSWER);
		break ;
	case 'd':
		fmt (ANSWER, V__.usr_answ[at_answer].decimal_places, (double)*TARGET.d);
		RELINE ;
		addstr (ANSWER);
		break ;
	default:
		break ;
	}
    }
    num_answers = at_answer ;
    if (interrupts_ok)
	    move(22,0) ;
    else
	    move(23,0) ;
    centered("AFTER COMPLETING ALL ANSWERS, HIT <ESC><ENTER> TO CONTINUE") ;
    if (interrupts_ok)
    {
	sprintf(temp,"(OR <Ctrl-C> TO %s)", V__.interrupt_msg);
	centered(temp);
    }

/* Begin taking commands/answers from terminal */
    at_answer = 0 ;
    ans_col  = 0 ;

    move(ROW, COL) ;
    refresh() ;

    while(1)
    {
	getyx (stdscr, y, x);

	c = getch() & 0177;
	if(c == EOF)
	{
	    move (y, x);
	    refresh();
	    continue;
	}
	if (c == CTRLC && interrupts_ok == 0)
	{
	    move (y, x);
	    refresh();
	    continue;
	}
	if (V__.NUM_ANSW <= 0)
	{
	    if (c == ESC || c == CTRLC)
	    {
		interrupts_ok = 0;
		V_exit() ;
fprintf(stderr,"Returning from V_call\n");
		return(c == ESC) ;
	    }
	    move (y, x);
	    refresh();
	    continue;
	}
	newchar = c;
	switch (newchar)  
	{
	case CR: case NL: case UP: case ESC: case CTRLC:
		V__remove_trail(LENGTH, ANSWER) ;
		switch (TYPE) 
		{
		case 's':
		    /*
			ans_col = (LENGTH) ; 
			while((ANSWER[--ans_col] < 041) || (ANSWER[ans_col] > 0175));
			ANSWER[++ans_col] = 000 ;
		    */
		    /*
			V__remove_trail(LENGTH, ANSWER) ;
		    */
			strcpy(TARGET.c, ANSWER) ;
			RELINE ;
			addstr(TARGET.c) ;
			break ;
		case 'i':
			*TARGET.i = atoi(ANSWER) ;
			RELINE ;
			sprintf(temp,"%d", *TARGET.i) ;
			addstr (temp) ;
			break ;
		case 'l':
			*TARGET.l = atol(ANSWER) ;
			RELINE ;
			sprintf(temp,"%ld", *TARGET.l) ;
			addstr (temp) ;
			break ;
		case 'f':
			sscanf (ANSWER,"%f",TARGET.f);
			RELINE ;
			fmt (ANSWER, V__.usr_answ[at_answer].decimal_places,
				(double)*TARGET.f);
			sscanf (ANSWER,"%f",TARGET.f);
			addstr (ANSWER) ;
			break ;
		case 'd':
			sscanf (ANSWER,"%lf",TARGET.d);
			RELINE ;
			fmt (ANSWER, V__.usr_answ[at_answer].decimal_places,
				(double)*TARGET.d);
			sscanf (ANSWER,"%lf",TARGET.d);
			addstr (ANSWER) ;
			break ;
		default:
			break ;
		}
		if ((newchar == CR && lastchar == ESC)|| newchar == CTRLC) 
		{
		    interrupts_ok = 0;
		    V_exit() ;
		    return(c == CR) ;
		}
                if (newchar == ESC) 
		{
                    lastchar = ESC;
                    break;
                }
		if (newchar == UP)
		    at_answer = (at_answer+num_answers-1) % num_answers ;
		else
		    at_answer = (at_answer+1) % num_answers ;
		ans_col  = 0 ;
		move(ROW, COL) ;
		refresh() ;
		break ;

	case BS:
		ans_col = (ans_col-1 >= 0) ? ans_col-1 : 0 ;
		move(ROW, COL + ans_col) ;
		refresh() ;
		break ;

	case FS:
		if (ANSWER[ans_col+1] != '\0')
		{
		    ans_col = (ans_col+1 <= LENGTH) ? ans_col+1 : LENGTH ;
		    move(ROW, COL + ans_col) ;
		    refresh() ;
		}
		else
		    putc(BELL, stderr) ;
		break ;

	case RPLT:
		getyx (stdscr, y, x);		/* shapiro */
		wrefresh(curscr) ;
		move (y, x);		/* shapiro */
		refresh();		/* shapiro */
		break ;

	case DUMP:
		V__dump_window() ;
		break ;

	default:
                if (newchar == 91 && lastchar == ESC) {
                    lastchar = -91;
                    break;
                }
                if (lastchar == -91) {
                    lastchar = 0;
                    if (newchar == 49) {        /* HOME */
		        ans_col = 0 ;
                    } else if (newchar == 52) { /* END */
                        for (c = 0; c < LENGTH && ANSWER[c]; c++) {}
		        ans_col = --c ;
                    } else if (newchar == 65) { /* UP */
		        at_answer = (at_answer+num_answers-1) % num_answers ;
		        ans_col  = 0 ;
                    } else if (newchar == 66) { /* DOWN */
		        at_answer = (at_answer+1) % num_answers ;
		        ans_col  = 0 ;
                    } else if (newchar == 67) { /* LEFT */
		        if (ans_col+1 < LENGTH && ANSWER[ans_col+1]) ++ans_col;
                    } else if (newchar == 68) { /* RIGHT */
		        ans_col = (ans_col-1 >= 0) ? ans_col-1 : 0 ;
                    } else if (newchar == 50) { /* DELETE */
                        break;
                    }
		    move(ROW, COL + ans_col) ;
		    refresh() ;
                    break;
                }
		if (ans_col >= LENGTH) 
		    break ;
		if ((newchar >= '\040') && (newchar < '\176')) 
		{
		    addch(newchar) ;
		    ANSWER[ans_col] = newchar ;
		    ans_col++ ;
		    refresh() ;
		}
		break ;
	} 
    } 
}

int V_intrpt_ok()
{
    interrupts_ok = 1;		/* will be set false when V_call() exists */

	return 0;
}
int V_intrpt_msg ( char *msg)
{
    strcpy (V__.interrupt_msg, msg);

	return 0;
}


static int fmt(char *s,int n, double x)
{
    char buf[20];

    if (n >= 0)
	sprintf (buf, "%%.%dlf", n);
    else
	strcpy (buf, "%.5lf");
/* I had to use .5lf instead of just lf since a number like 65.8 got sprintf'ed as 65.800003
 * this is a hack - I admit it.
 */
    sprintf (s, buf, x);
    if (n < 0)
	V__trim_decimal(s);

	return 0;
}

static int centered( char *msg)
{
    int indent;

    indent = (80 - strlen(msg))/2;
    while (indent-- > 0)
	addstr(" ");
    addstr(msg);
    addstr("\n");

	return 0;
}
