#include <stdio.h>
#include "local_proto.h"
#define MAXLINES 18
#define LEFT "where am i"
#ifdef ANOTHER_BUTTON
#	define MIDDLE "done"
#	define RIGHT "mark point"
#else
#	define MIDDLE "mark point"
#	define RIGHT "done"
#endif
#define TITLE (char *) 0

static char *Title  = TITLE;
static char *Left   = LEFT;
static char *Middle = MIDDLE;
static char *Right  = RIGHT;

static int nlines = 0;

int instructions (int n)
{
    if (n == 0) nlines = MAXLINES;
    if (nlines >= MAXLINES)
    {
	if(Title) fprintf (stdout,"%s\n", Title);
	fprintf (stdout," Buttons:\n") ;
	fprintf (stdout,"  Left:   %s\n", Left) ;
	fprintf (stdout,"  Middle: %s\n", Middle) ;
	fprintf (stdout,"  Right:  %s\n\n", Right) ;
	nlines = 0 ;
    }
    nlines += n;

    return 0;
}

int reset_instructions (void)
{
    Title  = TITLE;
    Left   = LEFT;
    Middle = MIDDLE;
    Right  = RIGHT;
    nlines = 0;

    return 0;
}

int left_button (char *s)
{
    Left = s;

    return 0;
}

int middle_button (char *s)
{
    Middle = s;

    return 0;
}

int right_button (char *s)
{
    Right = s;

    return 0;
}
