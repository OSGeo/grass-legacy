#include <stdio.h>
#include <unistd.h>
#include <time.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <curses.h>
#include "gis.h"
#include "keyboard.h"
#include "local_proto.h"

static FILE *debugfp;
static int debug_on;

int debugf (char *format,...)
/* int a, int b, int c, int d, int e, int f, int g, int h, int i, int j, int k, int l)*/
{
     va_list	vptr; 
    char buf[1024], *p;

    if (!debug_on)
	return 0;

    va_start(vptr,format);
    switch (debug_on) {
	case 1:
	    /* { */
	    fprintf (stderr, "%c}", 27);

	    vsprintf (buf, format, vptr);
	    for (p = buf ; *p ; p++)
	    {
		fputc (*p, stderr);
		if (*p == '\n')
		    fputc ('\r', stderr);
	    }

	    fprintf (stderr, "%c]", 27);
	    break;
	case 2:
	    vfprintf (debugfp, format, vptr);
	    break;
	default:
	    break;
    }
    return 0;
}

int init_debug (char *file)
{
    debug_on = 0;
    if (!getenv ("DEBUG"))
	return 1;
    if (strcmp ("wy50", getenv ("DEBUG")) == 0)
    {
	/* set 43 lines/page */
	fprintf (stderr, "%ce+", 27);
	/* split  24/19 */
	fprintf (stderr, "%cx18", 27);
	sleep (1);
	flush_keyboard ();
	debug_on = 1;
    }
    else
    {
	if ((debugfp = fopen (getenv ("DEBUG"), "a")) == NULL)
	{
	    if ((debugfp = fopen (getenv ("DEBUG"), "w")) == NULL)
	    {
		fprintf (stderr, "NO DEBUG\n");
		debug_on = 0;
	    }
	    else
		debug_on = 2;
	}
	else
	    debug_on = 2;
    }
    if (debug_on)
    {
	long timer;

	setbuf (debugfp, NULL);
	time (&timer);
	debugf ("\n\nSTARTUP: %s", asctime (localtime(&timer)));
	debugf ("USER: %s  File: %s\n", G_whoami(), file);
    }
    return 0;
}

int 
close_debug (void)
{
    switch (debug_on) {
	case 1:
#ifdef FOO
	    /* set 24 rows */
	    fprintf (stderr, "%ce(", /*)*/  27);
	    /* set to full screen */
	    fprintf (stderr, "%cx0", 27);
	    flush_keyboard ();
#endif
	    break;
	case 2:
	    fclose (debugfp);
	    break;
	default:
	    break;
    }
    debug_on = 0;
    return 0;
}
