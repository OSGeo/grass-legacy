#include "digit.h"

#include <sys/types.h>
#include <time.h>


static FILE *debugfp;
static int debug_on;
init_debug (file)
     char *file;
{
    char *getenv ();

    debug_on = 0;
    if (!getenv ("DEBUG"))
	return ;
    if (strcmp ("wy50", getenv ("DEBUG")) == 0)
    {
	/* set 43 lines/page */
	fprintf (stderr, "%ce+", 27);
	/* split  24/19 */
	fprintf (stderr, "%cx18", 27);
	sleep (1);
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
	debugf ("ok\n");
	
	/*debugf ("USER: %s  File: %s\n", G_whoami(), file);   */

    }
}

close_debug()
{
    switch (debug_on) {
	case 1:
	    break;
	case 2:
	    fclose (debugfp);
	    break;
	default:
	    break;
    }
    debug_on = 0;
}
debugf (format, a, b, c, d, e, f, g, h, i, j, k, l)
    char *format;
    int a, b, c, d, e, f, g, h, i, j, k, l;
{
      
    char buf[1024], *p;

    if (!debug_on)
	return 0;

    switch (debug_on) {
	case 1:
	    /* { */
	    fprintf (stderr, "%c}", 27);

	    sprintf (buf, format, a, b, c, d, e, f, g, h, i, j, k, l);
	    for (p = buf ; *p ; p++)
	    {
		fputc (*p, stderr);
		if (*p == '\n')
		    fputc ('\r', stderr);
	    }

	    fprintf (stderr, "%c]", 27);
	    break;
	case 2:
	    fprintf (debugfp, format, a, b, c, d, e, f, g, h, i, j, k, l);
	    break;
	default:
	    break;
    }
}
