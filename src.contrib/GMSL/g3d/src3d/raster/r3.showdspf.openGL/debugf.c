#include <stdio.h>
#include <varargs.h>

#define DEBUG

#ifdef DEBUG

char *getenv ();
/*VARARGS*/
void
debugf (va_alist)
     /* Note that the function_name and format arguments cannot be
      *    separately declared because of the definition of varargs.  */
     va_dcl
{
      va_list args;
      char *fmt;
      char *p;
      static int first = 1;
      static FILE *debugfp;


    if (first)
    {
	first = 0;

	if (p = getenv ("DEBUG"))
	{
	    if (strlen (p))
	    {
		debugfp = fopen (p, "w");
		setbuf (debugfp, NULL);
	    }
	    else
		debugfp = stderr;
	}
	else
	    debugfp = NULL;
    }
	
    if (!debugfp)
        return;

      va_start(args);
      /* print out name of function causing error */

      fmt = va_arg(args, char *);

      /* print out remainder of message */
      (void)vfprintf(debugfp, fmt, args);
      va_end(args);
}
#else

debugf ()
{
}
#endif
