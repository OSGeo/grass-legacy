#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <assert.h>
#include "gis.h"

/*
 * Eric G. Miller egm2@jps.net 
 * Thu, 2 May 2002 17:51:54 -0700 
 * 
 * 
 * I've got a sort of cheat for asprintf. We can't use vsnprintf for the
 * same reason we can't use snprintf ;-)  Comments welcome (I'm not too
 * sure tmpfile() is safe.  It's apparently an anonymous file on my
 * machine, as nothing shows up in /tmp or the current directory...)
 */

/* Make sure the macro doesn't impact our function, if it is defined */
#undef G_asprintf

/* We cheat by printing to a tempfile via vfprintf and then reading it
 *  * back in.  Not the most efficient way, probably and tmpfile() is
 *  * not safe?
 *  */
int
  G_asprintf (char **out, const char *fmt, ...)
{
       va_list ap;
       int ret_status = EOF;
       int count = 0;
       FILE *fp = NULL;
       char *work = NULL;
       
       assert (out != NULL && fmt != NULL);
       
       va_start (ap, fmt);
       if ((fp = tmpfile()))
     {
	        count = vfprintf (fp, fmt, ap);
	        if (count >= 0)
	  {
	                 work = calloc (count + 1, 1);
	                 if (work != NULL)
	       {
		                  rewind (fp);
		                  ret_status = fread (work, 1, count, fp);
		                  if (ret_status != count)
		    {
		                           ret_status = EOF;
		                           free (work);
		                           work = NULL;
		    }
	       }
	  }
	        fclose (fp);
     }
       va_end (ap);
       *out = work;
       return ret_status;
}
