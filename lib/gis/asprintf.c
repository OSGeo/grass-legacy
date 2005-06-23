#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <unistd.h>
#include <assert.h>
#include "gis.h"


/*
 * Eric G. Miller egm2@jps.net 
 * Thu, 2 May 2002 17:51:54 -0700 
 * 
 * 
 * I've got a sort of cheat for asprintf. We can't use vsnprintf for the
 * same reason we can't use snprintf ;-)  Comments welcome.
 */

/* Make sure the macro doesn't impact our function, if it is defined */
#undef G_asprintf


/* We cheat by printing to a tempfile via vfprintf and then reading it
 * back in.  Not the most efficient way, probably.
 */

/*!
 * \brief safe replacement for asprintf()
 *
 * Allocate a string large enough to hold the new output,
 * including the terminating NUL, and return a pointer to
 * the first parameter.
 * The pointer should be passed to G_free() to release the
 * allocated storage when it is no longer needed.
 * Returns number of bytes written.
 *
 * \param char **out
 * \param char *fmt
 * \return int
 */

int G_asprintf(char **out, const char *fmt, ...)
{
    va_list ap;
    int ret_status = EOF;
    FILE *fp = NULL;
    char *work = NULL;

    assert(out != NULL && fmt != NULL);

    va_start(ap, fmt);

    if ( fp = tmpfile() ) {
	int count;

	count = vfprintf(fp, fmt, ap);
	if (count >= 0) {
	    work = G_calloc(count + 1, sizeof(char));
	    if (work != NULL) {
		rewind(fp);
		ret_status = fread(work, sizeof(char), count, fp);
		if (ret_status != count) {
		    ret_status = EOF;
		    G_free(work);
		    work = NULL;
		}
	    }
	}
	fclose(fp);
    }
    va_end(ap);
    *out = work;

    return ret_status;
}
