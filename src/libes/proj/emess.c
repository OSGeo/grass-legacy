/* Error message processing */
#ifndef lint
static char RCSID[] = "@(#)$Id: emess.c,v 4.1 1992/07/02 12:54:05 gie Exp $";
#endif

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>
#define _emess_routine
#include "projects.h"
/*
#include "emess.h"
*/

#ifdef __STDC__
#include <stdarg.h>
#else
#include <varargs.h>
#endif

#ifdef __STDC__

void
emess(int code, char *fmt, ...) 
{
	va_list args;

	va_start(args, fmt);
	/* prefix program name, if given */
	if ((fmt != NULL) &&(emess_dat.Prog_name !=NULL))
		(void)fprintf(stderr,"<%s>: ",emess_dat.Prog_name);
	/* print file name and line, if given */
	if (emess_dat.File_name != NULL && *emess_dat.File_name)
		(void)fprintf(stderr,"while processing file: %s, line %d\n",
			emess_dat.File_name, emess_dat.File_line);
	else
		putc('\n', stderr);
	/* if |code|==2, print errno code data */
	if (code == 2 || code == -2)
		(void)fprintf(stderr, "Sys errno: %d: %s\n",
			errno, strerror(errno));
	/* post remainder of call data */
	(void)vfprintf(stderr,fmt,args);
	va_end(args);
	/* die if code positive */
	if (code > 0) {
		(void)fputs("\nprogram abnomally terminated\n", stderr);
		exit(code);
	}
	else
		putc('\n', stderr);
}

#else

void
emess(va_alist) 
    va_dcl
{
	int code;
	char *fmt;
	va_list ap;
	char *args[100];
	int argno = 0;

	va_start(ap);
	code = va_arg (ap, int);
	fmt = va_arg (ap, char *);

	/* prefix program name, if given */
	if ((fmt != NULL) &&(emess_dat.Prog_name !=NULL))
		(void)fprintf(stderr,"<%s>: ",emess_dat.Prog_name);
	/* print file name and line, if given */
	if (emess_dat.File_name != NULL && *emess_dat.File_name)
		(void)fprintf(stderr,"while processing file: %s, line %d\n",
			emess_dat.File_name, emess_dat.File_line);
	else
		putc('\n', stderr);
	/* if |code|==2, print errno code data */
	if (code == 2 || code == -2)
		(void)fprintf(stderr, "Sys errno: %d: %s\n",
			errno, strerror(errno));

	while ((args[argno++] = va_arg(ap, char *)) != (char *)0)
	    ;

	/* post remainder of call data */
	(void)vfprintf(stderr,fmt,args);
	va_end(ap);
	/* die if code positive */
	if (code > 0) {
		(void)fputs("\nprogram abnomally terminated\n", stderr);
		exit(code);
	}
	else
		putc('\n', stderr);
}

#endif
