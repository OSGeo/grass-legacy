#ifndef lint
static char *SCCSID = "@(#)emess.c	USGS v.3.1";
#endif
# include <stdio.h>
# include <varargs.h>
/* general error message handler
** usage:
**	emess(code,args)
**		int code;
**	code >0 terminate program
**	|code| = 2 second argument is file name (char *)
**			third argument format (char *)
**		else second argument format (char *)
**	remaining argument are format % words
**
** If the following externals are set then file/line annotation is also given
*/
	char
*File_name = (char *)0, /* input file name */
*Prog_name = "";	/* name of program */
	int /* approximate line read where error occured */
File_line = 0;
	void
emess(va_alist) va_dcl {
	va_list args;
	char *fmt;
	int code;

	va_start(args);
	code = va_arg(args, int);
	fmt = va_arg(args, char *);
	(void)fprintf(stderr,"<%s>: ",Prog_name);
	if (File_name && *File_name)
		fprintf(stderr,"while processing file: %s, line %d\n",
			File_name, File_line);
	if (code == 2 || code == -2) {
		perror(fmt);
		fmt = va_arg(args, char *);
	}
	(void)vfprintf(stderr,fmt,args);
	if (code > 0) {
		fputs("\nprogram abnomally terminated\n", stderr);
		exit(code);
	}
	va_end(args);
}
