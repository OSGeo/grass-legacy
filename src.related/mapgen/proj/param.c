static char *SCCSID = "@(#)param.c	AMG v.1.2";
/* namelist style parameter input processor */
# include "projects.h"
	double
atof();
/* The following two globals must be
** initialized by calling system */
extern char *pargv[]; /* list of argument string pointers */
extern int pargc;     /* number of user arguments */

	static char *	/* lookup argument in list */
lookfor(s) char *s; {	/* linear list lookup */
	register char *sa, *sb;
	register i;

	for (i = 0; i < pargc; i++) {
		for (sa = s, sb = pargv[i]; *sa && *sa == *sb ; ++sa, ++sb) ;
		if (!*sa && (!*sb || *sb == '='))
			return sb; /* entry found */
	}
	return 0; /* not found */
}
/* parameter input for projection functions
** s   ::= string with 'tag'+id
**	see switch list for 'tag' types supported.
** def ::= string of value to be employed if 'id'
**	not found in input list.
** Returns pointer to union of appropriate value types.
*/
	char *
param(s, def) char *s, *def; {
	int type;
	char *arg;
	static union {
		double	f;
		int	i;
		char	s; /* dummy for compiler */
	} value;

	type = *s++;
	value.i = (arg = lookfor(s)) != 0; /* look for argument id */
	if (type != 'b') {	/* not boolean or flag type */
		if ( ! (value.i && *arg++ == '=') )
			arg = def;
		switch (type) {
		case 'i':	/* integer input */
			value.i = atoi(arg);
			break;
		case 'd':	/* simple real input */
			value.f = atof(arg);
			break;
		case 'r':	/* degrees input */
			value.f = dmstor(arg, 0);
			break;
		case 's':	/* char string */
			return (arg);
		}
	}
	/* casting of call procedure will extract proper type */
	return ( &value.s );
}
