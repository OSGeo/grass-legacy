#ifndef lint
static char *SCCSID = "@(#)load_opt.c	USGS v.3.2";
#endif
/* Procedures to save list of +option[=arg] entries from either
** the run line or option control files.  Because of requirement
** of preserving order of entry as well as limited execution
** requirements, simple linear list methodology is employed.
*/
# include <stdio.h>
	extern char
*malloc(), *strchr();
	extern double
atof(), dmstor();
	static
struct OPT_LIST { /* option list */
	char *opt;
	char *arg;
	struct OPT_LIST *next;
} *opt_list = (struct OPT_LIST *)0, *p, *q;
	int
dump_opt(opt, arg) char **opt, **arg; {
	static int init = 0;
	static struct OPT_LIST *lp;

	if (!init) {
		lp = opt_list;
		init = 1;
	}
	if (lp) {
		*opt = lp->opt;
		*arg = lp->arg;
		lp = lp->next;
		return 1;
	} else
		return 0;
}
	void
load_opt(opt, copy) char *opt; {
	char *s;

	if (*opt == '+')
		++opt;
	if (copy) { /* save string */
		if (!(s = malloc(strlen(opt)+1)))
			emess(2,"memory allocation for option");
		strcpy(s, opt);
		opt = s;
	}
	if (!(p = (struct OPT_LIST *)malloc(sizeof(struct OPT_LIST))))
		emess(2,"memory allocation for option list");
	p->opt = opt;
	if (p->arg = strchr(opt, '=')) {
		*(p->arg++) = '\0';
		if (!*p->arg)
			p->arg = (char *)0;
	}
	p->next = (struct OPT_LIST *)0;
	/* add to list */
	if (opt_list) {
		for (q = opt_list; q->next; q = q->next) ;
		q->next = p;
	} else
		opt_list = p;
}
	int /* load parameters from a file */
file_opt(name) char *name; {
	FILE *fid;
	char option[100]; /* should be big enough */
	int c;

	if (!(fid = fopen(name, "r"))) {
		emess(-2,"parameter option file");
		return 1;
	}
	while (fscanf(fid,"%s",option) == 1) {
		if (*option == '#') { /* check for and flush comments */
			while((c = fgetc(fid)) >= 0 && c != '\n') ;
			if (c < 0)
				break;
			else
				continue;
		}
		load_opt(option, 1);
	}
	fclose(fid);
	return 0;
}
/* parameter input for projection functions
** opt	::= string with 'tag'+id
**	see switch list for 'tag' types supported.
** def	::= string of value to be employed if 'id'
**	not found in input list.
** Returns pointer to union of appropriate value types.
*/
	char *
param(opt, def) char *opt, *def; {
	int type;
	static union {
		double	f;
		int	i;
		char	s; /* dummy for compiler */
	} value;

	type = *opt++;
	/* simple linear lookup */
	for (p = opt_list; p ; p = p->next)
		if (!strcmp(p->opt, opt)) {
			if (p->arg) def = p->arg;
			break;
		}
	if (type != 'b')	/* not boolean */
		switch (type) {
		case 'i':	/* integer input */
			value.i = atoi(def);
			break;
		case 'd':	/* simple real input */
			value.f = atof(def);
			break;
		case 'r':	/* degrees input */
			value.f = dmstor(def, 0);
			break;
		case 's':	/* char string */
			return (def);
		}
	else /* return 1 if present (ie. didn't fall out of lookup loop */
		value.i = p != (struct OPT_LIST *)0;
	/* casting of call will extract proper type */
	return ( &value.s );
}
