#ifndef lint
static char *SCCSID = "@(#)setform.c	AMG v.3.1";
#endif
# include <string.h>

/* maximum number of fields which can be specified */
# define MAX_LIST 20

static	int last = 0;
static	struct LIST {	/* input field definitions	*/
	short n,m,l;	/* field, offset and length	*/
} list[MAX_LIST];

static char *argvl[2 * MAX_LIST+1];

static short sl[MAX_LIST+1]; /* list ordered by field (n) */

	/* field sort compare */
static cmp(i, j) short *i, *j; { return (list[*i].n - list[*j].n); }

setform(s, offset) char *s; {
	int i, num;

	if (offset) last = offset;
	for (num = 0, i = offset; i < MAX_LIST && s && *s; ++i) {
		list[i].m = list[i].l = 0;
		list[i].n = strtol(s, &s, 10);
		if (*s == '.') {
			list[i].m = strtol(++s, &s, 10);
			if (*s == '.')
				list[i].l = strtol(++s, &s, 10);
		}
		if (*s && (s = strchr(s, ','))) ++s;
		if (list[i].n <= 0) list[i].n = 1;
		++num;
	}
	if (i > last) last = i;
	for (i = 0; i < last; ++i) sl[i] = i;
	qsort(sl, last, sizeof (sl[0]), cmp);
	return (num); /* return number of values entered */
}

groups(line, delim, func) char *line; int (*func)(); {
	char *end, *s0, *s1, *a0, *a1;
	int t, n, i, minp, p;

	minp = -1;
	end = line + strlen(line);
	if (!(s1 = strchr(s0 = line, delim))) s1 = end;
	for (t = 1, i = 0; i < last ; ++i) {
		n = list[p = sl[i]].n;
		for (; t < n && *s1 ; ++t)
			if (!(s1 = strchr(s0 = s1+1, delim))) s1 = end;
		if (t == n && *s0 && ((a0 = s0 + list[p].m) <= end)) {
			if (a1 = (char *)list[p].l) {
				if ((a1 += (int)a0) > end) a1 = end;
			} else
				a1 = s1;
			if (p > minp) minp = p;
		} else
			a0 = a1 = 0;
		argvl[p += p] = a0;
		argvl[++p] = a1;
	}
	return((*func)(minp+1));
}

static char savec, *savead;

	char *
setfield(n) { register char *s;
	if (savead = s = argvl[n+=n]) {
		savec = *(savead = argvl[++n]);
		*savead = '\0';
	} else s = "";
	return (s);
}

	void
resetf() { if (savead) *savead = savec; }
