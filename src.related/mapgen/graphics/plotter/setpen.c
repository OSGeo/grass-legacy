#ifndef lint
static char *SCCSID = "@(#)setpen.c	USGS v.4.1";
#endif
/* basic PEN structure control */
# define PLOTTER
# include "plotter.h"
# include "graphics.h"

extern PEN *pen;

extern int error;
	static PEN
pen_array[MAX_PENS];
	static PEN *
pen_pool = pen_array;
	void
init_pens() {
	int i;

	for (i = 1; i < MAX_PENS; ++i)
		pen_array[i].next = pen_pool++;
}
	/* create new pen structure */
newpen(s) char *s; {
	PEN *p, *p2;
	char *s2, *strchr();
	int  solid();

	if (s2 = strchr(s, ':')) { /* set template, if any */
		*s2++ = '\0';
		for (p2 = pen; p2 && strncmp(s2,p2->name,MAXPEN);
			p2 = p2->next) ;
		if (!p2)
			return(error = E_NOTMPL);
	}

		/* find end of list or equal name */
	for (p = pen; p && strncmp(s,p->name,MAXPEN); p = p->next) ;

	if (p)  /* blow out if name found */
		return(error = E_EQNAME);
	if (!(p = pen_pool))
		return(error = E_PALLOC);
	pen_pool = p->next;
	if (s2) *p = *p2;
	else {
		memset(p, 0, sizeof(PEN));
		p->xy = &p->local;
		max_x_y(&p->local.xhi, &p->local.yhi);
		p->line = solid;
	}
	strncpy(p->name,s,MAXPEN);
	p->next = pen;
	pen = p;
	return(0);
}
	/* select existing pen structure */
setpen(s) char *s; {
	PEN *p, *l;

	l = 0;
	for (p = pen; p && strncmp(p->name,s,MAXPEN); p = p->next)
		l = p;
	if (p) { /* update order on demand basis */
		if (p == pen)
			return(0);
		if (l)
			l->next = p->next;
		p->next = pen;
		pen = p;
		return(0);
	} else { /* leave everything as is */
		error = E_NOPEN;
		return(1);
	}
}
	/* delete current pen structure */
delpen() {
	PEN *p;
		/* unlink any other pens */
	for (p = pen->next; p ; p = p->next)
		if (p->xy == &pen->local)
			p->xy = &p->local;
	p = pen->next;
	pen->next = pen_pool;
	pen_pool = pen;
	pen = p;
}
	/* link x-y to another pen */
linkxy(s) char *s; {
	PEN *p;

	for (p = pen->next; p && strncmp(s,p->name,MAXPEN); p = p->next) ;

	if (p) pen->xy = p->xy;
	else  /* can't find pen */
		return(error = E_LINK);
	return(0);
}
	/* delink current pen x-y and establish as local */
delink() {
	pen->xy = &pen->local;
}
