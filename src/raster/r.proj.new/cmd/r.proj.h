/* @(#)r.proj.h	v1.2 - 27 Jun 1995 	-emes- */

#ifndef R_PROJ_H
#define R_PROJ_H

typedef void (*func)();

struct menu
{
    func  method;	/* routine to interpolate new value	 */
    char *name;  	/* method name				 */
    char *text;		/* menu display - full description	 */
};
int bordwalk(struct Cell_head *, struct Cell_head *, struct pj_info *, struct pj_info *, char[256]);
FCELL **readcell(int);
/* declare resampling methods */
/* bilinear.c */
void p_bilinear(FCELL **, void *, int, double *, double *, struct Cell_head *);
/* cubic.c */
void p_cubic(FCELL **, void *, int, double *, double *, struct Cell_head *);
/* nearest.c */
void p_nearest(FCELL **, void *, int, double *, double *, struct Cell_head *);


/* modify this table to add new methods */
struct menu menu[] = {
	{ p_nearest,	"nearest",	"nearest neighbor" },
	{ p_bilinear,	"bilinear",	"bilinear" },
	{ p_cubic,	"cubic",	"cubic convolution" },
	{0,0,0} };

#endif
