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

/* declare resampling methods */
void p_nearest();
void p_bilinear();
void p_cubic();


/* modify this table to add new methods */
struct menu menu[] = {
	{ p_nearest,	"nearest",	"nearest neighbor" },
	{ p_bilinear,	"bilinear",	"bilinear" },
	{ p_cubic,	"cubic",	"cubic convolution" },
	{0,0,0} };

#endif
