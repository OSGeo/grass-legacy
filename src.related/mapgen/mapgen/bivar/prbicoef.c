#ifndef lint
static char *SCCSID = "@(#)prbicoef.c	AMG v.3.1";
#endif
/* print bivariate structure */
# include "bieval.h"
# include "stdio.h"
prbicoef(fid, P) FILE *fid; BICOEF *P; {
	fprintf(fid,"%d %d %d %d\n",P->type,P->x_deg,P->y_deg,P->maxC);	
	pgroup(fid, &P->x_scale, 8);
	pgroup(fid, P->Cxy, P->maxC);
}
