#ifndef lint
static char *SCCSID = "@(#)rdbicoef.c	AMG v.3.1";
#endif
/* read bivariate structure */
# include "bieval.h"
# include "stdio.h"
rdbicoef(fid, P) FILE *fid; BICOEF *P; {
	int	cnp(), mnp();
	double	*malloc();

	fscanf(fid,"%d %d %d %d",
		&P->type,&P->x_deg,&P->y_deg,&P->maxC);
	switch (P->type) {
	case 0:	P->poly = mnp;
		break;
	case 1:	P->poly = cnp;
		break;
	default:
		fprintf(stderr,"invalid polynomial type\n");
		return 1;
	}
	rgroup(fid, &P->x_scale, 8);
	P->Tx = malloc((P->x_deg+P->y_deg+P->maxC+2)*sizeof(double));
	P->Ty = P->Tx + P->x_deg + 1;
	P->Cxy = P->Ty + P->y_deg + 1;
	rgroup(fid, P->Cxy, P->maxC);
	return 0;
}
