/* cont.c */
void contour (double *,int,struct Map_info,DCELL **,struct Cell_head,FILE *,int,int,int);
int checkedge (DCELL,DCELL,double);
/* line_cent.c */
int get_line_center(double *, double *, struct line_pnts *);
/* main.c */
DCELL **get_z_array (int, int,int, int);
double *getlevels(struct Option *, struct Option *, struct Option *, struct Option *, struct FPRange *, int *, int);
void displaceMatrix(DCELL**, int, int, double*, int, int);
void writeCategorySupport(double* lev, int nlevels, char* vName, char* mName);
