/* cont.c */
void contour (double *,int,struct Map_info,DCELL **,struct Cell_head,int,int,int);
int checkedge (DCELL,DCELL,double);
/* main.c */
DCELL **get_z_array (int, int,int, int);
double *getlevels(struct Option *, struct Option *, struct Option *, struct Option *, struct FPRange *, int *, int);
void displaceMatrix(DCELL**, int, int, double*, int, int);
void writeCategorySupport(double* lev, int nlevels, char* vName, char* mName);
