extern int             KMAX2, KMIN, KMAX;
extern struct quadtree *root;
extern int             NPOINT;
extern int             OUTRANGE;
extern int             NPT;

extern int             nsizr, nsizc,total;


extern char           *input;
extern char           *maskmap;
extern char           *mapset ;
extern char           *elev ;
extern char           *slope ;
extern char           *aspect ;
extern char           *pcurv ;
extern char           *tcurv ;
extern char           *mcurv ;
extern int            which_att;


extern double /* pargr */ ns_res, ew_res;

extern struct BM *bitmask;
extern double /* datgr */ *az, *adx, *ady, *adxx, *adyy, *adxy;
extern double /* error */ ertot, ertre,zminac,zmaxac,dmin,zmult;


extern double          DETERM;
extern int             NERROR, cond1, cond2;

extern FILE           *fdinp, *fdredinp, *fdzout, *fddxout, *fddyout, *fdxxout,
               *fdyyout, *fd4, *fxyout;

extern int            sdisk,disk;
extern FILE *Tmp_fd_z;
extern char *Tmp_file_z; 
extern FILE *Tmp_fd_dx;
extern char *Tmp_file_dx;
extern FILE *Tmp_fd_dy;
extern char *Tmp_file_dy;
extern FILE *Tmp_fd_xx;
extern char *Tmp_file_xx;
extern FILE *Tmp_fd_yy;
extern char *Tmp_file_yy;
extern FILE *Tmp_fd_xy;
extern char *Tmp_file_xy; 

extern struct Cell_head cellhd;





