#if defined(__STDC__) || defined(__cplusplus)
# define P_(s) s
#else
# define P_(s) ()
#endif


/* adjlz0.c */
double adjlz0_ P_((double *lon));

/* asinz0.c */
double asinz0_ P_((double *con));

/* blkdta.c */

/* dmslz0.c */
int dmslz0_ P_((double *ang, int *iunit, char *dms, FILE *ipfile, int *iflg));

/* e0fnz0.c */
double e0fnz0_ P_((double *eccnts));

/* e1fnz0.c */
double e1fnz0_ P_((double *eccnts));

/* e2fnz0.c */
double e2fnz0_ P_((double *eccnts));

/* e3fnz0.c */
double e3fnz0_ P_((double *eccnts));

/* e4fnz0.c */
double e4fnz0_ P_((double *eccent));

/* gtrnz0.c */
int gtrnz0_ P_((double *crdin, int *indef, double *tparin, double *crdio, int *iodef, double *tpario, FILE *ipfile, int *iflg));

/* mlfnz0.c */
double mlfnz0_ P_((double *e0, double *e1, double *e2, double *e3, double *phi));

/* msfnz0.c */
double msfnz0_ P_((double *eccent, double *sinphi, double *cosphi));

/* paksz0.c */
double paksz0_ P_((double *ang, FILE *ipfile, int *iflg));

/* phi1z0.c */
double phi1z0_ P_((double *eccent, double *qs, FILE *ipfile, int *iflg));

/* phi2z0.c */
double phi2z0_ P_((double *eccent, double *ts, FILE *ipfile, int *iflg));

/* phi3z0.c */
double phi3z0_ P_((double *ml, double *e0, double *e1, double *e2, double *e3, FILE *ipfile, int *iflg));

/* phi4z0.c */
double phi4z0_ P_((double *eccnts, double *e0, double *e1, double *e2, double *e3, double *a, double *b, double *c, FILE *ipfile, int *iflg));

/* pj01z0.c */
int pj01z0_0_ P_((int n__, int *zone, double *data, FILE *ipfile, int *iflg, double *geog, double *proj));
int pj01z0_ P_((void));
int is01z0_ P_((int *zone, double *data, FILE *ipfile, int *iflg));
int pf01z0_ P_((double *geog, double *proj, int *iflg));
int pi01z0_ P_((double *proj, double *geog, int *iflg));

/* pj02z0.c */
int pj02z0_0_ P_((int n__, int *zone, double *data, FILE *ipfile, int *iflg, double *geog, double *proj));
int pj02z0_ P_((void));
int is02z0_ P_((int *zone, double *data, FILE *ipfile, int *iflg));
int pf02z0_ P_((double *geog, double *proj, int *iflg));
int pi02z0_ P_((double *proj, double *geog, int *iflg));
int ZoneListing P_((FILE *fp));

/* pj03z0.c */
int pj03z0_0_ P_((int n__, int *zone, double *data, FILE *ipfile, int *iflg, double *geog, double *proj));
int pj03z0_ P_((void));
int is03z0_ P_((int *zone, double *data, FILE *ipfile, int *iflg));
int pf03z0_ P_((double *geog, double *proj, int *iflg));
int pi03z0_ P_((double *proj, double *geog, int *iflg));

/* pj04z0.c */
int pj04z0_0_ P_((int n__, int *zone, double *data, FILE *ipfile, int *iflg, double *geog, double *proj));
int pj04z0_ P_((void));
int is04z0_ P_((int *zone, double *data, FILE *ipfile, int *iflg));
int pf04z0_ P_((double *geog, double *proj, int *iflg));
int pi04z0_ P_((double *proj, double *geog, int *iflg));

/* pj05z0.c */
int pj05z0_0_ P_((int n__, int *zone, double *data, FILE *ipfile, int *iflg, double *geog, double *proj));
int pj05z0_ P_((void));
int is05z0_ P_((int *zone, double *data, FILE *ipfile, int *iflg));
int pf05z0_ P_((double *geog, double *proj, int *iflg));
int pi05z0_ P_((double *proj, double *geog, int *iflg));

/* pj06z0.c */
int pj06z0_0_ P_((int n__, int *zone, double *data, FILE *ipfile, int *iflg, double *geog, double *proj));
int pj06z0_ P_((void));
int is06z0_ P_((int *zone, double *data, FILE *ipfile, int *iflg));
int pf06z0_ P_((double *geog, double *proj, int *iflg));
int pi06z0_ P_((double *proj, double *geog, int *iflg));

/* pj07z0.c */
int pj07z0_0_ P_((int n__, int *zone, double *data, FILE *ipfile, int *iflg, double *geog, double *proj));
int pj07z0_ P_((void));
int is07z0_ P_((int *zone, double *data, FILE *ipfile, int *iflg));
int pf07z0_ P_((double *geog, double *proj, int *iflg));
int pi07z0_ P_((double *proj, double *geog, int *iflg));

/* pj08z0.c */
int pj08z0_0_ P_((int n__, int *zone, double *data, FILE *ipfile, int *iflg, double *geog, double *proj));
int pj08z0_ P_((void));
int is08z0_ P_((int *zone, double *data, FILE *ipfile, int *iflg));
int pf08z0_ P_((double *geog, double *proj, int *iflg));
int pi08z0_ P_((double *proj, double *geog, int *iflg));

/* pj09z0.c */
int pj09z0_0_ P_((int n__, int *zone, double *data, FILE *ipfile, int *iflg, double *geog, double *proj));
int pj09z0_ P_((void));
int is09z0_ P_((int *zone, double *data, FILE *ipfile, int *iflg));
int pf09z0_ P_((double *geog, double *proj, int *iflg));
int pi09z0_ P_((double *proj, double *geog, int *iflg));

/* pj10z0.c */
int pj10z0_0_ P_((int n__, int *zone, double *data, FILE *ipfile, int *iflg, double *geog, double *proj));
int pj10z0_ P_((void));
int is10z0_ P_((int *zone, double *data, FILE *ipfile, int *iflg));
int pf10z0_ P_((double *geog, double *proj, int *iflg));
int pi10z0_ P_((double *proj, double *geog, int *iflg));

/* pj11z0.c */
int pj11z0_0_ P_((int n__, int *zone, double *data, FILE *ipfile, int *iflg, double *geog, double *proj));
int pj11z0_ P_((void));
int is11z0_ P_((int *zone, double *data, FILE *ipfile, int *iflg));
int pf11z0_ P_((double *geog, double *proj, int *iflg));
int pi11z0_ P_((double *proj, double *geog, int *iflg));

/* pj12z0.c */
int pj12z0_0_ P_((int n__, int *zone, double *data, FILE *ipfile, int *iflg, double *geog, double *proj));
int pj12z0_ P_((void));
int is12z0_ P_((int *zone, double *data, FILE *ipfile, int *iflg));
int pf12z0_ P_((double *geog, double *proj, int *iflg));
int pi12z0_ P_((double *proj, double *geog, int *iflg));

/* pj13z0.c */
int pj13z0_0_ P_((int n__, int *zone, double *data, FILE *ipfile, int *iflg, double *geog, double *proj));
int pj13z0_ P_((void));
int is13z0_ P_((int *zone, double *data, FILE *ipfile, int *iflg));
int pf13z0_ P_((double *geog, double *proj, int *iflg));
int pi13z0_ P_((double *proj, double *geog, int *iflg));

/* pj14z0.c */
int pj14z0_0_ P_((int n__, int *zone, double *data, FILE *ipfile, int *iflg, double *geog, double *proj));
int pj14z0_ P_((void));
int is14z0_ P_((int *zone, double *data, FILE *ipfile, int *iflg));
int pf14z0_ P_((double *geog, double *proj, int *iflg));
int pi14z0_ P_((double *proj, double *geog, int *iflg));

/* pj15z0.c */
int pj15z0_0_ P_((int n__, int *zone, double *data, FILE *ipfile, int *iflg, double *geog, double *proj));
int pj15z0_ P_((void));
int is15z0_ P_((int *zone, double *data, FILE *ipfile, int *iflg));
int pf15z0_ P_((double *geog, double *proj, int *iflg));
int pi15z0_ P_((double *proj, double *geog, int *iflg));

/* pj16z0.c */
int pj16z0_0_ P_((int n__, int *zone, double *data, FILE *ipfile, int *iflg, double *geog, double *proj));
int pj16z0_ P_((void));
int is16z0_ P_((int *zone, double *data, FILE *ipfile, int *iflg));
int pf16z0_ P_((double *geog, double *proj, int *iflg));
int pi16z0_ P_((double *proj, double *geog, int *iflg));

/* pj17z0.c */
int pj17z0_0_ P_((int n__, int *zone, double *data, FILE *ipfile, int *iflg, double *geog, double *proj));
int pj17z0_ P_((void));
int is17z0_ P_((int *zone, double *data, FILE *ipfile, int *iflg));
int pf17z0_ P_((double *geog, double *proj, int *iflg));
int pi17z0_ P_((double *proj, double *geog, int *iflg));

/* pj18z0.c */
int pj18z0_0_ P_((int n__, int *zone, double *data, FILE *ipfile, int *iflg, double *geog, double *proj));
int pj18z0_ P_((void));
int is18z0_ P_((int *zone, double *data, FILE *ipfile, int *iflg));
int pf18z0_ P_((double *geog, double *proj, int *iflg));
int pi18z0_ P_((double *proj, double *geog, int *iflg));

/* pj19z0.c */
int pj19z0_0_ P_((int n__, int *zone, double *data, FILE *ipfile, int *iflg, double *geog, double *proj));
int pj19z0_ P_((void));
int is19z0_ P_((int *zone, double *data, FILE *ipfile, int *iflg));
int pf19z0_ P_((double *geog, double *proj, int *iflg));
int pi19z0_ P_((double *proj, double *geog, int *iflg));

/* pj20z0.c */
int pj20z0_0_ P_((int n__, int *zone, double *data, FILE *ipfile, int *iflg, double *geog, double *proj));
int pj20z0_ P_((void));
int is20z0_ P_((int *zone, double *data, FILE *ipfile, int *iflg));
int pf20z0_ P_((double *geog, double *proj, int *iflg));
int pi20z0_ P_((double *proj, double *geog, int *iflg));

/* qsfnz0.c */
double qsfnz0_ P_((double *eccent, double *sinphi));

/* spakz0.c */
double spakz0_ P_((double *ang, FILE *ipfile, int *iflg));

/* tsfnz0.c */
double tsfnz0_ P_((double *eccent, double *phi, double *sinphi));

/* unitz0.c */
int unitz0_ P_((double *parin, int *inunit, double *pario, int *iounit, FILE *ipfile, int *iflg));

/* untfz0.c */
int untfz0_ P_((int *inunit, int *iounit, double *factor, FILE *ipfile, int *iflg));

/* xtra.c */
double d_sign P_((double *a, double *b));
double pow_dd P_((double *ap, double *bp));
double d_mod P_((double *x, double *y));

#undef P_
