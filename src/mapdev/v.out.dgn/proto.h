int wshort( FILE *, int );
int wlong( FILE *, int );
int wtcb( FILE *);
int wehead( FILE *, EHEAD *, int);
int wmslink( FILE *, EHEAD *);

int wpoint( FILE *, EHEAD *, int );
int wline( FILE *, EHEAD *, int, int );
int warea( FILE *, EHEAD *, int, int );
int wparts( FILE *, EHEAD *, int );
int wcell( FILE *, EHEAD *, int, int, double, double);
int wtext( FILE *, EHEAD *, char *, double, double );

int line_cent( struct line_pnts *, double *, double*);

