/* load.c */
Site **gu_load_cached_sites(FILE *, int, int, int, struct Cell_head *, int *);
int site_mem(Site *, int);
int compress_cached_site(Site *, int);
int gu_free_cached_sites(Site **, int);
/* main.c */
int copy_sitedata(Site *, Site *);
/* territory.c */
double distance(double [2], double [2]);
double calc_min_territory(double, double, double, double, FCELL *, FCELL *, struct Cell_head *, int);
int write_imap_fromf(int, FCELL *, struct Cell_head *);
int write_fmap(FCELL *, double, struct Cell_head *);
int show_box(FCELL *, double, struct Cell_head *, int, int, int, int);
int calc_simultaneous_territory(Site **, int, int, int, FCELL *, FCELL *, struct Cell_head *, int);
