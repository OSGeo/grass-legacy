/* raster_ptr.c: From Huidae Cho ...*/
union RASTER_PTR
{
	void	*v;
	CELL	*c;
	FCELL	*f;
	DCELL	*d;
};

struct RASTER_MAP_PTR
{
	RASTER_MAP_TYPE		type;
	union RASTER_PTR	data;
};

int is_null_value(struct RASTER_MAP_PTR buf, int col);
/* End from Huidae Cho */

/* Put all the state infomation into a struct */
struct rr_state {
    char *inraster, *outraster, *mapset, *outsites;
    int  use_nulls, verbose, fd_old, fd_new;
    long nCells, nNulls, nRand;
    struct RASTER_MAP_PTR nulls, buf, min, max;
    FILE *fsites;
};


/* count.c */
void
get_stats(struct rr_state *);
/* creat_rand.c */
long make_rand(void);
void init_rand(void);
long make_rand(void);
void init_rand(void);
/* random.c */
int execute_random(struct rr_state *);
/* support.c */
int make_support (struct rr_state *, int );
/* vim: set softtabstop=4 shiftwidth=4 expandtab: */
