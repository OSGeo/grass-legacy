
#define	QUAD_SIZE	7.30  /*  7 degrees, 30 seconds  */
#define	QUAD_SIZE_SEC	450.00  /*  all in seconds  */


struct quads_description
{
    int    zone ;		/*  UTM zone */
    int    north ;		/*  true if north hemisphere  */
    int    east ;		/*  true if east hemisphere  */
    int    spheroid_num ;	/*  index to spheroid array  */
    char   *spheroid_name ;	/*  spheroid name            */
    double origin_lat ;        /*  lower left point of grid  */
    double origin_lon ;
    double origin_x ;        /*  lower left point of grid  */
    double origin_y ;		/*  in UTM's  */
    double lat_shift ;		/* positive or negative quad size */
    double lon_shift ;		/* which direction we go  */
    int    num_rows ;
    int    num_cols ;
    int    num_vect_rows ;
    int    num_vect_cols ;
};



/*  flags that get set from the command line  */
struct command_flags
{
    int    vectors ;  	/* create the vector file of quads  */
    int    sites ;	/*  site file of quad points  */
    int    windows ;	/*  window files  from quad points  */
    int    encompass ;	/*  encompass map */
    int    reg ;	/*  registration file for `digit`  */
    int    files ;
    int    usage ;
};

#ifdef GRASS_GIS_H
/* calc_quads.c */
int calculate_quads(struct quads_description *, struct Cell_head *, struct command_flags *);
/* convert.c */
int setup_ll_to_utm(struct quads_description *);
int find_quad_point(struct quads_description *, struct Cell_head *, struct command_flags *);
/* init_quad.c */
int init_quad_struct(struct quads_description *, struct Cell_head *);
/* window_quads.c */
int window_quads ( struct quads_description *, struct Cell_head *);
#endif
/* convert.c */
int convert_ll_to_utm(double, double, double *, double *, struct quads_description *);
int print_quad(struct quads_description *);

/* mkquads_cm.c */
int check_args(int, char *[], char *, struct command_flags *);
int dashed_args(char *, struct command_flags *);
/* reg_quads.c */
int reg_quads(FILE *, struct quads_description *);
/* report_quads.c */
int report_quads(FILE *, struct quads_description *, struct command_flags *);
/* sites_quads.c */
int sites_quads(FILE *, struct quads_description *);

#ifdef GRASS_VECT_H
/* write_quads.c */
int write_quads(FILE *, struct quads_description *, struct Map_info *);
#endif
