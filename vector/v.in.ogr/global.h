
#ifdef MAIN
    int n_polygons = 0;
#else
    extern n_polygons;
#endif


/* centroid structure */
typedef struct {
    double x, y;
    struct line_cats *cats;
    int valid;
} CENTR;
