#ifndef __GLOBAL_H__
#define __GLOBAL_H__


#ifdef MAIN
    int n_polygons = 0;
#else
    extern int n_polygons;
#endif


/* centroid structure */
typedef struct {
    double x, y;
    struct line_cats *cats;
    int valid;
} CENTR;


#endif /* __GLOBAL_H__ */
