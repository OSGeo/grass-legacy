#ifndef NULL
#define NULL 0
#endif

#define DELETED -2

#define MAXSITES 4096

#define le 0
#define re 1

extern struct Site *(*next)();
extern int triangulate, sorted, plot, debug, outputmode;
extern float xmin, xmax, ymin, ymax, deltax, deltay;
extern float pxmin, pxmax, pymin, pymax;

extern char execdir[];		/* from voronoi.c */

extern int numvedges, numdedges;

extern int total_alloc;	    /* memory.c */
extern int menu;	    /* menus.c */

/* GL DRAWING STUFF */

/* vertex: x, y */
struct vertstruct {
    float x, y;
    int pid;
    };

/* vertex: x, y, theta */
struct vertthetastruct {
    float x, y, theta;
    int e1, e2;
    };

/* edge: CLIPPED to reasonable bounds */
struct edgestruct {
    float x1, y1, x2, y2;
    int nbr1, nbr2;
    float xm, ym;	    /* halfway between inducing voronoi sites */
    };

typedef struct vertstruct VERT;
typedef struct vertthetastruct VERTTHETA;
typedef struct edgestruct EDGE;

/* triangle: holds POINTERS to three verts */
struct tristruct {
    VERT *v1, *v2, *v3;
    };

struct circstruct {
    float cx, cy, r;
    int nbr1, nbr2, nbr3;
    };

typedef struct tristruct TRI;
typedef struct circstruct CIRC;

#define MAXVERTS (3 * MAXSITES)
#define MAXEDGES (2 * MAXSITES)
#define MAXTRIS  (MAXEDGES)

extern VERT GLsites[MAXVERTS];
extern VERT verts[MAXVERTS];
extern EDGE vedges[MAXEDGES];
extern EDGE dedges[MAXEDGES];
extern TRI  tris[MAXTRIS];
extern CIRC circles[MAXTRIS];

/* END GL DRAWING STUFF */

struct	Freenode	{
    struct Freenode *nextfree;
};

struct	Freelist	{
    struct Freenode *head;
    int	nodesize;
};

struct Point {
    float x,y;
};

/* structure used both for sites and for vertices */
struct Site {
    struct Point coord;
    int sitenbr;
    int refcnt;
};

typedef struct Site SITE;

struct Edge {
    float a,b,c;
    struct Site *ep[2];
    struct Site	*reg[2];
    int	edgenbr;
};

extern struct Site *sites;
extern int nsites;
extern int siteidx;
extern int sqrt_nsites;
extern int nvertices;
extern struct Freelist sfl;
extern struct Site *bottomsite;

extern struct Site *nextone();	    /* from voronoi.c */

extern int nedges;
extern struct	Freelist efl;

void v_endpoint();
int has_endpoint(),right_of();
struct Site *intersect();
float dist();
struct Point PQ_min();
struct Halfedge *PQextractmin();
struct Edge *bisect();

struct Halfedge {
    struct Halfedge *ELleft, *ELright;
    struct Edge	*ELedge;
    int	ELrefcnt;
    char ELpm;
    struct Site	*vertex;
    float ystar;
    struct Halfedge *PQnext;
};

extern struct Freelist	hfl;
extern struct Halfedge *ELleftend, *ELrightend;
extern int ELhashsize;
extern struct Halfedge **ELhash;

extern int PQhashsize;
extern struct Halfedge *PQhash;
extern int PQcount;
extern int PQmin;

int PQempty();
struct Halfedge *PQfind();
/* edgelist.c */
struct Halfedge *HEcreate(struct Edge *, int);
int ELinsert(struct Halfedge *, struct Halfedge *);
struct Halfedge *ELgethash(int);
struct Halfedge *ELleftbnd(struct Point *);
int ELdelete(struct Halfedge *);
struct Halfedge *ELright(struct Halfedge *);
struct Halfedge *ELleft(struct Halfedge *);
struct Site *leftreg(struct Halfedge *);
struct Site *rightreg(struct Halfedge *);

/* geometry.c */
struct Edge *bisect(struct Site *, struct Site *);
struct Site *intersect(struct Halfedge *, struct Halfedge *);
int right_of(struct Halfedge *, struct Point *);
void v_endpoint(struct Edge *, int, struct Site *);
float dist(struct Site *, struct Site *);
int makevertex(struct Site *);
int deref(struct Site *);
int ref(struct Site *);
/* heap.c */
int PQinsert(struct Halfedge *, struct Site *, double);
int PQdelete(struct Halfedge *);
int PQbucket(struct Halfedge *);
int PQempty(void);
struct Point PQ_min(void);
struct Halfedge *PQextractmin(void);
int PQinitialize(void);
/* memory.c */
int freeinit(struct Freelist *, int);
char *getfree(struct Freelist *);
int makefree(struct Freenode *, struct Freelist *);
char *myalloc(unsigned);
void myfreeall(void);
/* voronoi.c */
int scomp(struct Point *, struct Point *);
int GLscomp(VERT *, VERT *);
struct Site *nextone(void);
int sortGLsites(void);
int voronoi(struct Site *(*)(void));
/* vregion.c */
int out_site(struct Site *);
int out_bisector(struct Edge *);
int out_ep(struct Edge *);
int out_vertex(struct Site *);
int out_triple(struct Site *, struct Site *, struct Site *);
int clip_line(struct Edge *);
int vtcomp(VERTTHETA *, VERTTHETA *);
int find_dtriangles(TRI **);
