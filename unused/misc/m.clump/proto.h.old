#ifdef __STDC__
# define	P(s) s
#else
# define P(s) ()
#endif


/* attr.c */
int init_attributes P((char **f ));
int attributes_are_the_same P((int pt1 , int pt2 ));
int have_attributes P((void ));

/* barriers.c */
char *get_barrier_name P((int i ));
void init_barriers P((char **names ));

/* bbox.c */
void get_bounding_box P((double *x , double *y , int n , struct box *box ));
void get_arc_bounding_box P((struct arc *arc , struct box *box ));
void get_pointlist_bounding_box P((struct box *box ));
int segment_may_fall_in_box P((double x1 , double y1 , double x2 , double y2 , struct box *box ));
int boxes_overlap P((struct box *box1 , struct box *box2 ));

/* break.c */
void break_connections P((void ));

/* disconnect.c */
int disconnect P((int pt1 , int pt2 ));

/* fields.c */
int *get_field_list P((char **fields , int *count ));
char *find_fs P((char *buf , char *fs ));
int get_field P((char *buf , int which , char *fs , char *word ));

/* inci.c */
void inciMakeIncidences P((void *g , indexType **loi , int *n ));
void inciInitIncidences P((void *g ));
void inciEnd P((void ));
int inciGetFirstIncidence P((void *g , indexType site , indexType *qe ));
int inciGetNextIncidence P((void *g , indexType *qe ));

/* intersect.c */
int arc_intersects_segment P((struct arc *arc , double x1 , double y1 , double x2 , double y2 ));
int segments_intersect P((double ax1 , double ay1 , double ax2 , double ay2 , double bx1 , double by1 , double bx2 , double by2 ));

/* list.c */
int *get_neighbor_list P((int pt , int *count ));
int get_number_of_neighbors P((int pt ));
int get_neighbor P((int pt , int n ));
void unset_neighbor P((int pt , int n ));

/* main.c */
int main P((int argc , char *argv []));
int be_quiet P((void ));

/* neighbors.c */
void doMakeNeighbors P((void *g , int ***neighbors , int *n ));
void makeNeighbors P((double *x , double *y , int nofSites , int ***neighbors ));
void disposeNeighbors P((int **neighbors , int nofSites ));

/* parse.c */
void parse_command_line P((int argc , char *argv [], struct parms *parms ));

/* read.c */
void read_point_list P((char *filename , char *fs ));
void extend_pointlist P((struct pointlist *list , int n ));
int is_comment P((char *buf ));
int get_xy P((char *buf , double *x , double *y , char *fs ));

/* readline.c */
int readline P((FILE *fd , char *buf , int len ));

/* region.c */
void set_region P((void ));
int have_region P((void ));
int point_in_region P((double x , double y ));

/* report.c */
int report_point P((FILE *fd , int pt ));
void read_point P((FILE *fd , long offset , char *buf , int len ));
void write_point P((FILE *fd , char *buf , char *indent ));
void read_and_print P((FILE *outfd , FILE *infd , long offset , char *indent ));

/* triangulate.c */
void triangulate_point_list P((void ));

/* vect.c */
char *find_vector P((char *name ));
void open_vector_map P((char *name ));
void close_vector_map P((void ));
int read_next_vector_arc P((struct arc *arc ));

/* write.c */
void write_results P((char *filename ));

#undef P
