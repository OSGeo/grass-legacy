/* v.out.dxf.c */
int dxf_open(char *);

int dxf_limits(double, double, double, double);

int dxf_solidline(void);
int dxf_layer0(void);
int dxf_layer(char *, int, char *, int);
int dxf_polyline(char *);
int dxf_vertex(char [], double, double, double);
int dxf_text(char [], double, double, double, double, int, char []);
int init_vars(char *[]);

double do_limits(struct Map_info *);
int make_layername( void );

int add_plines( struct Map_info *, double );

int add_text(double);
int do_eof(void);
