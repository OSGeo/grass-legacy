/*  @(#)structures.h	2.1  6/26/87  */
#define ALLOC_AMT		100

struct endpoints
{
	double x, y ;
	double angle ;
	int node ;
} *endpoints ;

struct lines
{
	int endpoint_beg ;
	int endpoint_end ;
	long offset ;
	short n_points ;
	short right, left ;
	char dig_type ;
} *lines ;

struct areas
{
	double cent_x ;
	double cent_y ;
	double n_bound ;
	double s_bound ;
	double e_bound ;
	double w_bound ;
	int num_lines ;
	int *line_list ;
	int n_islands ;
	int *island_list ;
	int category ;
} *areas ;

struct islands
{
	double cent_x ;
	double cent_y ;
	int num_lines ;
	int *line_list ;
} *islands ;


struct node_lines
{
	int n_lines ;
	int *lines ;
} *node_lines ;

int n_endpts ;
int n_nodes ;
int n_areas ;
int n_lines ;
int n_islands ;

int alloc_lines ;
int alloc_endpoints ;
int alloc_node_lines ;

double y_size, x_size, south_edge, west_edge ;

#define USED	-1
#define UNUSED	0
#define A_BEGIN_NODE	endpoints[areas[cur_area].endpoint_beg].node
#define A_END_NODE		endpoints[areas[cur_area].endpoint_end].node
#define L_BEGIN_NODE	endpoints[lines[cur_line].endpoint_beg].node
#define L_END_NODE		endpoints[lines[cur_line].endpoint_end].node
#define L_NODE			endpoints[dots[cur_dot].endpoint].node
