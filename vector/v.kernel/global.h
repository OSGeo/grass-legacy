#include "gis.h"

double euclidean_distance(double *x, double *y, int n);
double gaussian2dBySigma( double d, double sigma );
double invGaussian2d(double sigma, double prob);
double gaussian2dByTerms(double d, double term1, double term2);
double brent(double ax, double bx, double cx, double (*f)(), double tol, double *xmin);
double kernel1(double d, double rs, double lambda);
double segno(double x);

int read_points(struct Map_info *In, double ***coordinate, double dsize);
double compute_all_distances(double **coordinate, double **dists, int n, double dmax);
double compute_all_net_distances(struct Map_info *In, struct Map_info *Net, double netmax, 
	                         double **dists, double dmax);
void compute_distance( double N, double E, struct Map_info *In,
	                       double term1, double term2, double *gaussian, double dmax);
void compute_net_distance( double x, double y, struct Map_info *In, struct Map_info *Net, double netmax,
	                       double term1, double term2, double *gaussian, double dmax);

