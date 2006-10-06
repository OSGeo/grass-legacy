#include <grass/gis.h>
#include <grass/Vect.h>
#include <grass/dbmi.h>
#include <grass/glocale.h>

#include <grass/PolimiFunct.h>

#define PI 3.141592

/*---------------------------------------------------------------------------------------*/
int edge_detection (struct Cell_head, 	/**/
		    BOUND_BOX, 			/**/
		    double *, 			/**/
		    double, 			/**/
		    double, 			/**/
		    double*, 			/**/
		    double, 			/**/
		    double, 			/**/
		    double, 			/**/
		    double			/**/);

double * 
Get_Gradient (struct Cell_head,		/**/
	  	double, 			/**/
	  	double, 			/**/
	  	double* 			/**/);

void classification (struct Map_info *, 	/**/
		     struct Cell_head,	 	/**/
		     BOUND_BOX, 	 	/**/
		     BOUND_BOX, 	 	/**/
		     double **,	 	/**/ 
		     double *, 	 	/**/
		     double *, 	 	/**/
		     double, 		 	/**/
		     double, 		 	/**/
		     double, 		 	/**/
		     double, 		 	/**/
		     double, 		 	/**/
		     int *, 		 	/**/
		     int, 		 	/**/
		     dbDriver *,		/**/
		     char *			/**/);

int Insert (double,				/**/
	    double, 				/**/
	    double, 				/**/
	    int, 				/**/
	    dbDriver *				/**/);

int UpDate (double,				/**/ 
	    double,				/**/ 
	    double, 				/**/
	    int,				/**/ 
	    dbDriver *				/**/);

int Select (double *,				/**/ 
	    double *, 				/**/
	    double *, 				/**/
	    int, 				/**/
	    dbDriver *				/**/);

int Create_AuxEdge_Table (dbDriver *, char*);
int Create_Interpolation_Table (char *, dbDriver *);
int Insert_Interpolation (double, int, dbDriver *, char *);
