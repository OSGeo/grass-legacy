/* Matrici (Righe, Colonne) */

void normalDefBicubic(double **N, double *TN, double *Q, double **obsVect,
		      double deltaX, double deltaY, int xNum, int yNum,
		      double xMin, double yMin, int obsNum, int parNum, int BW);


void normalDefBilin(double **N, double *TN, double *Q, double **obsVect,
		    double deltaX, double deltaY, int xNum, int yNum,
		    double xMin, double yMin, int obsNum, int parNum, int BW);

void nCorrectLapl(double **N,	/* Matrice normale () */
		  double lambda,	/*  */
		  int xNum,	/*  */
		  int yNum,	/*  */
		  double deltaX,	/*  */
		  double deltaY);	/*  */



void nCorrectGrad(double **N, double lambda, int xNum, int yNum, double deltaX,
		  double deltaY);

void obsEstimateBicubic(double **obsV,	/*  */
			double *obsE,	/*  */
			double *parV,	/*  */
			double deltX,	/*  */
			double deltY,	/*  */
			int xNm,	/*  */
			int yNm,	/*  */
			double xMi,	/*  */
			double yMi,	/*  */
			int obsN);	/*  */

double dataInterpolateBicubic(double x,	/*  */
			      double y,	/*  */
			      double deltaX,	/*  */
			      double deltaY,	/*  */
			      int xNum,	/*  */
			      int yNum,	/*  */
			      double xMin,	/*  */
			      double yMin,	/*  */
			      double *parVect);	/*  */

void obsEstimateBilin(double **obsV, double *obsE, double *parV, double deltX,
		      double deltY, int xNm, int yNm, double xMi, double yMi,
		      int obsN);

double dataInterpolateBilin(double x, double y, double deltaX, double deltaY,
			    int xNum, int yNum, double xMin, double yMin,
			    double *parVect);
