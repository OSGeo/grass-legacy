void tcholDec(double **N, double **T, int n, int BW);

void tcholSolve(double **N, double *TN, double *parVect, int n, int BW);

void tcholSolve2(double **N, double *TN, double **T, double *parVect, int n,
		 int BW);

void tcholInv(double **N, double *invNdiag, int n, int BW);

void tcholSolveInv(double **N, double *TN, double *invNdiag, double *parVect,
		   int n, int BW);
