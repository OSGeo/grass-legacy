#include <stdio.h>
#include <string.h>
#include <math.h>
#include <signal.h>
#include "gis.h"
					/* main.c */ 
					/*
	'r.rational.regression'
	the program establishes RVI (Relaxation Vegetation Index) model
	using rational method of nonlinear regression
					*/


double *vector();
double **matrix();
double rms(), log(), sqrt(), err2();
void function();

main(argc,argv) 
int   argc;
char *argv[];
{
	FILE *fdinp, *fdoutp1, *fdoutp2, *popen();

    double **u, **v, *w, *value,ssto,ssresid,ym;
    double *x,*y,*y2,*xhong,*epsilon,*oldeps,*delteps, *residual;
    double *linx,*lina;
    double t,test,sigma1;
    double *a;				/* regression coefficients */
    double *sigma;			/*  variance of a */
    double *xprediction,*yprediction;
    double rr2;		/* r-squire, coefficients of determinant*/
    double rrr;		/* adjusted r-squire */
    double ff;			/* F test number */
    double eps1;		/* eps1 is (a0*x0+a1*x1+a2*x2+a6) */
    double eps2;   		/* eps2 is (a3*x3+a4*x4+a5*x5+1) */ 
    double p;   		/* p is temperally parameter */
    double r;	/* correlation between observed and calculated y */
    double *xmul, *ymul;
    int i,j,k,ka,ma, iamp;
    int nx,nxhong,nxmul,model;		/* number of variables */
    int na,n1,n2,n3,ndata1,ndata2;	/* number of parameters */
    int ndata, flag_nonlinear, flag_prediction;
    char *other_filename[40];
    char buf[512];
    char *inp, *outp1, *outp2,*title;
    int *niteration, n_max_iteration, n_actual_iteration;

struct
{
        struct Option *input, *output1, *output2, *niteration;
} parm;


        G_gisinit (argv[0]);

						/* define the different options
						*/
        parm.input = G_define_option() ;
        parm.input->key        = "input";
        parm.input->type       = TYPE_STRING;
        parm.input->required   = YES;
        parm.input->description= "File name of imagery to be regressed";

        parm.output1 = G_define_option() ;
        parm.output1->key        = "coefficients";
        parm.output1->type       = TYPE_STRING;
        parm.output1->required   = YES;
        parm.output1->description= "File name to hold regression coefficients";

        parm.output2 = G_define_option() ;
        parm.output2->key        = "report";
        parm.output2->type       = TYPE_STRING;
        parm.output2->required   = NO;
        parm.output2->description= "File name to hold listing of results";
	parm.output2->answer	 = "second.output";

        parm.niteration = G_define_option() ;
        parm.niteration->key        = "niteration";
        parm.niteration->type       = TYPE_INTEGER;
        parm.niteration->required   = YES;
        parm.niteration->description= "Number of iterations";


if (G_parser(argc, argv))
                exit(-1);

        inp = parm.input->answer;
        outp1 = parm.output1->answer;
        outp2 = parm.output2->answer;
	n_max_iteration = atoi(parm.niteration->answer);

    fdoutp1=fopen (outp1, "w");
    fdoutp2=fopen (outp2, "w");
    fdinp = fopen (inp, "r");
    if (fdinp == NULL)
    	{
                sprintf (buf, "%s - not found\n", inp);
                G_fatal_error (buf);
                exit(1);
        }

if (G_legal_filename (outp1) < 0)
        {
                sprintf (buf, "%s - illegal file name\n", outp1);
                G_fatal_error (buf);
                exit(1);
        }
if (G_legal_filename (outp2) < 0)
        {
                sprintf (buf, "%s - illegal file name\n", outp2);
                G_fatal_error (buf);
                exit(1);
        }

					/* read original data */
    	read_data (fdinp, fdoutp1, fdoutp2,
		&xhong, &nxhong, &y, &ndata);

	residual = vector (ndata);
	epsilon = vector (ndata);
	oldeps = vector (ndata);
	delteps = vector (ndata);

	nx = 2*nxhong;
	x = vector (nx*ndata);
	for(i=0; i < ndata; i++)
	   {
	   *(epsilon + i) = 0.0;
	   *(oldeps + i) = 0.0;
	   }
	n_actual_iteration = 0;
iteration:
    	nonlinear_data(xhong,&nxhong,y,epsilon,&ndata,&x,&nx);

    na = nx+1;
    a = vector (na);    		/* parameter array */
    u = matrix (ndata,na);
    v = matrix (na,na);
    w = vector (na);
    value = vector (na);
    y2 = vector (ndata);
						/* linearly fit the model */

    svdfitn(x,nx,y,ndata,a,na,u,v,w,function);

						/**** test matrix v
						*/

for(i=0;i<na;i++)
  for(j=0;j<na;j++)
  {
    test=0.0;
    for(k=0;k<na;k++)
    test += v[k][i]*v[k][j];
  }
					/* check confidential, write sigma */
sigma = vector (na);
for(ka=0;ka<na;ka++)
{
  sigma[ka]=0.0;
  for(ma=0;ma<na;ma++)
  {
  if(w[ma] != 0.0)
    sigma[ka] += (v[ka][ma]/w[ma])*(v[ka][ma]/w[ma]);
  }
}

					/* compute calculated values */
	for (i=0; i < ndata; i++)
	{
	eps1=eps2=0.0;
	  for (k=0; k< nxhong; k++)
	  {
	    eps1 += a[k] * x[i*nx+k];
	    eps2 += a[k+nxhong] * x[i*nx+k];}
	    eps1 += a[nx];
	    eps2 += 1.0;
	    y2[i] = eps1/eps2;
	  }

					/** Check utility: 
	   				check total sum of squares SSto,
	   				residual sum of squares SSresid,
	   				mean value of y: ym,
	   				regression sum of squares
					*/
    printf ("for iteration # %2d------------\n",n_actual_iteration);
    fprintf (fdoutp1, "for iteration # %2d ----", n_actual_iteration);
    fprintf (fdoutp2, "for iteration # %2d ------------\n", n_actual_iteration);
	ssresid = 0.0;
	ym = 0.0;
	for (i=0; i < ndata; i++)
	{
	ym += y[i];
	epsilon[i] = y[i] - y2[i];
	ssresid += epsilon[i] * epsilon[i];
	p=epsilon[i]/y[i];
	}
	ym = ym/ndata;
	
	ssto = 0.0;
	for (i=0;i<ndata;i++)
	{
	 ssto += (y[i] - ym)*(y[i] - ym);
	}
	ff = ( ssto - ssresid ) / ssresid * ndata/nx;
	
	fprintf(fdoutp2, "F = %12.4f\n",ff);
	sigma1 = sqrt ( (double) ssresid/(ndata-(nx+1)));
	rr2 = (ssto -ssresid ) / ssto;
	rrr = 1. - (double) (( ndata - 1 ) /( ndata - 1 - nx )) * ssresid / ssto;
	fprintf(fdoutp2, "R ** 2 = %12.4f\n",rr2);

			/* calculate correlation coefficient between y and y2 */

    correlation (y,y2,ndata,&r);

					/* calculate t */
    tscore (r,ndata,&t);

					/* print coefficients */
    fprintf (fdoutp1, "r^2 = %g; t = %g\n", r*r, t);
		
		fprintf(fdoutp1, "a0 = %g \n", a[na-1]);
		for (i=0; i< (na - 1 ) / 2; i++)
		fprintf(fdoutp1, "a%d = %g \n", i+1, a[i]);
		fprintf(fdoutp1, "b0 = 1.0 \n");
		for (i= (na - 1 ) / 2; i< (na - 1); i++)
		fprintf(fdoutp1, "b%d = %g \n", i+1-(na-1)/2, a[i]);
    printf ("r = %g; r^2 = %g; t = %g\n", r, r*r, t);
    fprintf (fdoutp2, "r = %g; r^2 = %g; t = %g\n", r, r*r, t);
    fprintf (fdoutp2, "#, x[0], x[1],..., y(cal) diviation[y(ob)-y(cal)]\n");
        for (i=0; i<ndata; i++)
        {
	fprintf(fdoutp2,"%3d",i+1);
        for (j=0; j<nxhong; j++)
        fprintf(fdoutp2, "%10.4f", xhong[j]);
        fprintf(fdoutp2,"%10.4f %10.4f\n",y2[i], epsilon[i]);
        }

					/* iteration for nonlinear fitting */
 if( n_actual_iteration < n_max_iteration )
  {
	for (i=0; i<ndata; i++)
	{
	  delteps[i]=oldeps[i]-epsilon[i];
	  oldeps[i]=epsilon[i];
	  epsilon[i]= delteps[i] + epsilon[i];
	}
  n_actual_iteration = n_actual_iteration + 1;
  goto iteration;
   }

    exit(0);
}
