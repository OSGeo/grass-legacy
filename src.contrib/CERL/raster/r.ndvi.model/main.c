#include <stdio.h>
#include <string.h>
#include <math.h>
#include <signal.h>
#include "gis.h"
					/* main.c */ 
					/*
	this is a program "r.ndvi.model" with the following functions:
	generate Normalized Differential Vegetation Index (NDVI) model:
		y = a[0]*(anvi) + a[1]
	In the output file:
		line #1 ------ a[0]
		line #2 ------ a[1]
		line #3 ------ a[0]= ., a[1]= .
		line #4 ------ r = ., r^2 = ., t = .

					*/


double *vector();
double **matrix();
double rms(), log(), sqrt(), err2();
void function();

main(argc,argv) 
int   argc;
char *argv[];
{
	FILE *fdinp, *fdoutp, *popen();

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
    char *inp, *outp, *title;

struct
{
        struct Option *input, *output;
} parm;


        G_gisinit (argv[0]);

						/* define the different options
						*/
        parm.input = G_define_option() ;
        parm.input->key        = "input";
        parm.input->type       = TYPE_STRING;
        parm.input->required   = YES;
        parm.input->description= "File name of imagery to be regressed";

        parm.output = G_define_option() ;
        parm.output->key        = "output";
        parm.output->type       = TYPE_STRING;
        parm.output->required   = YES;
        parm.output->description= "File name to hold regression result";

if (G_parser(argc, argv))
                exit(-1);

        inp = parm.input->answer;
        outp = parm.output->answer;

    fdoutp=fopen (outp, "w");
    fdinp = fopen (inp, "r");
    if (fdinp == NULL)
    	{
                sprintf (buf, "%s - not found\n", inp);
                G_fatal_error (buf);
                exit(1);
        }

if (G_legal_filename (outp) < 0)
        {
                sprintf (buf, "%s - illegal file name\n", outp);
                G_fatal_error (buf);
                exit(1);
        }

					/* read original data */
    	read_data (fdinp, fdoutp, &xhong, &nxhong, &y, &ndata);

	residual = vector (ndata);
	epsilon = vector (ndata);
	oldeps = vector (ndata);
	delteps = vector (ndata);

ndvi(xhong,&nxhong,&ndata,&x,&nx);
 
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
  printf("w[%2d] = %12.4f\n",ka,w[ka]);
  printf("sigma[%1d] **2 =%12.4f\n", ka,sigma[ka]);
}

					/* compute calculated values */
    for (i=0; i < ndata; i++)
    {
	function (x+i*nx,nx,value,na);
	y2[i] = 0.0;
	for (k = 0; k < na; k++)
	    y2[i] += a[k] * value[k];
    }

					/** Check utility: 
	   				check total sum of squares SSto,
	   				residual sum of squares SSresid,
	   				mean value of y: ym,
	   				regression sum of squares
					*/
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
	printf(" \nSSresid = %12.4f\n", ssresid);
	printf(" standard residiance = %16.10f\n", sqrt(ssresid/ndata));
	printf(" mean y = %12.4f\n", ym);
	
	ssto = 0.0;
	for (i=0;i<ndata;i++)
	{
	 ssto += (y[i] - ym)*(y[i] - ym);
	}
	printf(" SSto = %12.4f\n", ssto);
	printf(" standard variance of y = %12.4f\n", sqrt(ssto/ndata));
	ff = ( ssto - ssresid ) / ssresid * ndata/nx;
	printf("F = %12.4f\n",ff);
	sigma1 = sqrt ( (double) ssresid/(ndata-(nx+1)));
	printf("standard deviation = %12.4f\n",sigma1);
	rr2 = (ssto -ssresid ) / ssto;
	rrr = 1. - (double) (( ndata - 1 ) /( ndata - 1 - nx )) * ssresid / ssto;
	printf("R ** 2 = %12.4f\n",rr2);
	printf("adjusted R ** 2 = %12.4f\n",rrr);

			/* calculate correlation coefficient between y and y2 */

    correlation (y,y2,ndata,&r);

					/* calculate t */
    tscore (r,ndata,&t);

					/* print coefficients */
    printf ("------------------------------------------\n\n");
    show_parms(fdoutp, a,na);

    for (i = 0; i < na; i++)
	if (w[i] == 0.0)
	    printf ("   a[%d] has w == 0\n", i);

    printf ("r = %g; r^2 = %g; t = %g\n", r, r*r, t);
    fprintf (fdoutp, "r = %g; r^2 = %g; t = %g\n", r, r*r, t);

    exit(0);
}

