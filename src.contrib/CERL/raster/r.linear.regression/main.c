/* main.c */ 
/*
	r.linear.regression
	the program has the following functions:
	conduct usual linear regression: 
				y=a[0]x[0]+a[1],
                            	y=a[0]x[0]+a[1]x[1]+a[2]
                           	y=a[0]x[0]+a[1]x[1]+a[2]x[2]+a[4]
*/

#include <stdio.h>
#include <string.h>
#include <math.h>
#include <signal.h>
#include "gis.h"


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
    double rr2;		/* r-squire, coefficients of determinant*/
    double rrr;		/* adjusted r-squire */
    double ff;			/* F test number */
    double eps1;		/* eps1 is (a0*x0+a1*x1+a2*x2+a6) */
    double eps2;   		/* eps2 is (a3*x3+a4*x4+a5*x5+1) */ 
    double p;   		/* p is temperally parameter */
    double r;	/* correlation between observed and calculated y */
    int i,j,k,ka,ma;
    int nx,nxhong,model;		/* number of variables */
    int na,n1,n2,n3,ndata1,ndata2;	/* number of parameters */
    int ndata;
    char buf[512];
    char *inp, *outp, *title;

struct
{
        struct Option *input, *output;
} parm;

     struct GModule *module;

        G_gisinit (argv[0]);
        
        
        /* Set description */
        module              = G_define_module();
        module->description = ""\
        "Linear regression calculation from data stored in ASCII file";

	/* define the different options */
        parm.input = G_define_option() ;
        parm.input->key        = "input";
        parm.input->type       = TYPE_STRING;
        parm.input->required   = YES;
        parm.input->description= "ASCII file name of imagery to be regressed";

        parm.output = G_define_option() ;
        parm.output->key        = "output";
        parm.output->type       = TYPE_STRING;
        parm.output->required   = YES;
        parm.output->description= "ASCII file name to hold regression result";

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

nx=nxhong;
x=vector(nx*ndata);
x=xhong;

	na = nx + 1;
	a = vector (na);
	u = matrix (ndata, na);
	v = matrix (na, na);
	w = vector (na);
	value = vector (na);
	y2 = vector (ndata);

    svdfitn(x,nx,y,ndata,a,na,u,v,w,function);

						/**** test matrix v
						*/
for(i=0;i<na;i++)
{
  for(j=0;j<na;j++)
  fprintf(fdoutp, "for check of v[%2d][%2d] = %12.4f\n",i,j,v[i][j]);
}

for(i=0;i<na;i++)
  for(j=0;j<na;j++)
  {
    test=0.0;
    for(k=0;k<na;k++)
    test += v[k][i]*v[k][j];
    fprintf(fdoutp, "for i=%2d and j=%2d check summation of Vki*Vkj =%12.4f\n",
            i,j,test);
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
  fprintf (stdout,"w[%2d] = %12.4f\n",ka,w[ka]);
  fprintf(fdoutp, "w[%2d] = %12.4f\n",ka,w[ka]);
  fprintf (stdout,"sigma[%1d] **2 =%12.4f\n", ka,sigma[ka]);
  fprintf(fdoutp, "sigma[%1d] **2 =%12.4f\n", ka,sigma[ka]);
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
	fprintf(fdoutp, " epsilon/y for data i = %2d is = %12.4f\n", i,p);
	}
	ym = ym/ndata;
	fprintf (stdout," \nSSresid = %12.4f\n", ssresid);
	fprintf (stdout," standard residiance = %16.10f\n", sqrt(ssresid/ndata));
	fprintf (stdout," mean y = %12.4f\n", ym);
	fprintf(fdoutp, " SSresid = %12.4f\n", ssresid);
	fprintf(fdoutp, " standard residiance = %16.10f\n", sqrt(ssresid/ndata));
	fprintf(fdoutp, " mean y = %12.4f\n", ym);
	
	ssto = 0.0;
	for (i=0;i<ndata;i++)
	{
	 ssto += (y[i] - ym)*(y[i] - ym);
	}
	fprintf (stdout," SSto = %12.4f\n", ssto);
	fprintf(fdoutp, " SSto = %12.4f\n", ssto);
	fprintf (stdout," standard variance of y = %12.4f\n", sqrt(ssto/ndata));
	fprintf(fdoutp, " standard variance of y = %12.4f\n", sqrt(ssto/ndata));
	ff = ( ssto - ssresid ) / ssresid * ndata/nx;
	fprintf (stdout,"F = %12.4f\n",ff);
	fprintf(fdoutp, "F = %12.4f\n",ff);
	sigma1 = sqrt ( (double) ssresid/(ndata-(nx+1)));
	fprintf (stdout,"standard deviation = %12.4f\n",sigma1);
	fprintf(fdoutp, "standard deviation = %12.4f\n",sigma1);
	rr2 = (ssto -ssresid ) / ssto;
	rrr = 1. - (double) (( ndata - 1 ) /( ndata - 1 - nx )) * ssresid / ssto;
	fprintf (stdout,"R ** 2 = %12.4f\n",rr2);
	fprintf(fdoutp, "R ** 2 = %12.4f\n",rr2);
	fprintf (stdout,"adjusted R ** 2 = %12.4f\n",rrr);
	fprintf(fdoutp, "adjusted R ** 2 = %12.4f\n",rrr);

			/* calculate correlation coefficient between y and y2 */

    correlation (y,y2,ndata,&r);

					/* calculate t */
    tscore (r,ndata,&t);

					/* print coefficients */
    fprintf (stdout,"------------------------------------------\n\n");
    fprintf (fdoutp, "------------------------------------------\n\n");
    show_parms(fdoutp, a,na);

    for (i = 0; i < na; i++)
	if (w[i] == 0.0)
	    fprintf (stdout,"   a[%d] has w == 0\n", i);
	    fprintf (fdoutp, "   a[%d] has w == 0\n", i);

    fprintf (stdout,"r = %g; r^2 = %g; t = %g\n", r, r*r, t);
    fprintf (fdoutp, "r = %g; r^2 = %g; t = %g\n", r, r*r, t);
    exit(0);
}

