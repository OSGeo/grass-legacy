#include <stdio.h>
#include <string.h>
#include <math.h>
#include <signal.h>
#include "gis.h"
					/* main.c */ 
					/*
	the program has the following functions:

	a) Select models:
		1) usual linear regression: 
				y=a[0]x[0]+a[1],
                            	y=a[0]x[0]+a[1]x[1]+a[2]
                           	y=a[0]x[0]+a[1]x[1]+a[2]x[2]+a[4]
		2) normalized by x1:    
				y=a[0]x[1]/x[0] + a[1]
       			        y=a[0]x[1]/x[0] + a[1]x[2]/x[0] + a[2]
		3) normalized by x2:    
				y=a[0]x[0]/x[1] + a[1]
               		        y=a[0]x[0]/x[1] + a[1]x[2]/x[1] + a[2]
		4) NDVI model
		5) intensity NDVI model
		6) reflectance NDVI model
		7) HRVI (Half Relaxation Vegetation Index) model
		8) RVI (Relaxation Vegetation Index) model
	b) Check multi-collinearity
	c) Predict other part of the same data set
	d) Predict other data set
	e) Check confidencial of a
	f) Test model adequacy
	g) F check model utility
	h) Give correlation between observed and calculated y
	i) Give data for scatter plot
	j) Give data for residual plot
	k) Give data for soil isoline plot
					*/


double *vector();
double **matrix();
double rms(), log(), sqrt(), err2();
void function();

int 
main (int argc, char *argv[])
{
	FILE *curve_radiance_coverage_x1y;
	FILE *curve_radiance_coverage_x2y;
	FILE *curve_radiance_coverage_x3y;
	FILE *curve_veget_soil_x1x2;
	FILE *curve_veget_soil_x1x3;
	FILE *curve_veget_soil_x2x3;
	FILE *curve_residual_x1;
	FILE *curve_residual_x2;
	FILE *curve_residual_x3;
	FILE *curve_residual_y;
	FILE *y_y;
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
        struct Option *input, *output, *check, *predict, *calculat, 
			*plot;
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

        parm.check = G_define_option() ;
        parm.check->key        = "goodness";
        parm.check->type       = TYPE_STRING;
        parm.check->required   = NO;
        parm.check->description= "Goodness of fit option" ;
	parm.check->options    = "none,multx1,multx2,multx3";
	parm.check->answer="none";

        parm.predict = G_define_option() ;
        parm.predict->key        = "predict";
        parm.predict->type       = TYPE_STRING;
        parm.predict->required   = NO;
        parm.predict->description= "Temporal prediction" ;
	parm.predict->options    = "none,same,other";
	parm.predict->answer="none";

        parm.calculat = G_define_option() ;
        parm.calculat->key        = "rvi";
        parm.calculat->type       = TYPE_STRING;
        parm.calculat->required   = NO;
        parm.calculat->description= "Method of calculation for rvi" ;
        parm.calculat->options    = "rational,linear,nonlinear,all";
	parm.calculat->answer="rational";

        parm.plot = G_define_option() ;
        parm.plot->key        = "plot";
        parm.plot->type       = TYPE_STRING;
        parm.plot->required   = NO;
        parm.plot->description= "Kind of plot needed" ;
	parm.plot->options    = "none,radiance-coverage,vegation-soil,adequacy,all";
	parm.plot->answer="none";

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

	flag_prediction = 0;
	flag_nonlinear = 0; 

	residual = vector (ndata);
	epsilon = vector (ndata);
	oldeps = vector (ndata);
	delteps = vector (ndata);

					/* set model number */
while (1)
{
fprintf (stdout,"\nInput model number prefered:\n");
fprintf (stdout,"        1) linear regression: y=a[0]x[0]+a[1]\n");
fprintf (stdout,"                              y=a[0]x[0]+a[1]x[1]+a[2]\n");
fprintf (stdout,"                              y=a[0]x[0]+a[1]x[1]+a[2]x[2]+a[4]\n");
fprintf (stdout,"        2) normalized by x1: y=a[0]x[1]/x[0] + a[1]\n");
fprintf (stdout,"                             y=a[0]x[1]/x[0] + a[1]x[2]/x[0] + a[2]\n");
fprintf (stdout,"        3) normalized by x2: y=a[0]x[0]/x[1] + a[1]\n");
fprintf (stdout,"                             y=a[0]x[0]/x[1] + a[1]x[2]/x[1] + a[2]\n");
fprintf (stdout,"        4) NDVI \n");
fprintf (stdout,"        5) intensity NDVI \n");
fprintf (stdout,"        6) reflectance NDVI \n");
fprintf (stdout,"        7) HRVI (Half Relaxation Vegetation Index) \n");
fprintf (stdout,"        8) RVI (Relaxation Vegetation Index) \n");
fprintf (stdout,"> ");
if (!G_gets(buf) || sscanf(buf,"%d",&model) != 1 || model < 1 || model > 8) 
continue;
break;
}
fprintf (stdout,"\nSelected model is %d\n", model);
fprintf(fdoutp, "selected model is %d\n", model);


					/* usual linear regression */
if (model == 1)
{
nx=nxhong;
x=vector(nx*ndata);
x=xhong;
goto regression;
}

					/* normalization with respect to x1 */
if (model == 2)
{
normalization_x1(xhong,&nxhong,&ndata,&x,&nx);
goto regression;
}

					/* normalization with respect to x2 */
if (model == 3 )
{
normalization_x2(xhong,&nxhong,&ndata,&x,&nx);
goto regression;
}

					/*  use NDVI to conduct regression 
					*/
if (model == 4) 
{
ndvi(xhong,&nxhong,&ndata,&x,&nx);
goto regression;
}
 
					/* use NDVI of intensity */
if(model == 5)
{
ndvi_intensity(xhong,&nxhong,&ndata,&x,&nx);
goto regression;
}
					/* use NDVI of reflectance */
if(model == 6)
{
ndvi_reflectance(xhong,&nxhong,&ndata,&x,&nx);
goto regression;
}

					/* half relaxation VI */
if(model == 7)
{
rvi(xhong,&nxhong,&ndata,&x,&nx);
goto regression;
}

					/* plot radiance_coverage curves
					*/
if(strcmp(parm.plot->answer, "radiance-coverage") == 0 ||
	strcmp(parm.plot->answer, "all") == 0 )
{
if(nx==1)
{
	curve_radiance_coverage_x1y = fopen ("curve.radiance_coverage_x1y","w");
	for (ma=0; ma < ndata; ma++)
	{
	fprintf(curve_radiance_coverage_x1y,"%12.4f %12.4f\n",
		y[ma],x[ma*nx]);
	}
}
if(nx==2)
{
	curve_radiance_coverage_x1y = fopen ("curve.radiance_coverage_x1y","w");
	curve_radiance_coverage_x2y = fopen ("curve.radiance_coverage_x2y","w");
	for (ma=0; ma < ndata; ma++)
	{
	fprintf(curve_radiance_coverage_x1y,"%12.4f %12.4f\n",
		y[ma],x[ma*nx]);
	fprintf(curve_radiance_coverage_x2y,"%12.4f %12.4f\n",
		y[ma],x[ma*nx+1]);
	}
}
if(nx==3)
{
	curve_radiance_coverage_x1y = fopen ("curve.radiance_coverage_x1y","w");
	curve_radiance_coverage_x2y = fopen ("curve.radiance_coverage_x2y","w");
	curve_radiance_coverage_x3y = fopen ("curve.radiance_coverage_x3y","w");
	for (ma=0; ma < ndata; ma++)
	{
	fprintf(curve_radiance_coverage_x1y,"%12.4f %12.4f\n",
		y[ma],x[ma*nx]);
	fprintf(curve_radiance_coverage_x2y,"%12.4f %12.4f\n",
		y[ma],x[ma*nx+1]);
	fprintf(curve_radiance_coverage_x3y,"%12.4f %12.4f\n",
		y[ma],x[ma*nx+2]);
	}
}
}

					/*   plot vegetation-soil curves  
					*/
if(strcmp(parm.plot->answer, "vegetation-soil") == 0 ||
	strcmp(parm.plot->answer, "all") == 0)
{
if (nx == 2 )
{
	curve_veget_soil_x1x2 = fopen ( "curve.veget_soil_x1x2", "w");
   for ( ma=0; ma< ndata; ma++)
	{ 
	n1=ma*nx;
	n2=ma*nx+1;
	fprintf(curve_veget_soil_x1x2, "%12.4f%12.4f\n",
	       x[n1],x[n2]);
	}
}
if (nx == 3 )
{
	curve_veget_soil_x1x2 = fopen ( "curve.veget_soil_x1x2", "w");
	curve_veget_soil_x1x3 = fopen ( "curve.veget_soil_x1x3", "w");
	curve_veget_soil_x2x3 = fopen ( "curve.veget_soil_x2x3", "w");
   for ( ma=0; ma< ndata; ma++)
	{ 
	n1=ma*nx;
	n2=ma*nx+1; 
	n3=ma*nx+2; 
	fprintf(curve_veget_soil_x1x2, "%12.4f%12.4f\n",
	       x[n1],x[n2]);
	fprintf(curve_veget_soil_x1x3, "%12.4f%12.4f\n",
	       x[n1],x[n3]);
	fprintf(curve_veget_soil_x2x3, "%12.4f%12.4f\n",
	       x[n2],x[n3]);
	}
}
}
					/* rational method of rvi model 
					fitting */
if (model == 8)
{
flag_nonlinear = 1;
	nx = 2*nxhong;
	x = vector (nx*ndata);
	for(i=0; i < ndata; i++)
	   {
	   *(epsilon + i) = 0.0;
	   *(oldeps + i) = 0.0;
	   }
iteration:
    	nonlinear_data(xhong,&nxhong,y,epsilon,&ndata,&x,&nx);
}

regression:

    na = nx+1;
    a = vector (na);    		/* parameter array */
    u = matrix (ndata,na);
    v = matrix (na,na);
    w = vector (na);
    value = vector (na);
    y2 = vector (ndata);

					/* check multi_collinearity 
					*/
if(flag_nonlinear == 1)
goto delay_multi;

nxmul = nx -1;
xmul = vector(nxmul*ndata);
ymul = vector (ndata);
    if (strcmp(parm.check->answer, "multx1") == 0) 
{
	fprintf (stdout,"check multicollinearity for x1=y\n");
	fprintf(fdoutp, "check multicollinearity for x1=y\n");
	for ( i=0; i<ndata; i++)
	{
		for(j=1; j<(nx); j++)
		xmul[i*(nx-1)+(j-1)] = x[i*nx+j];
	ymul[i]=x[i*nx];
	}

	nx=nxmul;
	for(k=0; k< ndata; k++)
	{
		for(i=0; i<nx; i++)
		{
		x[k*nx+i]=xmul[k*nx+i];
		}
	y[k]=ymul[k];
	}
goto multi;
}
    if (strcmp(parm.check->answer, "multx2") == 0) 
{
	fprintf (stdout,"check multicollinearity for x2=y\n");
	fprintf(fdoutp, "check multicollinearity for x2=y\n");
	for ( i=0; i<ndata; i++)
	{
		j=0;
		xmul[i*(nx-1)+j] = x[i*nx+j];
		j=2;
		xmul[i*(nx-1)+(j-1)] = x[i*nx+j];
	ymul[i]=x[i*nx+1];
	}

	nx=nxmul;
	for(k=0; k< ndata; k++)
	{
		for(i=0; i<nx; i++)
		{
		x[k*nx+i]=xmul[k*nx+i];
		}
	y[k]=ymul[k];
	}
goto multi;
}
    if (strcmp(parm.check->answer, "multx3") == 0 )
{
	fprintf (stdout,"check multicollinearity for x3=y\n");
	fprintf(fdoutp, "check multicollinearity for x3=y\n");
	for ( i=0; i<ndata; i++)
	{
		for(j=0; j<(nx-1); j++)
		xmul[i*(nx-1)+j] = x[i*nx+j];
	ymul[i]=x[i*nx+2];
	}

	nx=nxmul;
	for(k=0; k< ndata; k++)
	{
		for(i=0; i<nx; i++)
		{
		x[k*nx+i]=xmul[k*nx+i];
		}
	y[k]=ymul[k];

	}
}
multi:
na = nx + 1;

delay_multi:
					/* set prediction flag */
if(flag_prediction  == 0)
{
    if (strcmp(parm.predict->answer, "same") == 0 )
    { 
    while(1)
    {
	fprintf (stdout,"Enter number of sampling data. [1-%d]: ", ndata);
	if (!G_gets(buf) || (sscanf(buf, "%d", &ndata1) != 1) ||
	    ndata1 < 1 || ndata1 > ndata) 
	continue;
	break;
    }
    fprintf(fdoutp, "number of sampling data = %d\n", ndata1);
    flag_prediction = 1;
    ndata2=ndata-ndata1;
    ndata=ndata1;
    xprediction=vector(ndata2*nxhong);
    yprediction=vector(ndata2);
    for(i=0;i<ndata2;i++)
	{
		if(flag_nonlinear == 0)
		{
	for(k=0;k<nx;k++)
		xprediction[i*nx+k]=x[nx*ndata1+i*nx+k];
		}
		if(flag_nonlinear == 1)
		{
	for(k=0;k<nxhong;k++)
		xprediction[i*nxhong+k]=xhong[nxhong*ndata1+i*nxhong+k];
		}
	yprediction[i]=y[ndata1+i];
	}
    }
}

						/* linearly fit the model */

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
if(flag_nonlinear == 1)
{
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
}

if(flag_nonlinear == 0)
{
    for (i=0; i < ndata; i++)
    {
	function (x+i*nx,nx,value,na);
	y2[i] = 0.0;
	for (k = 0; k < na; k++)
	    y2[i] += a[k] * value[k];
    }
}

					/** Check utility: 
	   				check total sum of squares SSto,
	   				residual sum of squares SSresid,
	   				mean value of y: ym,
	   				regression sum of squares
					*/
	y_y = fopen ("y.y", "w");
	ssresid = 0.0;
	ym = 0.0;
	for (i=0; i < ndata; i++)
	{
	ym += y[i];
	epsilon[i] = y[i] - y2[i];
	ssresid += epsilon[i] * epsilon[i];
	p=epsilon[i]/y[i];
	fprintf(y_y,"observed y = %12.4f, calculated y = %12.4f\n",y[i],y2[i]);
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
					/*  test adequacy  */
    if(strcmp(parm.plot->answer, "adequacy") == 0 ||
       strcmp(parm.plot->answer, "all") == 0 )
{
if(nx == 1 )
{
	curve_residual_x1 = fopen ("curve.residual_x1", "w");
	curve_residual_y = fopen ("curve.residual_y", "w");
	for(i=0;i<ndata;i++)
	{
	residual[i]=epsilon[i]/sigma1;
	fprintf(curve_residual_x1,"%12.4f %12.4f\n",x[i*nx+0],residual[i]);
	fprintf(curve_residual_y,"%12.4f %12.4f\n",y[i],residual[i]);
	}
}
if(nx == 2 )
{
	curve_residual_x1 = fopen ("curve.residual_x1", "w");
	curve_residual_x2 = fopen ("curve.residual_x2", "w");
	curve_residual_y = fopen ("curve.residual_y", "w");
	for(i=0;i<ndata;i++)
	{
	residual[i]=epsilon[i]/sigma1;
	fprintf(curve_residual_x1,"%12.4f %12.4f\n",x[i*nx+0],residual[i]);
	fprintf(curve_residual_x2,"%12.4f %12.4f\n",x[i*nx+1],residual[i]);
	fprintf(curve_residual_y,"%12.4f %12.4f\n",y[i],residual[i]);
	}
}
if(nx > 2 )
{
	curve_residual_x1 = fopen ("curve.residual_x1", "w");
	curve_residual_x2 = fopen ("curve.residual_x2", "w");
	curve_residual_x3 = fopen ("curve.residual_x3", "w");
	curve_residual_y = fopen ("curve.residual_y", "w");
	for(i=0;i<ndata;i++)
	{
	residual[i]=epsilon[i]/sigma1;
	fprintf(curve_residual_x1,"%12.4f %12.4f\n",x[i*nx+0],residual[i]);
	fprintf(curve_residual_x2,"%12.4f %12.4f\n",x[i*nx+1],residual[i]);
	fprintf(curve_residual_x3,"%12.4f %12.4f\n",x[i*nx+2],residual[i]);
	fprintf(curve_residual_y,"%12.4f %12.4f\n",y[i],residual[i]);
	}
}
}


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
/*
    if (argc > 2)
    {
	if(!freopen (argv[2], "w", stdout))
	    perror (argv[2]);
	else
	{
	    fprintf (stdout,"parameters\n");
	    fprintf (fdoutp, "parameters\n");
	    show_parms(fdoutp, a,na);

	    fprintf (stdout,"corr=%g t=%g\n", r, t);
	    fprintf (fdoutp, "corr=%g t=%g\n", r, t);
	    fprintf (stdout,"  observed  predicted   error\n");
	    fprintf (fdoutp, "  observed  predicted   error\n");
	    for (i = 0; i < ndata; i++)
		fprintf (stdout,"%12.4lf %12.4lf %12.4lf\n", y[i], y2[i], y[i]-y2[i]);
		fprintf (fdoutp, "%12.4lf %12.4lf %12.4lf\n", y[i], y2[i], y[i]-y2[i]);
	}
    }
    if (argc > 3)
    {
	if (!freopen (argv[3], "w", stdout))
	    perror (argv[3]);
	else
	    for (i = 0; i < ndata; i++)
		fprintf (stdout,"%12.4lf %12.4lf %12.4lf\n", y[i], y2[i], y[i]-y2[i]);
		fprintf (fdoutp, "%12.4lf %12.4lf %12.4lf\n", y[i], y2[i], y[i]-y2[i]);
    }
*/
					/* iteration for nonlinear fitting */
if(flag_nonlinear == 1)
{
 if( G_yes("\nNeed further iteration?",0) )
  {
   while(1)
   {
       fprintf (stdout,"Enter percentage of reserving old epsilon [0-100]: ");
       if (!G_gets(buf) || sscanf(buf, "%d", &iamp) != 1 || iamp < 0 
	   || iamp > 100) continue;
       break;
   }
   fprintf (fdoutp, 
     "\n\n\n more iteration, %d percentage of old epsilon reserved\n", iamp);
	for (i=0; i<ndata; i++)
	{
	  delteps[i]=oldeps[i]-epsilon[i];
	  oldeps[i]=epsilon[i];
	  epsilon[i]= (double)iamp / 100. * delteps[i] + epsilon[i];
	}
  goto iteration;
  }
					/* linearization method of
					calculation of rvi model
					using derivatives */
 if(strcmp(parm.calculat->answer, "linearized") == 0 ||
              	strcmp(parm.calculat->answer, "all") ==0 )
 {
  fprintf (fdoutp, "\n\n\n linearization method of calculating rvi\n"); 
  nonlinear_linearization(fdoutp, xhong,&nxhong,&ndata,y,a, &lina);
  relaxation(fdoutp, xhong,&nxhong,&ndata,y,lina);
 }

					/* nonlinear calculation of rvi*/
 if(strcmp(parm.calculat->answer, "nonlinear") == 0 || 
             	strcmp(parm.calculat->answer, "all") == 0 )
 {
  fprintf (fdoutp, "\n\n\n nonlinear method of calculating rvi\n"); 
  nonlinear_linearization(fdoutp, xhong,&nxhong,&ndata,y,a, &lina);
 } 
}

					/* predictions */
    if (flag_prediction == 1 )
{
    if (strcmp(parm.predict->answer, "same") == 0 )
{
if(flag_nonlinear == 0)
	prediction_linear(fdoutp, xprediction,yprediction,ndata2,nx,a,na);
if(flag_nonlinear == 1)
	prediction_nonlinear(fdoutp, xprediction,yprediction,ndata2,nxhong,a,na);
goto final;
}

another_data:
    if (strcmp(parm.predict->answer, "other") == 0 )
{
  while(1)
  {
    fprintf (stdout,"Input the name of the other data set: ");
    if (!G_gets(buf) || (sscanf(buf, "%s", other_filename) != 1)) continue;
    break;
}
fprintf(fdoutp, "\n\n\nThe other data set for prediction = %s\n", other_filename);
    fprintf (stdout,"using %s\n", other_filename);
if(flag_nonlinear == 0)
	prediction_linear_other(fdoutp, other_filename,model,a,na);
if(flag_nonlinear == 1)
	prediction_nonlinear_other(fdoutp, other_filename,a,na);
goto another_data;
}

goto final;
}

if(flag_nonlinear ==0)
goto final;

			/* delayed check multi_collinearity for nonlinear case*/
    if (strcmp(parm.check->answer, "multx1") == 0 ||
    strcmp(parm.check->answer, "multx2") == 0 ||
    strcmp(parm.check->answer, "multx3") == 0 )
goto checkmulti;
	goto final;
checkmulti:
nxmul = nx -1;
xmul = vector(nxmul*ndata);
ymul = vector (ndata);
    if (strcmp(parm.check->answer, "multx1") == 0 )
{
	fprintf (stdout,"check multicollinearity for x1=y\n");
	for ( i=0; i<ndata; i++)
	{
		for(j=1; j<(nx); j++)
		xmul[i*(nx-1)+(j-1)] = x[i*nx+j];
	ymul[i]=x[i*nx];
	}

	nx=nxmul;
	for(k=0; k< ndata; k++)
	{
		for(i=0; i<nx; i++)
		{
		x[k*nx+i]=xmul[k*nx+i];
		}
	y[k]=ymul[k];
	}
goto nonmult;
}
    if (strcmp(parm.check->answer, "multx2") == 0 )
{
	fprintf (stdout,"check multicollinearity for x2=y\n");
	for ( i=0; i<ndata; i++)
	{
		j=0;
		xmul[i*(nx-1)+j] = x[i*nx+j];
		j=2;
		xmul[i*(nx-1)+(j-1)] = x[i*nx+j];
	ymul[i]=x[i*nx+1];
	}

	nx=nxmul;
	for(k=0; k< ndata; k++)
	{
		for(i=0; i<nx; i++)
		{
		x[k*nx+i]=xmul[k*nx+i];
		}
	y[k]=ymul[k];
	}
goto nonmult;
}
    if (strcmp(parm.check->answer, "multx3") == 0 )
{
	fprintf (stdout,"check multicollinearity for x3=y\n");
	for ( i=0; i<ndata; i++)
	{
		for(j=0; j<(nx-1); j++)
		xmul[i*(nx-1)+j] = x[i*nx+j];
	ymul[i]=x[i*nx+2];
	}

	nx=nxmul;
	for(k=0; k< ndata; k++)
	{
		for(i=0; i<nx; i++)
		{
		x[k*nx+i]=xmul[k*nx+i];
		}
	y[k]=ymul[k];
	}
}
nonmult:
na = nx + 1;

/* fit the model */
    svdfitn(x,nx,y,ndata,a,na,u,v,w,function);

/**** test matrix v
for(i=0;i<na;i++)
{
  for(j=0;j<na;j++)
  fprintf (stdout,"v[%2d][%2d] = %12.4f\n",i,j,v[i][j]);
}

for(i=0;i<na;i++)
  for(j=0;j<na;j++)
  {
  test=0.0;
  for(k=0;k<na;k++)
  test += v[k][i]*v[k][j];
  fprintf (stdout,"for i=%2d and j=%2d test[i,j]=%12.4f\n",i,j,test);
  }
*/

					/* 
					check confidential
					get and write sigma */
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

					/* compute predicted values */
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
	fprintf (stdout," SSresid = %12.4f\n", ssresid);
	fprintf(fdoutp, " SSresid = %12.4f\n", ssresid);
	fprintf (stdout," standard residiance = %16.10f\n", sqrt(ssresid/ndata));
	fprintf(fdoutp," standard residiance = %16.10f\n", sqrt(ssresid/ndata));
	fprintf (stdout," mean y = %12.4f\n", ym);
	fprintf(fdoutp, " mean y = %12.4f\n", ym);
	
	ssto = 0.0;
	for (i=0;i<ndata;i++)
	{
	ssto += (y[i] - ym)*(y[i] - ym);
	}
	fprintf (stdout," SSto = %12.4f\n", ssto);
	fprintf(fdoutp, " SSto = %12.4f\n", ssto);
	fprintf (stdout," standard variance = %12.4f\n", sqrt(ssto/ndata));
	fprintf(fdoutp, " standard variance = %12.4f\n", sqrt(ssto/ndata));
	ff = ( ssto - ssresid ) / ssresid * ndata/nx;
	fprintf (stdout,"F=%12.4f\n",ff);
	fprintf(fdoutp, "F=%12.4f\n",ff);
	sigma1 = sqrt ( (double) ssresid/(ndata-(nx+1)));
	for(i=0;i<ndata;i++)
	{
	residual[i]=epsilon[i]/sigma1;
	}
	rr2 = (ssto -ssresid ) / ssto;
	rrr = 1. - (double) (( ndata - 1 ) /( ndata - 1 - nx )) * 
		ssresid / ssto;
	fprintf (stdout,"R ** 2 =%12.4f\n",rr2);
	fprintf(fdoutp, "R ** 2 =%12.4f\n",rr2);
	fprintf (stdout,"adjusted R ** 2 =%12.4f\n",rrr);
	fprintf(fdoutp, "adjusted R ** 2 =%12.4f\n",rrr);

					/* calculate correlation coefficient 
					between y and y2 */	
    correlation (y,y2,ndata,&r);
    tscore (r,ndata,&t);
    fprintf (stdout,"---------------\n");
    fprintf (fdoutp, "---------------\n");
    show_parms(fdoutp, a,na);

    for (i = 0; i < na; i++)
	if (w[i] == 0.0)
	    fprintf (stdout,"   a[%d] has w==0\n", i);
	    fprintf (fdoutp, "   a[%d] has w==0\n", i);

    fprintf (stdout," r=%g r^2=%g t=%g\n", r, r*r, t);
    fprintf (fdoutp, " r=%g r^2=%g t=%g\n", r, r*r, t);

    if (argc > 2)
    {
	if(!freopen (argv[2], "w", stdout))
	    perror (argv[2]);
	else
	{
	    fprintf (stdout,"parameters\n");
	    fprintf (fdoutp, "parameters\n");
	    show_parms(fdoutp, a,na);
	    fprintf (stdout,"corr=%g t=%g\n", r, t);
	    fprintf (fdoutp, "corr=%g t=%g\n", r, t);
	    fprintf (stdout,"observed  predicted   error\n");
	    fprintf (fdoutp, "observed  predicted   error\n");
	    for (i = 0; i < ndata; i++)
		fprintf (stdout,"%12.4lf %12.4lf %12.4lf\n", y[i], y2[i], y[i]-y2[i]);
		fprintf (fdoutp, "%12.4lf %12.4lf %12.4lf\n", y[i], y2[i], y[i]-y2[i]);
	}
    }
    if (argc > 3)
    {
	if (!freopen (argv[3], "w", stdout))
	    perror (argv[3]);
	else
	    for (i = 0; i < ndata; i++)
		fprintf (stdout,"%12.4lf %12.4lf %12.4lf\n", y[i], y2[i], y[i]-y2[i]);
		fprintf (fdoutp, "%12.4lf %12.4lf %12.4lf\n", y[i], y2[i], y[i]-y2[i]);
    }


final:
    exit(0);
}

