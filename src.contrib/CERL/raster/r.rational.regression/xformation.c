/*
 * $Id$
 */

#include <stdio.h>
#include <math.h>					
#include <signal.h>					
					/* for veriaty of formations of xi
					   normalization to x1 */	
normalization_x1(xhong,nxhong,ndata,x,nx)
        double *xhong,**x;
        int *nxhong,*nx,*ndata;
{
        double xtemp;
        int i,k;
	double *vector();
/*
fprintf (stdout,"in normalizationx1: ndata=%d,nxhong=%d\n",*ndata,*nxhong);
for(i=0;i<(*ndata);i++)
{
  for(k=0;k<*nxhong;k++)
  {
  fprintf (stdout,"in normalizationx1: i=%d, k=%d\n",i,k);
  fprintf (stdout,"in normalizationx1: xhong[%d]=%10.4f\n",i*(*nxhong)+k,
			*(xhong+i*(*nxhong)+k));
  }
}
*/

if(*nxhong == 2)
        {
        *nx=1;
        *x = vector(*nx*(*ndata));
        for(i=0;i<(*ndata);i++)
		{ xtemp=*((xhong)+i*(*nxhong));
		*((*x)+i*(*nx))=*((xhong)+i*(*nxhong)+1)/xtemp;
                }
        }
if(*nxhong == 3)
        {
        *nx=2;
        *x = vector(*nx*(*ndata));
        for (i=0;i<*ndata;i++)
		{ xtemp=*((xhong)+i*(*nxhong));
		*((*x)+i*(*nx))=*((xhong)+i*(*nxhong)+1)/xtemp;
		*((*x)+i*(*nx)+1)=*((xhong)+i*(*nxhong)+2)/xtemp;
/*
		fprintf (stdout,"x[%d]=%10.4f, x[%d]=%10.4f\n",i*(*nx),*(*x+i*(*nx)),
 		i*(*nx)+1,*(*x+i*(*nx)+1));
*/
                }
        }
}


					/* normalization to x2 */
normalization_x2(xhong,nxhong,ndata,x,nx)
        double *xhong,**x;
        int *nxhong,*nx,*ndata;
{
        double xtemp;
        int i,k;
        double *vector();
/*
fprintf (stdout,"in normalizationx1: ndata=%d,nxhong=%d\n",*ndata,*nxhong);
for(i=0;i<(*ndata);i++)
{
  for(k=0;k<*nxhong;k++)
  {
  fprintf (stdout,"in normalizationx1: i=%d, k=%d\n",i,k);
  fprintf (stdout,"in normalizationx1: xhong[%d]=%10.4f\n",i*(*nxhong)+k,
			*(xhong+i*(*nxhong)+k));
  }
}
*/

if(*nxhong == 2)
{
        *nx=1;
        *x = vector(*nx*(*ndata));
        for(i=0;i<(*ndata);i++)
	{ 
        xtemp=*((xhong)+i*(*nxhong)+1);
	*((*x)+i*(*nx))=*((xhong)+i*(*nxhong))/xtemp;
/*
        fprintf (stdout,"x[%d]=%10.4f\n",i,*(*x+i*(*nx)));
*/
        }
}
if(*nxhong == 3)
{
        *nx=2;
        *x = vector(*nx*(*ndata));
        for (i=0;i<*ndata;i++)
	{ 
        xtemp=*((xhong)+i*(*nxhong)+1);
	*((*x)+i*(*nx))=*((xhong)+i*(*nxhong))/xtemp;
	*((*x)+i*(*nx)+1)=*((xhong)+i*(*nxhong)+2)/xtemp;
/*
fprintf (stdout,"x[%d]=%10.4f, x[%d]=%10.4f\n",i*(*nx),*(*x+i*(*nx)),
 i*(*nx)+1,*(*x+i*(*nx)+1));
*/
        }
}
}

					/* NDVI form */
ndvi(xhong,nxhong,ndata,x,nx)
        double *xhong,**x;
        int *nxhong,*nx,*ndata;
{
        double xtemp1,xtemp2;
        int i,k;
        double *vector();
					/* check input existance 
fprintf (stdout,"in ndvi: ndata=%d,nxhong=%d\n",*ndata,*nxhong);
for(i=0;i<(*ndata);i++)
{
  for(k=0;k<*nxhong;k++)
  {
  fprintf (stdout,"in ndvi: i=%d, k=%d\n",i,k);
  fprintf (stdout,"in ndvi: xhong[%d]=%10.4f\n",i*(*nxhong)+k,
			*(xhong+i*(*nxhong)+k));
  }
}
					*/

*nx=1;
*x = vector(*nx*(*ndata));
if(*nxhong == 2)
        {
	for(i=0;i<*ndata;i++)
		{
		xtemp1= *((xhong)+i*(*nxhong)+1);
		xtemp2= *((xhong)+i*(*nxhong));
		*((*x)+i)=(xtemp1-xtemp2)
			/(xtemp1+ xtemp2);
/*
                fprintf (stdout,"x[%d]=%10.4f\n",i,*(*x+i));
*/
                }
        }
if(*nxhong == 3)
        {
        for (i=0;i<*ndata;i++)
		{
		xtemp1= *((xhong)+i*(*nxhong)+2);
		xtemp2= *((xhong)+i*(*nxhong))
			+(*((xhong)+i*(*nxhong)+1));
		*((*x)+i)=(xtemp1-xtemp2)
			/(xtemp1+ xtemp2);
/*
                fprintf (stdout,"x[%d]=%10.4f\n",i,*(*x+i));
*/
                }
        }
}

                                        /* intensity NDVI form */
ndvi_intensity(xhong,nxhong,ndata,x,nx)
        double *xhong,**x;
        int *nxhong,*nx,*ndata;
{
        double xtemp1,xtemp2;
        int i,k;
        double *vector();
                                        /* check input existance 
fprintf (stdout,"in ndvi: ndata=%d,nxhong=%d\n",*ndata,*nxhong);
for(i=0;i<(*ndata);i++)
{
  for(k=0;k<*nxhong;k++)
  {
  fprintf (stdout,"in ndvi: i=%d, k=%d\n",i,k);
  fprintf (stdout,"in ndvi: xhong[%d]=%10.4f\n",i*(*nxhong)+k,
                        *(xhong+i*(*nxhong)+k));
  }
}
					*/

*nx=1;
*x = vector(*nx*(*ndata));
if(*nxhong == 2)
        {
        for(i=0;i<*ndata;i++)
                {
                xtemp1= *((xhong)+i*(*nxhong)+1);
		xtemp1 = xtemp1/0.1;
                xtemp2= *((xhong)+i*(*nxhong));
		xtemp2=xtemp2/0.07;
                *((*x)+i)=(xtemp1-xtemp2)
                        /(xtemp1+ xtemp2);
/*
                fprintf (stdout,"x[%d]=%10.4f\n",i,*(*x+i));
*/
                }
        }
if(*nxhong == 3)
        {
        for (i=0;i<*ndata;i++)
                {
                xtemp1= *((xhong)+i*(*nxhong)+2);
		xtemp1 = xtemp1/0.1;
                xtemp2= *((xhong)+i*(*nxhong))
                        +(*((xhong)+i*(*nxhong)+1));
		xtemp2=xtemp2/0.16;
                *((*x)+i)=(xtemp1-xtemp2)
                        /(xtemp1+ xtemp2);
/*
                fprintf (stdout,"x[%d]=%10.4f\n",i,*(*x+i));
*/
                }
        }
}
                                        /* reflectance NDVI form */
ndvi_reflectance(xhong,nxhong,ndata,x,nx)
        double *xhong,**x;
        int *nxhong,*nx,*ndata;
{
        double xtemp1,xtemp2;
        int i,k;
        double *vector();
                                        /* check input existance 
fprintf (stdout,"in ndvi: ndata=%d,nxhong=%d\n",*ndata,*nxhong);
for(i=0;i<(*ndata);i++)
{
  for(k=0;k<*nxhong;k++)
  {
  fprintf (stdout,"in ndvi: i=%d, k=%d\n",i,k);
  fprintf (stdout,"in ndvi: xhong[%d]=%10.4f\n",i*(*nxhong)+k,
                        *(xhong+i*(*nxhong)+k));
  }
}
					*/

*nx=1;
*x = vector(*nx*(*ndata));
if(*nxhong == 2)
        {
        for(i=0;i<*ndata;i++)
                {
                xtemp1= *((xhong)+i*(*nxhong)+1);
		xtemp1=xtemp1/331.0;
                xtemp2= *((xhong)+i*(*nxhong));
		xtemp2=xtemp2/502.0;
                *((*x)+i)=(xtemp1-xtemp2)
                        /(xtemp1+ xtemp2);
/*
                fprintf (stdout,"x[%d]=%10.4f\n",i,*(*x+i));
*/
                }
        }
if(*nxhong == 3)
        {
        for (i=0;i<*ndata;i++)
                {
                xtemp1= *((xhong)+i*(*nxhong)+2);
		xtemp1 /= 331.0;
                xtemp2= *((xhong)+i*(*nxhong))
                        +(*((xhong)+i*(*nxhong)+1));
		xtemp2 /= 1089.0;
                *((*x)+i)=(xtemp1-xtemp2)
                        /(xtemp1+ xtemp2);
/*
                fprintf (stdout,"x[%d]=%10.4f\n",i,*(*x+i));
*/
                }
        }
}


					/* relaxation vegetation index form */
rvi(xhong,nxhong,ndata,x,nx)
        double *xhong,**x;
        int *nxhong,*nx,*ndata;
{
        double xtemp1,xtemp2;
        int i,k;
        double *vector();
					/* check input existance 
fprintf (stdout,"in rvi: ndata=%d,nxhong=%d\n",*ndata,*nxhong);
for(i=0;i<(*ndata);i++)
{
  for(k=0;k<*nxhong;k++)
  {
  fprintf (stdout,"in rvi: i=%d, k=%d\n",i,k);
  fprintf (stdout,"in rvi: xhong[%d]=%10.4f\n",i*(*nxhong)+k,
			*(xhong+i*(*nxhong)+k));
  }
}
					*/

*nx=2;
*x = vector(*nx*(*ndata));
if(*nxhong == 2)
        {
	for(i=0;i<*ndata;i++)
		{
		xtemp1= *((xhong)+i*(*nxhong)+1);
		xtemp2= *((xhong)+i*(*nxhong));
		*((*x)+i*2)=xtemp2
			/(xtemp1+ xtemp2);
		*((*x)+i*2+1)=xtemp1
			/(xtemp1+ xtemp2);
/*
                fprintf (stdout,"x[%d]=%10.4f,x[%d]=%10.4f\n",
		i*2,*(*x+i*2),i*2+1,*(*x+i*2+1));
*/
                }
        }
if(*nxhong == 3)
        {
        for (i=0;i<*ndata;i++)
		{
		xtemp1= *((xhong)+i*(*nxhong)+2);
		xtemp2= *((xhong)+i*(*nxhong))
			+(*((xhong)+i*(*nxhong)+1));
		*((*x)+i*2)=xtemp2
			/(xtemp1+ xtemp2);
		*((*x)+i*2+1)=xtemp1
			/(xtemp1+ xtemp2);
/*
                fprintf (stdout,"x[%d]=%10.4f,x[%d]=%10.4f\n",
		i*2,*(*x+i*2),i*2+1,*(*x+i*2+1));
*/
                }
        }
}

					/* linearization method for non_linear
					   problem */
nonlinear_linearization(fdoutp, xhong,nxhong,ndata,y,a, lina)
	FILE *fdoutp;
	double *xhong,*y,*a,**lina;
	int  *nxhong,*ndata;
{
        FILE *curve_residual_x1;
        FILE *curve_residual_x2;
        FILE *curve_residual_x3;
        FILE *curve_residual_y;
        FILE *y_y;

	double p,q,q2,*newa,*linx,*liny,*sigma,*ta,*da;
	double ssresid,ym,*epsilon,ff,sigma1,*y2,ssto,rr2,rrr,*residuals;
	double *x,test,eps1,eps2,standard_residual,standard_variance;
	double standard_residual_old,r,t;
	int i,nx,na,ka,ma,ndatalin,j,k;
	double datan;
	double **u,**v,*w;
	void function();
	double **matrix();
	double *vector();

        curve_residual_x1 = fopen ("non.residual_x1", "w");
        curve_residual_x2 = fopen ("non.residual_x2", "w");
        curve_residual_x3 = fopen ("non.residual_x3", "w");
        curve_residual_y = fopen ("non.residual_y", "w");
        y_y = fopen ("non.y_y", "w");

	nx=(*nxhong)*2;
	na=nx+1;
	ndatalin=*ndata;
	linx=vector(nx*ndatalin);
	liny=vector(ndatalin);
	*lina=vector(na);
	ta=vector(na);
	da=vector(na);
	newa=vector(na);
	u=matrix(ndatalin,na);
	v=matrix(na,na);
	w=vector(na);
	y2=vector(ndatalin);
	epsilon=vector(ndatalin);
	residuals=vector(ndatalin);

	fprintf (stdout,"input a:%6.2f, %6.2f, %6.2f, %6.2f, %6.2f, %6.2f, %6.2f\n",
		*(a+0),*(a+1),*(a+2),*(a+3),*(a+4),*(a+5),*(a+6));
	fprintf(fdoutp, "input a:%6.2f,%6.2f,%6.2f,%6.2f,%6.2f,%6.2f,%6.2f\n", 
	       *(a+0),*(a+1),*(a+2),*(a+3),*(a+4),*(a+5),*(a+6));
	
	for(i=0; i<na; i++)
	  *(ta+i)=*(a+i);
	
	standard_residual_old = 0.0;

iteration:
	for(i=0;i<(*ndata);i++)
		{
		p=*(ta+0) * (*(xhong+i*(*nxhong)+0))
		 + *(ta+1) * (*(xhong+i*(*nxhong)+1))
		 + *(ta+2) * (*(xhong+i*(*nxhong)+2))
		 + *(ta+6);
		q= *(ta+3) * (*(xhong+i*(*nxhong)+0))
		 + *(ta+4) * (*(xhong+i*(*nxhong)+1))
		 + *(ta+5) * (*(xhong+i*(*nxhong)+2))
		 + 1.00;
		*(liny+i)=*(y+i) * q - p/q*(q-1.0);
/*
		*(liny+i)=(*(y+i)-epsilon[i]) * q - p/q*(q-1.0);
*/
		*(linx+i*nx+0)=*(xhong+i*(*nxhong)+0);
		*(linx+i*nx+1)=*(xhong+i*(*nxhong)+1);
		*(linx+i*nx+2)=*(xhong+i*(*nxhong)+2);
		*(linx+i*nx+3)= - (*(xhong+i*(*nxhong)+0)) * p/q ;
		*(linx+i*nx+4)= - (*(xhong+i*(*nxhong)+1)) * p/q ;
		*(linx+i*nx+5)= - (*(xhong+i*(*nxhong)+2)) * p/q ;
		}

		svdfitn(linx,nx,liny,ndatalin,newa,na,u,v,w,function);

	for(i=0;i<na;i++)
		{		
		*(da+i) = *(newa+i)-(*(ta+i));
		fprintf (stdout,"newa-olda=%10.4f-%10.4f=%10.4f\n",
			*(newa+i),*(ta+i),*(da+i) );
		fprintf(fdoutp, "newa-olda=%10.4f-%10.4f=%10.4f\n",
			*(newa+i),*(ta+i),*(da+i) );
		}

                                        /* comput  predicted values */
        for (i=0; i < ndatalin; i++)
        {
        eps1 = eps2 = 0.0;
          	for (k=0; k< *nxhong; k++)
          	{
		eps1 += newa[k] * (*(xhong+i*(*nxhong)+k));
           	eps2 += newa[k+(*nxhong)] * (*(xhong+i*(*nxhong)+k));
	  	}
        eps1 += newa[nx];
        eps2 += 1.0;
        y2[i] = eps1/eps2;
        }

					/* correlation of y_y 
						and t
					*/
    correlation (y,y2,ndatalin,&r);
    tscore (r,ndatalin,&t);

                                        /* print coefficients */
    fprintf (stdout,"------------------------------------\n");
    fprintf (fdoutp, "------------------------------------\n");
    show_parms(fdoutp, newa,na);
    for (i = 0; i < na; i++)
        if (w[i] == 0.0)
	   {
            fprintf (stdout,"   a[%d] has w==0\n", i);
            fprintf (fdoutp, "   a[%d] has w==0\n", i);
	   }

    fprintf (stdout," r=%g r^2=%g t=%g\n", r, r*r, t);
    fprintf (fdoutp, " r=%g r^2=%g t=%g\n", r, r*r, t);

/*                                                *** test matrix v
	for(i=0;i<na;i++)
	{
	  for(j=0;j<na;j++)
	  {
	    fprintf (stdout,"v[%2d][%2d] = %12.4f\n",i,j,v[i][j]);
	    fprintf(fdoutp, "v[%2d][%2d] = %12.4f\n",i,j,v[i][j]);
	  }
	}

	for(i=0;i<na;i++)
	{
	  for(j=0;j<na;j++)
	  {
	    test=0.0;
	    for(k=0;k<na;k++)
	      test += v[k][i]*v[k][j];

	    fprintf (stdout,"for i=%2d and j=%2d test[i,j]=%12.4f\n",i,j,test);
	    fprintf(fdoutp, "for i=%2d and j=%2d test[i,j]=%12.4f\n",i,j,test);
	  }
	}
*/

                                        /* check confidential, write sigma */
	sigma = vector (na);
	for(ka=0; ka<na; ka++)
	{
	  sigma[ka]=0.0;
      	  for(ma=0; ma<na; ma++)
	  {
	    if(w[ma] != 0.0)
 		sigma[ka] += (v[ka][ma]/w[ma])*(v[ka][ma]/w[ma]);
	  }
 	  fprintf (stdout,"w[%2d] = %12.4f\n",ka,w[ka]);
 	  fprintf(fdoutp, "w[%2d] = %12.4f\n",ka,w[ka]);
	  fprintf (stdout,"sigma[%1d] **2 =%12.4f\n", ka,sigma[ka]);
	  fprintf(fdoutp, "sigma[%1d] **2 =%12.4f\n", ka,sigma[ka]);
	}


		                                      /* Check utility:
                                        check total sum of squares SSto,
                                        residual sum of squares SSresid,
                                        mean value of y: ym,
                                        regression sum of squares
                                        */
        ssresid = ym = 0.0;
        for(i=0; i<ndatalin; i++)
        {
          ym += *(y+i);
          epsilon[i] = *(y+i) - y2[i];
          ssresid += epsilon[i] * epsilon[i];
          p=epsilon[i]/(*(y+i));
/*
          fprintf (stdout," epsilon/y for data i=%2d is=%12.4f\n", i,p);
          fprintf(fdoutp, " epsilon/y for data i=%2d is=%12.4f\n", i,p);
*/
        }
        ym = ym/ndatalin;
        fprintf (stdout," SSresid = %12.4f\n", ssresid);
        fprintf(fdoutp, " SSresid = %12.4f\n", ssresid);
        standard_residual = sqrt(ssresid/ndatalin);
        fprintf (stdout," standard residual = %16.10f\n", standard_residual );
        fprintf(fdoutp, " standard residual = %16.10f\n", standard_residual );
        fprintf (stdout," mean y = %12.4f\n", ym);
        fprintf(fdoutp, " mean y = %12.4f\n", ym);

        ssto = 0.0;
        for (i=0; i<ndatalin; i++)
        {
          ssto += (*(y+i) - ym)*(*(y+i) - ym);
        }
        fprintf (stdout," SSto = %12.4f\n", ssto);
        fprintf(fdoutp, " SSto = %12.4f\n", ssto);
        standard_variance = sqrt(ssto/ndatalin);
        fprintf (stdout," standard variance of y= %12.4f\n", standard_variance );
        fprintf(fdoutp, " standard variance of y= %12.4f\n", standard_variance);
        ff = ( ssto - ssresid ) / ssresid * ndatalin/nx;
        fprintf (stdout,"F=%12.4f\n",ff);
        fprintf(fdoutp, "F=%12.4f\n",ff);
        sigma1 = sqrt ( (double) ssresid/(ndatalin-(nx+1)));
        fprintf (stdout,"standard deviation = %12.4f\n",sigma1);
        fprintf(fdoutp, "standard deviation = %12.4f\n",sigma1);
        rr2 = (ssto -ssresid ) / ssto;
        rrr = 1. - (double) (( ndatalin - 1 ) /( ndatalin - 1 - nx )) *
                ssresid / ssto;
        fprintf (stdout,"R ** 2 =%12.4f\n",rr2);
        fprintf(fdoutp, "R ** 2 =%12.4f\n",rr2);
        fprintf (stdout,"adjusted R ** 2 =%12.4f\n",rrr);
        fprintf(fdoutp, "adjusted R ** 2 =%12.4f\n",rrr);
        fprintf (stdout,"new residual = %10.4f, old residual = %10.4f \n",
                standard_residual, standard_residual_old);
        fprintf(fdoutp, "new residual = %10.4f, old residual = %10.4f \n",
                standard_residual, standard_residual_old);

                                                /*  iteration */
if(G_yes("need more iteration?", 0))
{
        if(standard_residual < standard_residual_old)
        for(i=0; i<na; i++)
          *(ta+i)=(*(ta+i))+ 2.0 * (*(da+i));

        if(standard_residual > standard_residual_old)
          for(i=0;i<na;i++)
            *(ta+i)= (*(ta+i)) + 0.5 * (*(da+i));

        standard_residual_old = standard_residual;

        goto iteration;
}

        for(i=0; i<na; i++)
          *(*lina+i) = *(newa+i);

						/* write to file for graphic */
        for(i=0; i<ndatalin; i++)
        {
          fprintf(y_y,"%12.4f %12.4f\n",*(y+i),y2[i]);
          residuals[i]=epsilon[i]/sigma1;
          fprintf(curve_residual_x1,"%12.4f %12.4f\n",
		linx[i*nx+0],residuals[i]);
          fprintf(curve_residual_x2,"%12.4f %12.4f\n",
		linx[i*nx+1],residuals[i]);
          fprintf(curve_residual_x3,"%12.4f %12.4f\n",
		linx[i*nx+2],residuals[i]);
          fprintf(curve_residual_y,"%12.4f %12.4f\n",
		liny[i],residuals[i]);
        }
}

					/* check NDVI for non_linear problem */
nonlinear_linearization_NDVI(fdoutp, xhong,nxhong,ndata,y,a, lina)
	FILE *fdoutp;
	double *xhong,*y,*a,**lina;
	int  *nxhong,*ndata;
{
        FILE *curve_residual_x1;
        FILE *curve_residual_x2;
        FILE *curve_residual_x3;
        FILE *curve_residual_y;
        FILE *y_y;

	double p,q,q2,*newa,*linx,*liny,*sigma,*ta,*da;
	double ssresid,ym,*epsilon,ff,sigma1,*y2,ssto,rr2,rrr,*residuals;
	double *x,test,eps1,eps2,standard_residual,standard_variance;
	double standard_residual_old,r,t;
	int i,nx,na,ka,ma,ndatalin,j,k;
	double datan;
	double **u,**v,*w;
	void function();
	double **matrix();
	double *vector();

        curve_residual_x1 = fopen ("non.residual_x1", "w");
        curve_residual_x2 = fopen ("non.residual_x2", "w");
        curve_residual_x3 = fopen ("non.residual_x3", "w");
        curve_residual_y = fopen ("non.residual_y", "w");
        y_y = fopen ("non.y_y", "w");

	nx=(*nxhong)*2;
	na=nx+1;
	ndatalin=*ndata;
	linx=vector(nx*ndatalin);
	liny=vector(ndatalin);
	*lina=vector(na);
	ta=vector(na);
	da=vector(na);
	newa=vector(na);
	u=matrix(ndatalin,na);
	v=matrix(na,na);
	w=vector(na);
	y2=vector(ndatalin);
	epsilon=vector(ndatalin);
	residuals=vector(ndatalin);

	fprintf (stdout,"input a:%6.2f, %6.2f, %6.2f, %6.2f, %6.2f\n",
		*(a+0),*(a+1),*(a+2),*(a+3),*(a+4));
	fprintf(fdoutp, "input a:%6.2f, %6.2f, %6.2f, %6.2f, %6.2f\n",
		*(a+0),*(a+1),*(a+2),*(a+3),*(a+4));
	
	for(i=0; i<na; i++)
	{
	  *(ta+i)=*(a+i);
	}
	
	standard_residual_old = 0.0;

iteration:
	for(i=0; i<(*ndata); i++)
	{
	  p=*(ta+0) * (*(xhong+i*(*nxhong)+0)) + *(ta+1) * 
	    (*(xhong+i*(*nxhong)+1)) + *(ta+4);
	  q= *(ta+2) * (*(xhong+i*(*nxhong)+0))
		 + *(ta+3) * (*(xhong+i*(*nxhong)+1));

	  *(liny+i)=*(y+i) * q - p;
/*
	  *(liny+i)=(*(y+i)-epsilon[i]) * q - p/q*(q-1.0);
*/
	  *(linx+i*nx+0)=*(xhong+i*(*nxhong)+0);
  	  *(linx+i*nx+1)=*(xhong+i*(*nxhong)+1);
	  *(linx+i*nx+2)= - (*(xhong+i*(*nxhong)+0)) * p/q ;
	  *(linx+i*nx+3)= - (*(xhong+i*(*nxhong)+1)) * p/q ;
	}

	svdfitn(linx,nx,liny,ndatalin,newa,na,u,v,w,function);

	for(i=0; i<na; i++)
	{		
	*(da+i) = *(newa+i)-(*(ta+i));
	fprintf (stdout,"newa-olda=%10.4f-%10.4f=%10.4f\n", *(newa+i),*(ta+i),*(da+i) );
	fprintf(fdoutp, "newa-olda=%10.4f-%10.4f=%10.4f\n",
			*(newa+i),*(ta+i),*(da+i) );
	}

                                        /* comput  predicted values */
        for (i=0; i < ndatalin; i++)
        {
          eps1 = eps2 = 0.0;
          for (k=0; k < *nxhong; k++)
          {
	    eps1 += newa[k] * (*(xhong+i*(*nxhong)+k));
            eps2 += newa[k+(*nxhong)] * (*(xhong+i*(*nxhong)+k));
	  }
        eps1 += newa[nx];
        y2[i] = eps1/eps2;
        }

					/* correlation of y_y and t */
    correlation (y,y2,ndatalin,&r);
    tscore (r,ndatalin,&t);

                                        /* print coefficients */
    fprintf (stdout,"----------------------------------------\n");
    fprintf (fdoutp, "----------------------------------------\n");
    show_parms(fdoutp, newa,na);
    for (i = 0; i < na; i++)
      if (w[i] == 0.0)
      {
        fprintf (stdout,"   a[%d] has w==0\n", i);
        fprintf (fdoutp, "   a[%d] has w==0\n", i);
      }

    fprintf (stdout," r=%g r^2=%g t=%g\n", r, r*r, t);
    fprintf (fdoutp, " r=%g r^2=%g t=%g\n", r, r*r, t);

/*                                                *** test matrix v
	for(i=0; i<na; i++)
	{
	  for(j=0; j<na; j++)
	  fprintf (stdout,"v[%2d][%2d] = %12.4f\n",i,j,v[i][j]);
	}

	for(i=0; i<na; i++)
	{
	  for(j=0; j<na; j++)
	  {
	    test=0.0;
	    for(k=0; k<na; k++)
	      test += v[k][i]*v[k][j];

	    fprintf (stdout,"for i=%2d and j=%2d test[i,j]=%12.4f\n",i,j,test);
	  }
	}
*/

                                        /* check confidential, write sigma */
	sigma = vector (na);
	for(ka=0; ka<na; ka++)
	{
	  sigma[ka]=0.0;
	  for(ma=0; ma<na; ma++)
	  {
	    if(w[ma] != 0.0)
 	    sigma[ka] += (v[ka][ma]/w[ma])*(v[ka][ma]/w[ma]);
	  }
 	  fprintf (stdout,"w[%2d] = %12.4f\n",ka,w[ka]);
 	  fprintf(fdoutp, "w[%2d] = %12.4f\n",ka,w[ka]);
	  fprintf (stdout,"sigma[%1d] **2 =%12.4f\n", ka,sigma[ka]);
	  fprintf(fdoutp, "sigma[%1d] **2 =%12.4f\n", ka,sigma[ka]);
	}


		                                      /* Check utility:
                                        check total sum of squares SSto,
                                        residual sum of squares SSresid,
                                        mean value of y: ym,
                                        regression sum of squares
                                        */
        ssresid = ym = 0.0;
        for(i=0; i<ndatalin; i++)
        {
          ym += *(y+i);
          epsilon[i] = *(y+i) - y2[i];
          ssresid += epsilon[i] * epsilon[i];
          p=epsilon[i]/(*(y+i));
/*
          fprintf (stdout," epsilon/y for data i=%2d is=%12.4f\n", i,p);
          fprintf(fdoutp, " epsilon/y for data i=%2d is=%12.4f\n", i,p);
*/
        }
        ym = ym/ndatalin;
        fprintf (stdout," SSresid = %12.4f\n", ssresid);
        fprintf(fdoutp, " SSresid = %12.4f\n", ssresid);
        standard_residual = sqrt(ssresid/ndatalin);
        fprintf (stdout," standard residual = %16.10f\n", standard_residual );
        fprintf(fdoutp, " standard residual = %16.10f\n", standard_residual );
        fprintf (stdout," mean y = %12.4f\n", ym);
        fprintf(fdoutp, " mean y = %12.4f\n", ym);

        ssto = 0.0;
        for (i=0; i<ndatalin; i++)
          ssto += (*(y+i) - ym)*(*(y+i) - ym);
        fprintf (stdout," SSto = %12.4f\n", ssto);
        fprintf(fdoutp, " SSto = %12.4f\n", ssto);
        standard_variance = sqrt(ssto/ndatalin);
        fprintf (stdout," standard variance of y= %12.4f\n", standard_variance );
        fprintf(fdoutp, " standard variance of y= %12.4f\n", standard_variance);
        ff = ( ssto - ssresid ) / ssresid * ndatalin/nx;
        fprintf (stdout,"F=%12.4f\n",ff);
        fprintf(fdoutp, "F=%12.4f\n",ff);
        sigma1 = sqrt ( (double) ssresid/(ndatalin-(nx+1)));
        fprintf (stdout,"standard deviation = %12.4f\n",sigma1);
        fprintf(fdoutp, "standard deviation = %12.4f\n",sigma1);
        rr2 = (ssto -ssresid ) / ssto;
        rrr = 1. - (double)((ndatalin - 1)/(ndatalin - 1 - nx)) * ssresid/ssto;
        fprintf (stdout,"R ** 2 =%12.4f\n",rr2);
        fprintf(fdoutp, "R ** 2 =%12.4f\n",rr2);
        fprintf (stdout,"adjusted R ** 2 =%12.4f\n",rrr);
        fprintf(fdoutp, "adjusted R ** 2 =%12.4f\n",rrr);
        fprintf (stdout,"new residual = %10.4f, old residual = %10.4f \n",
                standard_residual, standard_residual_old);
        fprintf(fdoutp, "new residual = %10.4f, old residual = %10.4f \n",
                standard_residual, standard_residual_old);

                                                /*  iteration */
if(G_yes("need more iteration?",0))
{
        if(standard_residual < standard_residual_old)
        for(i=0; i<na; i++)
          *(ta+i)=(*(ta+i))+ 2.0 * (*(da+i));

        if(standard_residual > standard_residual_old)
        for(i=0; i<na; i++)
          *(ta+i)= (*(ta+i)) + 0.5 * (*(da+i));

        standard_residual_old = standard_residual;

        goto iteration;
}

        for(i=0; i<na; i++)
          *(*lina+i) = *(newa+i);
						/* write to file for graphic */
        for(i=0; i<ndatalin; i++)
        {
          fprintf(y_y,"%12.4f %12.4f\n",*(y+i),y2[i]);
          residuals[i]=epsilon[i]/sigma1;
          fprintf(curve_residual_x1,"%12.4f %12.4f\n",
		linx[i*nx+0],residuals[i]);
          fprintf(curve_residual_x2,"%12.4f %12.4f\n",
		linx[i*nx+1],residuals[i]);
          fprintf(curve_residual_x3,"%12.4f %12.4f\n",
		linx[i*nx+2],residuals[i]);
          fprintf(curve_residual_y,"%12.4f %12.4f\n",
		liny[i],residuals[i]);
        }
}

                                        /* relaxation VI fitting */
relaxation(fdoutp, xhong,nxhong,ndata,y,a)
	FILE *fdoutp;
        double *xhong,*y,*a;
        int  *nxhong,*ndata;
{
        FILE *curve_residual_x1;
        FILE *curve_residual_x2;
        FILE *curve_residual_x3;
        FILE *curve_residual_y;
        FILE *y_y;

        double p,q,q2,*newa,*linx,*liny,*sigma,*ta,*da;
        double ssresid,ym,*epsilon,ff,sigma1,*y2,ssto,rr2,rrr,*residuals;
        double *x,test,eps1,eps2,standard_residual,standard_variance;
        double standard_residual_old,r,t;
        int i,nx,na,ka,ma,ndatalin,j,k;
        double datan;
        double **u,**v,*w;
        void function();
        double **matrix();
        double *vector();

        curve_residual_x1 = fopen ("non.residual_x1", "w");
        curve_residual_x2 = fopen ("non.residual_x2", "w");
        curve_residual_x3 = fopen ("non.residual_x3", "w");
        curve_residual_y = fopen ("non.residual_y", "w");
        y_y = fopen ("non.y_y", "w");

        nx=*nxhong*2;
        na=nx+1;
        ndatalin=*ndata;
        linx=vector(nx*ndatalin);
        liny=vector(ndatalin);
        ta=vector(na);
        da=vector(na);
        newa=vector(na);
        u=matrix(ndatalin,na);
        v=matrix(na,na);
        w=vector(na);
        y2=vector(ndatalin);
        epsilon=vector(ndatalin);
        residuals=vector(ndatalin);

        for(i=0; i<na; i++)
          *(ta+i)=*(a+i);

	for(i=0; i<(*ndata); i++)
	{
	  p = q = 0.0;
	  for(ka=0; ka<(*nxhong); ka++)
	  {
            p = p + *(ta+ka) * (*(xhong+i*(*nxhong)+ka));
            q = q +  *(ta+ (*nxhong) +ka ) * (*(xhong+i*(*nxhong)+ka));
	  }
	  p = p+ *(ta + (*nxhong));
	  q=q+1.;
          *(liny+i)=*(y+i);
          *(linx+i)= p/q;
          }
         nx=1;
         na=nx+1;

         svdfitn(linx,nx,liny,ndatalin,newa,na,u,v,w,function);

                                        /* compute predicted values */
         for (i=0; i < ndatalin; i++)
           y2[i] = newa[0]* linx[i] + newa[1];

                                        /* correlation of y_y and t */
    correlation (y,y2,ndatalin,&r);
    tscore (r,ndatalin,&t);

                                        /* print coefficients */
    fprintf (stdout,"------------------------------------\n");
    fprintf (fdoutp, "------------------------------------\n");
    show_parms(fdoutp, newa,na);
    for (i = 0; i < na; i++)
      if (w[i] == 0.0)
      {
      fprintf (stdout,"   a[%d] has w==0\n", i);
      fprintf (fdoutp, "   a[%d] has w==0\n", i);
      }

    fprintf (stdout," r=%g r^2=%g t=%g\n", r, r*r, t);
    fprintf (fdoutp, " r=%g r^2=%g t=%g\n", r, r*r, t);

                                       /* check confidential, write sigma */
    sigma = vector (na);
    for(ka=0; ka<na; ka++)
      {
      sigma[ka]=0.0;
      for(ma=0; ma<na; ma++)
        if(w[ma] != 0.0)
          sigma[ka] += (v[ka][ma]/w[ma])*(v[ka][ma]/w[ma]);

      fprintf (stdout,"w[%2d] = %12.4f\n",ka,w[ka]);
      fprintf(fdoutp, "w[%2d] = %12.4f\n",ka,w[ka]);
      fprintf (stdout,"sigma[%1d] **2 =%12.4f\n", ka,sigma[ka]);
      fprintf(fdoutp, "sigma[%1d] **2 =%12.4f\n", ka,sigma[ka]);
      }

                                                      /* Check utility:
                                        check total sum of squares SSto,
                                        residual sum of squares SSresid,
                                        mean value of y: ym,
                                        regression sum of squares
                                        */
      ssresid = ym = 0.0;
      for(i=0; i<ndatalin; i++)
      {
      ym += *(y+i);
      epsilon[i] = *(y+i) - y2[i];
      ssresid += epsilon[i] * epsilon[i];
      p=epsilon[i]/(*(y+i));
/*
      fprintf (stdout," epsilon/y for data i=%2d is=%12.4f\n", i,p);
      fprintf(fdoutp, " epsilon/y for data i=%2d is=%12.4f\n", i,p);
*/
      }
      ym = ym/ndatalin;
      fprintf (stdout," SSresid = %12.4f\n", ssresid);
      fprintf(fdoutp, " SSresid = %12.4f\n", ssresid);
      standard_residual = sqrt(ssresid/ndatalin);
      fprintf (stdout," standard residual = %16.10f\n", standard_residual );
      fprintf(fdoutp, " standard residual = %16.10f\n", standard_residual );
      fprintf (stdout," mean y = %12.4f\n", ym);
      fprintf(fdoutp, " mean y = %12.4f\n", ym);

      ssto = 0.0;
      for (i=0; i<ndatalin; i++)
        ssto += (*(y+i) - ym)*(*(y+i) - ym);
      fprintf (stdout," SSto = %12.4f\n", ssto);
      fprintf(fdoutp, " SSto = %12.4f\n", ssto);
      standard_variance = sqrt(ssto/ndatalin);
      fprintf (stdout," standard variance of y= %12.4f\n", standard_variance );
      fprintf(fdoutp, " standard variance of y= %12.4f\n", standard_variance );
      ff = ( ssto - ssresid ) / ssresid * ndatalin / nx;
      fprintf (stdout,"F=%12.4f\n",ff);
      fprintf(fdoutp, "F=%12.4f\n",ff);
      sigma1 = sqrt ( (double) ssresid/(ndatalin-(nx+1)));
      fprintf (stdout,"standard deviation = %12.4f\n",sigma1);
      fprintf(fdoutp, "standard deviation = %12.4f\n",sigma1);
      rr2 = (ssto -ssresid ) / ssto;
      rrr = 1. - (double)((ndatalin - 1)/(ndatalin - 1 - nx)) * ssresid / ssto;
      fprintf (stdout,"R ** 2 =%12.4f\n",rr2);
      fprintf(fdoutp, "R ** 2 =%12.4f\n",rr2);
      fprintf (stdout,"adjusted R ** 2 =%12.4f\n",rrr);
      fprintf(fdoutp, "adjusted R ** 2 =%12.4f\n",rrr);
      fprintf (stdout,"new residual = %10.4f, old residual = %10.4f \n",
                standard_residual, standard_residual_old);
      fprintf(fdoutp, "new residual = %10.4f, old residual = %10.4f \n",
                standard_residual, standard_residual_old);
}

					/* do prediction for linear fitting*/
prediction_linear(fdoutp, x,y,ndata,nx,a,na)
	FILE *fdoutp;
	double *x,*y,*a;
	int ndata,nx,na;
{
	double eps1,eps2,*value,*y2,p,ssto,ff,sigma1,rr2,rrr,r,t;
	double ssresid,ym,*epsilon;
        double *vector();
	int i,k;

	y2=vector(ndata);
	epsilon=vector(ndata);
/*				* check input to prediction_linear
for(i=0; i<ndata; i++)
  {
  for(k=0; k<nx; k++)
    fprintf (stdout,"x[%2d * %2d + %2d]=%10.4f\t",i,nx,k,x[i*nx+k]);
  fprintf (stdout,"y[%2d]=%10.4f\n",i,y[i]);
  }
*/

	for (i=0; i < ndata; i++)
        {
          y2[i] = 0.0;
          for (k = 0; k < na-1; k++)
            y2[i] += a[k] * x[i*nx+k];
	 y2[i] +=a[nx];
        }
        ssresid =  ym = 0.0;
        for (i=0; i < ndata; i++)
        {
          ym += y[i];
          epsilon[i] = y[i] - y2[i];
          ssresid += epsilon[i] * epsilon[i];
          p=epsilon[i]/y[i];
/*
          fprintf (stdout," epsilon/y for data i=%2d is=%12.4f\n", i,p);
          fprintf(fdoutp, " epsilon/y for data i=%2d is=%12.4f\n", i,p);
*/
        }
        ym = ym/ndata;
        fprintf (stdout," SSresid for prediction= %12.4f\n", ssresid);
        fprintf(fdoutp, " SSresid for prediction= %12.4f\n", ssresid);
        fprintf (stdout," standard residiance for prediction = %16.10f\n", 
		sqrt(ssresid/ndata));
        fprintf(fdoutp, " standard residiance for prediction = %16.10f\n", 
		sqrt(ssresid/ndata));
        fprintf (stdout," mean y for prediction= %12.4f\n", ym);
        fprintf(fdoutp, " mean y for prediction= %12.4f\n", ym);

        ssto = 0.0;
        for (i=0; i<ndata; i++)
          ssto += (y[i] - ym)*(y[i] - ym);
        fprintf (stdout," SSto for prediction= %12.4f\n", ssto);
        fprintf(fdoutp, " SSto for prediction= %12.4f\n", ssto);
        fprintf (stdout," standard variance of y for prediction = %12.4f\n", 
		sqrt(ssto/ndata));
        fprintf(fdoutp, " standard variance of y for prediction = %12.4f\n", 
		sqrt(ssto/ndata));
        ff = ( ssto - ssresid ) / ssresid * ndata/nx;
        fprintf (stdout,"F for prediction=%12.4f\n",ff);
        fprintf(fdoutp, "F for prediction=%12.4f\n",ff);
        sigma1 = sqrt ( (double) ssresid/(ndata-(nx+1)));
        fprintf (stdout,"standard deviation for prediction= %12.4f\n",sigma1);
        fprintf(fdoutp, "standard deviation for prediction= %12.4f\n",sigma1);
        rr2 = (ssto -ssresid ) / ssto;
        rrr = 1. - (double)((ndata - 1) /(ndata - 1 - nx)) * ssresid / ssto;
        fprintf (stdout,"R ** 2 for prediction=%12.4f\n",rr2);
        fprintf(fdoutp, "R ** 2 for prediction=%12.4f\n",rr2);
        fprintf (stdout,"adjusted R ** 2 for prediction=%12.4f\n",rrr);
        fprintf(fdoutp, "adjusted R ** 2 for prediction=%12.4f\n",rrr);

                        /* calculate correlation coefficient between y and y2 */

    correlation (y,y2,ndata,&r);

                                        /* calculate t */
    tscore (r,ndata,&t);
	fprintf (stdout,"r=%g r^2=%g t=%g\n",r,r*r,t);
	fprintf(fdoutp, "r=%g r^2=%g t=%g\n",r,r*r,t);
}

					/* do prediction using other data set 
					for linear fitting*/
prediction_linear_other(fdoutp,file,model,a,na)
	FILE *fdoutp;
	char *file;
	double *a;
	int na,model;
{
    FILE *fd;
    double *vector();
    double *x,*xh,*xt, *y,*yt;
    double dummy;
    char *b;
    char buf[1024];
    int j,ndata,nx,nxh;
	double eps1,eps2,*value,*y2,p,ssto,ff,sigma1,rr2,rrr,r,t;
	double ssresid,ym,*epsilon;
	int i,k;

fprintf (stdout,"for predicting other imagery, input=%s\n",file );
fprintf(fdoutp, "for predicting other imagery, input=%s\n",file );
    fd = fopen (file, "r");
    if (fd == NULL)
    {
        perror (file);
        exit(1);
    }

					/* figure out how many variables 
					on the first line */
    if(NULL == fgets(b = buf, sizeof buf, fd))
        *buf = 0;

    nxh = 0;
    while(1)
    {
        while (*b == ' ' || *b == '\t' || *b == '\n')
                b++;
        if (*b == 0) break;

        nxh++;
        while (*b && *b != ' ' && *b != '\t' && *b != '\n')
                b++;
    }
    if (nxh == 1)
    {
        fprintf (stderr, "%s - no variables\n", file);
        exit(1);
    }
    if (nxh == 0)
    {
        fprintf (stderr, "%s - no data\n", file);
        exit(1);
    }
    fprintf (stdout,"\n\n%d variables\n", nxh-1);
    fprintf (fdoutp, "%d variables\n", nxh-1);

					/* read the file once to findout 
					how many data values */
    fseek (fd, 0L, 0);
    ndata = 0;
    while (fscanf(fd, "%lf", &dummy) == 1)
        ndata++;
    if (ndata % nxh)
    {
        fprintf (stderr, "%s - missing or extra data\n", file);
        exit(1);
    }
    ndata /= nxh;
    fprintf (stdout,"%d observations\n", ndata);
    fprintf (fdoutp, "%d observations\n", ndata);
    (nxh)--;
    xh = xt = vector(nxh*ndata);
    y = yt = vector (ndata);
    fseek (fd, 0L, 0);
    for (i = 0; i < ndata; i++)
    {
        for (j = 0; j < nxh; j++)
	    {
            fscanf (fd, "%lf", xt++);
	    }
        fscanf (fd, "%lf", yt++);
    }
    fclose (fd);

if (model == 1)
{
nx=nxh;
x=vector(nx,ndata);
x=xh;
goto regression;
}
	
if (model == 2)
{
normalization_x1(xh,&nxh,&ndata,&x,&nx);
goto regression;
}

if (model == 3 )
{
normalization_x2(xh,&nxh,&ndata,&x,&nx);
goto regression;
}

if (model == 4)
{
ndvi(xh,&nxh,&ndata,&x,&nx);
goto regression;
}

if(model == 5)
{
ndvi_intensity(xh,&nxh,&ndata,&x,&nx);
goto regression;
}

if(model == 6)
{
ndvi_reflectance(xh,&nxh,&ndata,&x,&nx);
goto regression;
}
if(model == 7)
{
rvi(xh,&nxh,&ndata,&x,&nx);
}
regression:



	y2=vector(ndata);
	epsilon=vector(ndata);

for(i=0;i<ndata;i++)
        {
        for(k=0;k<nx;k++)
		{
                fprintf (stdout,"x[%2d * %2d + %2d]=%10.4f\t",i,nx,k,x[i*nx+k]);
                fprintf(fdoutp, "x[%2d * %2d + %2d]=%10.4f\t",i,nx,k,x[i*nx+k]);
		}
        fprintf (stdout,"y[%2d]=%10.4f\n",i,y[i]);
        fprintf(fdoutp, "y[%2d]=%10.4f\n",i,y[i]);
        }

	  for (i=0; i < ndata; i++)
    {

        y2[i] = 0.0;
        for (k = 0; k < na-1; k++)
		{
            y2[i] += a[k] * x[i*nx+k];
		}
	 y2[i] +=a[nx];
fprintf (stdout,"in xformation,y2[%2d]=%10.4f\n",i,y2[i]);
fprintf(fdoutp, "in xformation,y2[%2d]=%10.4f\n",i,y2[i]);
    }
       ssresid = ym = 0.0;
        for (i=0; i < ndata; i++)
        {
          ym += y[i];
          epsilon[i] = y[i] - y2[i];
          ssresid += epsilon[i] * epsilon[i];
          p=epsilon[i]/y[i];
/*
          fprintf (stdout," epsilon/y for data i=%2d is=%12.4f\n", i,p);
*/
        }
        ym = ym/ndata;
        fprintf (stdout," SSresid for prediction= %12.4f\n", ssresid);
        fprintf(fdoutp, " SSresid for prediction= %12.4f\n", ssresid);
        fprintf (stdout," standard residiance for prediction = %16.10f\n", 
		sqrt(ssresid/ndata));
        fprintf(fdoutp, " standard residiance for prediction = %16.10f\n", 
		sqrt(ssresid/ndata));
        fprintf (stdout," mean y for prediction= %12.4f\n", ym);
        fprintf(fdoutp, " mean y for prediction= %12.4f\n", ym);

        ssto = 0.0;
        for (i=0; i<ndata; i++)
          ssto += (y[i] - ym)*(y[i] - ym);
        fprintf (stdout," SSto for prediction= %12.4f\n", ssto);
        fprintf(fdoutp, " SSto for prediction= %12.4f\n", ssto);
        fprintf (stdout," standard variance of y for prediction = %12.4f\n", 
		sqrt(ssto/ndata));
        fprintf(fdoutp, " standard variance of y for prediction = %12.4f\n", 
		sqrt(ssto/ndata));
        ff = ( ssto - ssresid ) / ssresid * ndata/nx;
        fprintf (stdout,"F for prediction=%12.4f\n",ff);
        fprintf(fdoutp, "F for prediction=%12.4f\n",ff);
        sigma1 = sqrt ( (double) ssresid/(ndata-(nx+1)));
        fprintf (stdout,"standard deviation for prediction= %12.4f\n",sigma1);
        fprintf(fdoutp, "standard deviation for prediction= %12.4f\n",sigma1);
        rr2 = (ssto -ssresid ) / ssto;
        rrr = 1. - (double)((ndata - 1)/(ndata - 1 - nx)) *  ssresid / ssto;
        fprintf (stdout,"R ** 2 for prediction=%12.4f\n",rr2);
        fprintf(fdoutp, "R ** 2 for prediction=%12.4f\n",rr2);
        fprintf (stdout,"adjusted R ** 2 for prediction=%12.4f\n",rrr);
        fprintf(fdoutp, "adjusted R ** 2 for prediction=%12.4f\n",rrr);

                        /* calculate correlation coefficient between y and y2 */

    correlation (y,y2,ndata,&r);

                                        /* calculate t */
    tscore (r,ndata,&t);
	fprintf (stdout,"r=%g r^2=%g t=%g\n",r,r*r,t);
	fprintf(fdoutp, "r=%g r^2=%g t=%g\n",r,r*r,t);

}



					/* do prediction for non-linear 
					fitting*/
prediction_nonlinear(fdoutp, x,y,ndata,nx,a,na)
	FILE *fdoutp;
	double *x,*y,*a;
	int ndata,nx,na;
{

    	double *vector();
	double eps1,eps2,*value,*y2,p,ssto,ff,sigma1,rr2,rrr,r,t;
	double ssresid,ym,*epsilon;
	int i,k;

	y2=vector(ndata);
	epsilon=vector(ndata);
        for (i=0; i < ndata; i++)
        {
        eps1=0.0;
        eps2=0.0;
          for (k=0; k< nx; k++)
	  {
           eps1 += a[k] * x[i*nx+k];
           eps2 += a[k+nx] * x[i*nx+k];}
          eps1 += a[nx*2];
          eps2 += 1.0;
        y2[i] = eps1/eps2;
        }

       ssresid = 0.0;
        ym = 0.0;
        for (i=0; i < ndata; i++)
        {
        ym += y[i];
        epsilon[i] = y[i] - y2[i];
        ssresid += epsilon[i] * epsilon[i];
        p=epsilon[i]/y[i];
                                        /*
        fprintf (stdout," epsilon/y for data i=%2d is=%12.4f\n", i,p);
                                        */
        }
        ym = ym/ndata;
        fprintf (stdout," SSresid for prediction= %12.4f\n", ssresid);
        fprintf(fdoutp, " SSresid for prediction= %12.4f\n", ssresid);
        fprintf (stdout," standard residiance for prediction= %16.10f\n", 
		sqrt(ssresid/ndata));
        fprintf(fdoutp, " standard residiance for prediction= %16.10f\n", 
		sqrt(ssresid/ndata));
        fprintf (stdout," mean y for prediction= %12.4f\n", ym);
        fprintf(fdoutp, " mean y for prediction= %12.4f\n", ym);

        ssto = 0.0;
        for (i=0;i<ndata;i++)
        {
        ssto += (y[i] - ym)*(y[i] - ym);
        }
        fprintf (stdout," SSto for prediction= %12.4f\n", ssto);
        fprintf(fdoutp, " SSto for prediction= %12.4f\n", ssto);
        fprintf (stdout," standard variance of y for prediction= %12.4f\n", 
		sqrt(ssto/ndata));
        fprintf(fdoutp, " standard variance of y for prediction= %12.4f\n", 
		sqrt(ssto/ndata));
        ff = ( ssto - ssresid ) / ssresid * ndata/nx;
        fprintf (stdout,"F for prediction=%12.4f\n",ff);
        fprintf(fdoutp, "F for prediction=%12.4f\n",ff);
        sigma1 = sqrt ( (double) ssresid/(ndata-(nx+1)));
        fprintf (stdout,"standard deviation for prediction= %12.4f\n",sigma1);
        fprintf(fdoutp, "standard deviation for prediction= %12.4f\n",sigma1);
        rr2 = (ssto -ssresid ) / ssto;
        rrr = 1. - (double) (( ndata - 1 ) /( ndata - 1 - nx )) *
                ssresid / ssto;
        fprintf (stdout,"R ** 2 for prediction=%12.4f\n",rr2);
        fprintf(fdoutp, "R ** 2 for prediction=%12.4f\n",rr2);
        fprintf (stdout,"adjusted R ** 2 for prediction=%12.4f\n",rrr);
        fprintf(fdoutp, "adjusted R ** 2 for prediction=%12.4f\n",rrr);


                        /* calculate correlation coefficient between y and y2 */

    correlation (y,y2,ndata,&r);

                                        /* calculate t */
    tscore (r,ndata,&t);
	fprintf (stdout,"r=%g r^2=%g t=%g\n",r,r*r,t);
	fprintf(fdoutp, "r=%g r^2=%g t=%g\n",r,r*r,t);
}




					/* do prediction for non-linear 
					fitting using other data set */
prediction_nonlinear_other(fdoutp, file,a,na)
	FILE *fdoutp;
	char *file;
	double *a;
	int na;
{

    FILE *fd;
    double *vector();
    double *x,*xt, *y,*yt;
    double dummy;
    char *b;
    char buf[1024];
    int j,ndata,nx;

	double eps1,eps2,*value,*y2,p,ssto,ff,sigma1,rr2,rrr,r,t;
	double ssresid,ym,*epsilon;
	int i,k;

	fprintf (stdout,"predicting other imagery for nonlinear fitting\n");
	fprintf (stdout,"input file=%s\n", file);
	fprintf(fdoutp, "predicting other imagery for nonlinear fitting\n");
	fprintf(fdoutp, "input file=%s\n", file);

   fd = fopen (file, "r");
    if (fd == NULL)
    {
        perror (file);
        exit(1);
    }

                                        /* figure out how many variables
                                        on the first line */
    if(NULL == fgets(b = buf, sizeof buf, fd))
        *buf = 0;

    nx = 0;
    while(1)
    {
        while (*b == ' ' || *b == '\t' || *b == '\n')
                b++;
        if (*b == 0) break;

        nx++;
        while (*b && *b != ' ' && *b != '\t' && *b != '\n')
                b++;
    }
    if (nx == 1)
    {
        fprintf (stderr, "%s - no variables\n", file);
        exit(1);
    }
    if (nx == 0)
    {
        fprintf (stderr, "%s - no data\n", file);
        exit(1);
    }
    fprintf (stdout,"%d variables\n", nx - 1);
    fprintf (fdoutp, "%d variables\n", nx - 1);

                                        /* read the file once to findout
                                        how many data values */
    fseek (fd, 0L, 0);
    ndata = 0;
    while (fscanf(fd, "%lf", &dummy) == 1)
        ndata++;
    if (ndata % nx)
    {
        fprintf (stderr, "%s - missing or extra data\n", file);
        exit(1);
    }
    ndata /= nx;
    fprintf (stdout,"%d observations\n", ndata);
    fprintf (fdoutp, "%d observations\n", ndata);
    (nx)--;
    x = xt = vector((nx)*ndata);
    y = yt = vector (ndata);
    fseek (fd, 0L, 0);
    for (i = 0; i < ndata; i++)
    {
        for (j = 0; j < nx; j++)
            fscanf (fd, "%lf", xt++);
        fscanf (fd, "%lf", yt++);
    }
    fclose (fd);


/*
fprintf (stdout,"test in xformation:\n");
for(i=0;i<ndata;i++)
        {
        for(k=0;k<nx;k++)
		{
                fprintf (stdout,"x[%2d * %2d + %2d]=%10.4f\t",i,nx,k,x[i*nx+k]);
                fprintf(fdoutp, "x[%2d * %2d + %2d]=%10.4f\t",i,nx,k,x[i*nx+k]);
		}
        fprintf (stdout,"y[%2d]=%10.4f\n",i,y[i]);
        fprintf(fdoutp, "y[%2d]=%10.4f\n",i,y[i]);
        }
*/



	y2=vector(ndata);
	epsilon=vector(ndata);
        for (i=0; i < ndata; i++)
        {
        eps1 = eps2 = 0.0;
          for (k=0; k< nx; k++)
	  {
            eps1 += a[k] * x[i*nx+k];
            eps2 += a[k+nx] * x[i*nx+k];}
            eps1 += a[nx*2];
            eps2 += 1.0;
            y2[i] = eps1/eps2;
          }

        ssresid = ym = 0.0;
        for (i=0; i < ndata; i++)
        {
          ym += y[i];
          epsilon[i] = y[i] - y2[i];
          ssresid += epsilon[i] * epsilon[i];
          p=epsilon[i]/y[i];
/*
          fprintf (stdout," epsilon/y for data i=%2d is=%12.4f\n", i,p);
*/
        }
        ym = ym/ndata;
        fprintf (stdout," SSresid for prediction= %12.4f\n", ssresid);
        fprintf(fdoutp, " SSresid for prediction= %12.4f\n", ssresid);
        fprintf (stdout," standard residiance for prediction= %16.10f\n", 
		sqrt(ssresid/ndata));
        fprintf(fdoutp, " standard residiance for prediction= %16.10f\n", 
		sqrt(ssresid/ndata));
        fprintf (stdout," mean y for prediction= %12.4f\n", ym);
        fprintf(fdoutp, " mean y for prediction= %12.4f\n", ym);

        ssto = 0.0;
        for (i=0; i<ndata; i++)
          ssto += (y[i] - ym)*(y[i] - ym);
        fprintf (stdout," SSto for prediction= %12.4f\n", ssto);
        fprintf(fdoutp, " SSto for prediction= %12.4f\n", ssto);
        fprintf (stdout," standard variance of y for prediction= %12.4f\n", 
		sqrt(ssto/ndata));
        fprintf(fdoutp, " standard variance of y for prediction= %12.4f\n", 
		sqrt(ssto/ndata));
        ff = ( ssto - ssresid ) / ssresid * ndata/nx;
        fprintf (stdout,"F for prediction=%12.4f\n",ff);
        fprintf(fdoutp, "F for prediction=%12.4f\n",ff);
        sigma1 = sqrt ( (double) ssresid/(ndata-(nx+1)));
        fprintf (stdout,"standard deviation for prediction= %12.4f\n",sigma1);
        fprintf(fdoutp, "standard deviation for prediction= %12.4f\n",sigma1);
        rr2 = (ssto -ssresid ) / ssto;
        rrr = 1. - (double)((ndata - 1)/(ndata - 1 - nx)) * ssresid / ssto;
        fprintf (stdout,"R ** 2 for prediction=%12.4f\n",rr2);
        fprintf(fdoutp, "R ** 2 for prediction=%12.4f\n",rr2);
        fprintf (stdout,"adjusted R ** 2 for prediction=%12.4f\n",rrr);
        fprintf(fdoutp, "adjusted R ** 2 for prediction=%12.4f\n",rrr);


                        /* calculate correlation coefficient between y and y2 */

    correlation (y,y2,ndata,&r);

                                        /* calculate t */
    tscore (r,ndata,&t);
	fprintf (stdout,"r=%g r^2=%g t=%g\n",r,r*r,t);
	fprintf(fdoutp, "r=%g r^2=%g t=%g\n",r,r*r,t);
}
