/*
**  Written by H. Mitasova, I. Kosinovsky, D. Gerdes  Spring 1992
**  US Army Construction Engineering Research Lab
**  Copyright  H. Mitasova, I. Kosinovsky, D.Gerdes  USA-CERL  1992
*/

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <unistd.h>
#include "gis.h"
#include "bitmap.h"

#include "quad.h"
#include "surf.h"
#include "userextern.h"
#include "userglobs.h"
#include "user.h"

int secpar_loop (int ngstc, int nszc, int k)
{
	double          dnorm1,
	 ro,		/* rad to deg conv */
	 dx2=0.0L,
	 dy2=0.0L,
	 grad2=0.0L,		/* gradient squared */
	 slp=0.0L,
	 grad,		/* gradient */
	 oor=0.0L,		/* aspect  (orientation) */
	 disk,		/* discriminant */
	 cur1,		/* temp */

	 cur2,		/* temp */
	 curn=0.0L,		/* profile curvature */
	 curh=0.0L,		/* tangential curvature */
	 curm=0.0L,		/* mean curvature */
	 temp,		/* temp  variable*/
	 dxy2;		/* temp variable   square of part diriv. */

	double          gradmin;
	int             i, got, bmask=1;
	static int		first_time_g = 1;

	ro = 57.295779;
	gradmin = 0.001;

	for (i = ngstc; i <= nszc; i++)
		{
		if(maskmap != NULL)
			{
			bmask = BM_get(bitmask, i, k); 
			}
  		got = 0;
  		if (bmask == 1 )
  			{
 			while ((got == 0) && (cond1)) {
				dx2 = adx[i] * adx[i];
				dy2 = ady[i] * ady[i];
				grad2 = dx2 + dy2;
				grad = sqrt (grad2);
				/* slope in %        slp = 100. * grad; */
				/* slope in degrees*/	
				slp = ro * atan (grad);   
				if (grad <= gradmin)
					{
					oor = 0.;
					got = 3;
					if(cond2) {
						disk = (adxx[i] - adyy[i]) *
						(adxx[i] - adyy[i]) + 4. *
						adxy[i] * adxy[i];
						disk = sqrt (disk);
						cur1 = 0.5 * (adxx[i] +
							adyy[i] + disk);
						cur2 = 0.5 * (adxx[i] +
							adyy[i] - disk);
						if ((cur1 == 0.) &&
							(cur2 == 0.))
							{
							curn = 0.;
							curh = 0.;
							got = 3;
							break;
							}
						if ((cur1 > 0.) && (cur2 > 0.))
							{
							curn = 10.;
							curh = 10.;
							got = 3;
							break;
							}
						if ((cur1 < 0.) && (cur2 < 0.))
							{
							curn = 20.;
							curh = 20.;
							got = 3;
							break;
							}
						if (cur1 * cur2 < 0.)
							{
	       						curn = 30.;
							curh = 30.;
							got = 3;
							break;
							}
						}
					}
				if (got == 3)
					break;

/***********aspect from r.slope.aspect, with adx, ady computed
            from interpol. function RST **************************/

                if (adx[i] == 0)
                {
                    if (ady[i] > 0) oor = 90;  
                    else oor = 270;   
                }
                else 
		{
		   oor = ro * atan2(ady[i],adx[i]);
		   if(oor<=0)oor=360.+oor;
                 }

	got = 1;
    }			/* while */
    if ((got != 3) && (cond2))
    {

    dnorm1 =  sqrt (grad2 + 1.); 
    dxy2 = 2. * adxy[i] * adx[i] * ady[i]; 


   curn = (adxx[i]*dx2+dxy2+adyy[i]*dy2)/(grad2*dnorm1*dnorm1*dnorm1); 

   curh = (adxx[i] * dy2 - dxy2 + adyy[i] * dx2) / (grad2 * dnorm1); 

   temp = grad2 + 1.; 
   curm = .5*((1.+dy2) * adxx[i] - dxy2 + (1.+dx2) * adyy[i]) / (temp*dnorm1);
    }
    if (first_time_g)
    {
	first_time_g = 0;
	gmin = gmax = slp;
	c1min = c1max = curn;
	c2min = c2max = curh;
    }
    gmin = amin1 (gmin, slp);
    gmax = amax1 (gmax, slp);
    c1min = amin1 (c1min, curn);
    if (curn<10.)
    c1max = amax1 (c1max, curn);
    c2min = amin1 (c2min, curh);
    if (curh<10.)
    c2max = amax1 (c2max, curh);
   if (cond1)
   {
     adx[i] = slp;
     ady[i] = oor;
     if (cond2)
     {
       adxx[i] = curn;
       adyy[i] = curh;
       adxy[i] = curm;
     }
   }
  }
 }
return 1;
}





int 
fill_array1 (
    int **cell_table,   /*Table to fill */
    char *Tmp_file,
    int shorty
)
{
    int             i,j,k;
    FILE            *Tmp_fd=NULL;
    int             *array_int=NULL;
    short int       *array_ints=NULL;
    int             colmax,colmin;
    int             rowmax,rowmin;
    int	            first_time;
    int             ngstc,nszc,ngstr,nszr;
    int             ind[4];

    if (Tmp_file==NULL) {
       fprintf(stderr,"filename is NULL in fill_array()!\n");
       exit (0);
    }
    first_time = 1;
    if (NULL == (Tmp_fd = fopen (Tmp_file, "r"))) {
       sprintf (msg, "Can't open temp file [%s] for reading", Tmp_file);
       G_fatal_error (msg);
       exit (1);
    }
    if (shorty == 1) {
      if (!(array_ints = (short int *) malloc (sizeof (short int) * (nsizc + 1))))
      {
         fprintf (stderr,"Allocation problem for array_ints!!! \n");
         exit (0);
      }
    }
    else {
      if (!(array_int = (int *) malloc (sizeof (int) * (nsizc + 1))))
      {
         fprintf (stderr,"Allocation problem for array_int!!! \n");
         exit (0);
      }
    }
    for (k = 1; k<= total; k++) {
      fread(ind,sizeof(int),4,Tmp_fd);
      ngstc=ind[0];
      nszc= ind[1];
      ngstr=ind[2];
      nszr= ind[3];
      if (first_time) {		/* first time through - set min, max values */
	      first_time = 0;
	      colmin = ngstc;
	      colmax = nszc;
	      rowmin = ngstr;
	      rowmax = nszr;
      }
      if (ngstc<colmin) colmin=ngstc;
      if (nszc>colmax)  colmax=nszc;
      if (ngstr<rowmin) rowmin=ngstr;
      if (nszr>rowmax)  rowmax=nszr;
      for (i=ngstr;i<=nszr;i++) {
        if(shorty == 1) 
            fread(array_ints, sizeof(short int), nszc-ngstc+1, Tmp_fd);
        else
            fread(array_int, sizeof(int), nszc-ngstc+1, Tmp_fd);
	    for (j = ngstc; j <= nszc; j++)
	    {
              if (shorty == 1)
		cell_table[i][j] = (int)array_ints[j - ngstc];
              else
		cell_table[i][j] = array_int[j - ngstc];
	    }
      }
      
    }
    if (shorty == 1)
      free (array_ints);
    else 
      free(array_int);

    fclose(Tmp_fd);
    return 1;
 }






int fill_row (
    CELL *row,   /*row to fill */
    char *Tmp_file,
    int n_row,   /* The number of row in the raster */
    int shorty
)
{
    int             j,k;
    FILE            *Tmp_fd=NULL;
    int             *array_int=NULL;
    short int       *array_ints=NULL;
    int             ngstc,nszc,ngstr,nszr;
    int             ind[4];

    if (Tmp_file==NULL) {
       fprintf(stderr,"filename is NULL in fill_array()!\n");
       exit (0);
    }
    if (NULL == (Tmp_fd = fopen (Tmp_file, "r"))) {
       sprintf (msg, "Can't open temp file [%s] for reading", Tmp_file);
       G_fatal_error (msg);
       exit (1);
    }
    if (shorty == 1) {
      if (!(array_ints = (short int *) malloc (sizeof (short int) * (nsizc + 1))))
      {
         fprintf (stderr,"Allocation problem for array_ints!!! \n");
         exit (0);
      }
    }
    else {
      if (!(array_int = (int *) malloc (sizeof (int) * (nsizc + 1))))
      {
         fprintf (stderr,"Allocation problem for array_int!!! \n");
         exit (0);
      }
    }
    for (k = 1; k<= total; k++) {
      fread(ind,sizeof(int),4,Tmp_fd);
      ngstc=ind[0];
      nszc= ind[1];
      ngstr=ind[2];
      nszr= ind[3];
      
      if ((n_row+1<ngstr)||(n_row+1>nszr))  {
       if(shorty == 1) 
         if (fseek(Tmp_fd,(long)(nszr-ngstr+1)*(nszc-ngstc+1)*sizeof(short int),
                                                  1) == -1) return -1;
       else
         if (fseek(Tmp_fd,(long)(nszr-ngstr+1)*(nszc-ngstc+1)*sizeof(int),
                                                  1) == -1) return -1;
      }
      else   {
       if(shorty == 1) {
         if (fseek(Tmp_fd,(long)(n_row+1-ngstr)*(nszc-ngstc+1)*sizeof(short int),
                                                  1) == -1) return -1;
            fread(array_ints, sizeof(short int), nszc-ngstc+1, Tmp_fd);
            for (j = ngstc; j <= nszc; j++)  {
        	   row[j-1] = (CELL)array_ints[j - ngstc];
            }
            if (fseek(Tmp_fd,(long)(nszr-n_row-1)*(nszc-ngstc+1)*sizeof(short int),
                                                  1) == -1) return -1;
         } 
       else  {
         if (fseek(Tmp_fd,(long)(n_row+1-ngstr)*(nszc-ngstc+1)*sizeof(int),
                                                  1) == -1) return -1;
            fread(array_int, sizeof(int), nszc-ngstc+1, Tmp_fd);
            for (j = ngstc; j <= nszc; j++)  {
        	   row[j-1] = (CELL)array_int[j - ngstc];
            }
            if (fseek(Tmp_fd,(long)(nszr-n_row-1)*(nszc-ngstc+1)*sizeof(int),
                                                  1) == -1) return -1;
         }
      }
/*
      for (i=ngstr;i<=nszr;i++) {
        if(shorty == 1) 
            fread(array_ints, sizeof(short int), nszc-ngstc+1, Tmp_fd);
        else
            fread(array_int, sizeof(int), nszc-ngstc+1, Tmp_fd);
        for (j = ngstc; j <= nszc; j++)
        {
           if (i == n_row + 1) {
              if (shorty ==  1)
        	   row[j-1] = (CELL)array_ints[j - ngstc];
              else  row[j-1] = (CELL)array_int[j - ngstc];
           }
        }
      }
*/
      
    }

    if (shorty == 1)
      free (array_ints);
    else 
      free(array_int);

    fclose(Tmp_fd);
    return 1;
 }





int 
COGRR1 (double x_or, double y_or, int n_rows, int n_cols, int n_points, struct triple *points)
/*C
C       INTERPOLATION BY FUNCTIONAL METHOD : TPS + complete regul.
c
*/
{
    static double         *w2 = NULL;
    double 	    amaxa;
    double          stepix, stepiy, RO, xx, yy, xg, yg, xx2;
    double          rfsta2,/*cons, cons1,*/ wm, dx, dy, dxx, dyy, dxy, h;
    double          bmgd1=0.0L, bmgd2;
    int             n1, k1, k2, k, i1, l, l1, n4, n5, m, i;
    int             ngstc, nszc, ngstr, nszr;
    int             iiii, hiii=0;
    int             *array_int=NULL;
    short int       *array_ints=NULL;
    int             j,jr;
    double          zz,steps1,fstst,fstp;
    int             ind[4];
    double          /*rsm*/ val;
    double          rlogma=10000.;
    double          stepl=0.1, steps=0.05;
    double          logstepce;
    int             bmask=1;
    static int		    first_time_z = 1;

#define CEULER .57721566

    logstepce=CEULER+log(stepl);

/*
c normalization
*/
/**************************this will be changed with new segmentation
we must not forget it (as I usually do)**********************************/
    stepix = ew_res / dnorm;
    stepiy = ns_res / dnorm;
    tfstad = tfsta2 / dnorm;

    if (!w2)
    {
	if (!(w2 = (double *) malloc (sizeof (double) * (KMAX2 + 9))))
	{
	    fprintf (stderr,"Allocation problem!!! \n");
	    exit (0);
	}
      }
    ++hiii;
    for (i = 1; i <= n_points; i++)
    {
/*
            if(i<=40) fprintf (stdout,"before second transl: %f,%f,%f\n",
                   points[i-1].x,points[i-1].y,points[i-1].z);
*/
	points[i - 1].x = (points[i - 1].x - x_or) / dnorm;
	points[i - 1].y = (points[i - 1].y - y_or) / dnorm;
	points[i - 1].z = points[i - 1].z / dnorm;
    }
    n1 = n_points + 1;
/*
C
C      GENERATION OF MATRIX
C
C      FIRST COLUMN
C
*/
    A[1] = 0.;
    for (k = 1; k <= n_points; k++)
    {
	i1 = k + 1;
	A[i1] = 1.;
    }
/*
C
C      OTHER COLUMNS
C
*/
/*
    fprintf(stderr,"dnorm=%f\n",dnorm);
    fprintf(stderr,"smoothing parameter?\n");
    scanf("%lf",&rsm);
    fprintf(stderr,"rsm=%f",rsm);
*/
    RO = -rsm;
    for (k = 1; k <= n_points; k++)
    {
	k1 = k * n1 + 1;
	k2 = k + 1;
	i1 = k1 + k;
	A[i1] = RO;
	for (l = k2; l <= n_points; l++)
	{
	    xx = points[k - 1].x - points[l - 1].x;
	    yy = points[k - 1].y - points[l - 1].y;
	    rfsta2 = fstar2 * (xx * xx + yy * yy);
	    if (rfsta2 == 0.)
	    {
		fprintf (stderr,"ident. points in segm.  \n");
		fprintf (stderr,"x[%d]=%f,x[%d]=%f,y[%d]=%f,y[%d]=%f\n",
			k - 1, points[k - 1].x, l - 1, points[l - 1].x, k - 1, points[k - 1].y, l - 1, points[l - 1].y);
	    }
	    i1 = k1 + l;
	    A[i1] = crst (rfsta2);
	}
    }
/*
C
C       SYMMETRISATION
C
*/
    amaxa = 1.;
    for (k = 1; k <= n1; k++)
    {
	k1 = (k - 1) * n1;
	k2 = k + 1;
	for (l = k2; l <= n1; l++)
	{
	    m = (l - 1) * n1 + k;
	    A[m] = A[k1 + l];
	    amaxa = amax1 (A[m], amaxa);
	}
    }

/*
C        RIGHT SIDE
C
*/
    n4 = n1 * n1 + 1;
    A[n4] = 0.;
    for (l = 1; l <= n_points; l++)
    {
	l1 = n4 + l;
	A[l1] = points[l - 1].z;
    }
    n5 = n1 * (n1 + 1);
    for (i = 1; i <= n5; i++)
	A[i] = A[i] / amaxa;

/*
       ncont=n1*n1+n1;
       for(icont=1;icont<=ncont;icont++)
       fprintf (stdout,"mat %f", A[icont]);
       fprintf (stdout,"\n");
*/
/*
        SOLVING OF SYSTEM
*/

    if (LINEQS (n1, n1, 1, &NERROR, &DETERM))
    {

	for (k = 1; k <= n_points; k++)
	{
	    l = n4 + k;
	    b[k] = A[l];
	}
	b[n_points + 1] = A[n4];


	POINT (/* x_or, y_or,*/ n_points, points);
/*
C
C         INTERPOLATION   *  MOST INNER LOOPS !
C
*/
/*    fprintf(stderr,"system solved=%f",rsm); */
	 /*     cons = -fi / (3.1415926 * 2.);   */
 /*	cons1 = cons / dnorm; */
	iiii = 0;


	ngstc = (int) (x_or / ew_res + 0.5) + 1;
	nszc = ngstc + n_cols - 1;
	ngstr = (int) (y_or / ns_res + 0.5) + 1;
	nszr = ngstr + n_rows - 1;

        ind[0]=ngstc;
        ind[1]=nszc;
        ind[2]=ngstr;
        ind[3]=nszr;
    
        if (elev != NULL) {
	  if(!(fwrite(ind,sizeof(int),4,Tmp_fd_z))) {
            fprintf(stderr,"Not enough disk space -- cannot write files\n");
            exit(0);
          }
        }
        if (slope != NULL) {
	  if(!(fwrite(ind,sizeof(int),4,Tmp_fd_dx))) {
            fprintf(stderr,"Not enough disk space -- cannot write files\n");
            exit(0);
          }
        }
        if (aspect != NULL) {
	  if(!(fwrite(ind,sizeof(int),4,Tmp_fd_dy))) {
            fprintf(stderr,"Not enough disk space -- cannot write files\n");
            exit(0);
          }
        }
        if (pcurv != NULL) {
	  if(!(fwrite(ind,sizeof(int),4,Tmp_fd_xx))) {
            fprintf(stderr,"Not enough disk space -- cannot write files\n");
            exit(0);
          }
        }
        if (tcurv != NULL) {
	  if(!(fwrite(ind,sizeof(int),4,Tmp_fd_yy))) {
            fprintf(stderr,"Not enough disk space -- cannot write files\n");
            exit(0);
          }
        }
        if (mcurv != NULL) {
	  if(!(fwrite(ind,sizeof(int),4,Tmp_fd_xy))) {
            fprintf(stderr,"Not enough disk space -- cannot write files\n");
            exit(0);
          }
        }
	if (!(array_int = (int *) malloc (sizeof (int) * (nszc-ngstc+1))))
	{
	    fprintf (stderr,"Allocation problem for array_int!!! \n");
	    exit (0);
	}
	if (!(array_ints = (short int *) malloc (sizeof (short int) * (nszc-ngstc+1))))
	{
	    fprintf (stderr,"Allocation problem for array_ints!!! \n");
	    exit (0);
	}
/*    fprintf(stderr,"interpolation begins =%f\n",rsm);*/
	steps1=steps/stepl;
        fstst = fstar2/stepl;
        fstp = -2./stepl;
/*
	    for (l = ngstc; l <= nszc; l++)
	    {
                ll1 = l - ngstc;
                ll2 = ll1 + 1;
		xg = ll1 * stepix;
		for (m = 1; m <= n_points; m++)
		{
		    xx = xg - points[m - 1].x;
		   wxx2[ll2][m] = fstar2 * xx * xx/stepl;
		   wxx[ll2][m]=xx;
                 }
             }
*/        
	for (k = ngstr; k <= nszr; k++)
	{
	    yg = (k - ngstr) * stepiy;
	    for (m = 1; m <= n_points; m++)
	    {
		wm = yg - points[m - 1].y;
	  	w[m] = wm;    
		w2[m] = fstar2 * wm * wm/stepl;
	    }
			iiii++;
	    for (l = ngstc; l <= nszc; l++)
	    {
              if(maskmap != NULL)
                bmask = BM_get(bitmask, l, k );
/*                ll2 = l - ngstc + 1; */
		xg = (l - ngstc) * stepix;      

		dx = 0.;
		dy = 0.;
		dxx = 0.;
		dyy = 0.;
		dxy = 0.;
                zz = 0.;
                if (bmask == 1)
                { /* compute everything for area which is not masked out */
                  h = b[n1];
                  /*fprintf(stderr,"iner cyckle begins %f\n",rsm);*/

		  for (m = 1; m <= n_points; m++)
		  {
                    xx = xg - points[m-1].x;
                    xx2 = fstst * xx * xx;
		    rfsta2 = xx2 + w2[m];
			if (rfsta2 < 350.) 
		        {
		        jr=rfsta2/steps1;
		        val = sp[jr][1]+rfsta2*(sp[jr][2]
                           +rfsta2*(sp[jr][3]+rfsta2*sp[jr][4]));

/*fprintf(stderr,"A,jr=%d,m=%d, val=%lg, sp=%lg, %lg %lg, %lg \n",
               jr, m, val, sp[jr][1],sp[jr][2],sp[jr][3],sp[jr][4]);*/

			   if (cond1)  
			   {
			   bmgd1=b[m]*(sp[jr][5]+rfsta2*(sp[jr][6]
			                        +rfsta2*sp[jr][7]));
			   dx=dx+bmgd1*xx;
			   dy=dy+bmgd1*w[m];
                           }
                           if (cond2)
			    {
			    if (rfsta2 == 0.)
 		              {
			       dxx = dxx + b[m];
			       dyy = dyy + b[m];
			       }
                            else
                              {
		            bmgd2 = b[m] * (sp[jr][8] + sp[jr][9]*rfsta2);
			    dxx = dxx + bmgd2 * xx2 + bmgd1;
			    dyy = dyy + bmgd2 * w2[m] + bmgd1;
			    dxy = dxy + bmgd2 * fstst * xx * w[m];
                              }
/*
fprintf(stderr,"A,jr%d, %d,  dx,y%lg, %lg bmgd1 %lg, b%lg\n",
                  jr, m,dx,dy,bmgd1,b[m]);
fprintf(stderr,"A,sp=  %lg, %lg, %lg \n", sp[jr][5],sp[jr][6],sp[jr][7]);
    fprintf(stderr,"continue ?\n");
    scanf("%f",&rsm);
*/
			   }
			}
			else if (rfsta2<rlogma)
			     {	
			     jr=rfsta2;
			     val=alg[jr]*rfsta2+blg[jr];
			     	if (cond1)
				{
				bmgd1=b[m]/(rfsta2*stepl);
			        dx=dx+bmgd1*xx;
			        dy=dy+bmgd1*w[m];
/*fprintf(stderr,"B,%d, %d, %f, %f, %f, %f \n", jr, m,dx,dy,bmgd1,b[m]);*/
				}
                                if (cond2)
                               {
                               bmgd2 = b[m] * fstp/(rfsta2*rfsta2);
			       dxx = dxx + bmgd2 * xx2 + bmgd1;
			       dyy = dyy + bmgd2 * w2[m] + bmgd1;
			       dxy = dxy + bmgd2 * fstst * xx * w[m];
                               }
			     }
			     else	 
			     {
			     val=log(rfsta2)+logstepce;	
			     	if (cond1)
				{
				bmgd1=b[m]/(stepl*rfsta2);
			        dx=dx+bmgd1*xx;
			        dy=dy+bmgd1*w[m];
/*fprintf(stderr,"C, %d, %d, %f, %f, %f, %f \n", jr, m,dx,dy,bmgd1,b[m]);*/
				}
                                if (cond2)
                               {
                               bmgd2 = b[m] * fstp/(rfsta2*rfsta2);
			       dxx = dxx + bmgd2 * xx2 + bmgd1;
			       dyy = dyy + bmgd2 * w2[m] + bmgd1;
			       dxy = dxy + bmgd2 * fstst * xx * w[m];
                               }
			     }

    /*fprintf(stderr,"iner cyckle %d\n",m);*/
			h = h + b[m] * val;

                        }

		zz = (h * dnorm) + zmin;
/*   fprintf(stderr,"az=...=%d\n",l); */
		if (first_time_z)
		{
		    first_time_z = 0;
		    zmaxac = zminac = zz;
		}
                zmaxac = amax1(zz,zmaxac);
                zminac = amin1(zz,zminac);
		if ((zz > zmax + 0.1 * (zmax - zmin))
			|| (zz < zmin - 0.1 * (zmax - zmin)))
		{
		    static int once = 0;

		    if (!once)
		    {
			once = 1;
			fprintf (stderr, "WARNING:\n");
			fprintf (stderr, "Overshoot -- increase in tension suggested.\n");
			fprintf (stderr, "Overshoot occures at (%d,%d) cell\n", l, k);
			fprintf (stderr, "The z-value is %f, zmin is %f,zmax is %f\n", zz, zmin, zmax);
		    }
		}
         } /* skip here if you are in masked area, zz,dx, ... dxy should be 0 */

		az[l] = zz;
		if (cond1)
		{
/*    fprintf(stderr," before adx,ady=...=%d,%f,%f,%f\n",l,dx,dy,tfsta2); */
		    adx[l] = -dx * tfsta2;
		    ady[l] = -dy * tfsta2;
 /*   fprintf(stderr,"adx,ady=...=%d\n",l); */
		    if (cond2)
		    {
			adxx[l] = -dxx * tfstad;
			adyy[l] = -dyy * tfstad;
			adxy[l] = -dxy * tfstad;
		    }
		}
	    }
            if ((iw2!=0)&&(cond1)) {
              if (!(secpar_loop(ngstc,nszc,k))) exit (0);
/*    fprintf(stderr,"secpar done..=%d\n",l);*/
            }

	    if (elev != NULL)
            {
	      for (j = 0; j < n_cols; j++)
	      {
		array_int[j] = (int) ((az[ngstc + j] * sciz) + 0.5);
	      }
	      if(!(fwrite(array_int,sizeof(int),n_cols,Tmp_fd_z))) {
                fprintf(stderr,"Not enough disk space -- cannot write files\n");
                exit(0);
              }
            }
	    if (slope != NULL)
	    {
	      for (j = 0; j < n_cols; j++)
	      {
		array_ints[j] = (short int) ((adx[ngstc + j] * scig) + 0.5);
	      }
	      if(!(fwrite(array_ints,sizeof(short int),n_cols,Tmp_fd_dx))) {
                fprintf(stderr,"Not enough disk space -- cannot write files\n");
                exit(0);
              }
            }
	    if (aspect != NULL)
            {
	      for (j = 0; j < n_cols; j++)
	      {
                if ( ady[ngstc + j] > 0. && ady[ngstc + j] < 0.5 )
                     ady[ngstc + j] = 360.;
		array_ints[j] = (short int) (ady[ngstc + j] * scio + 0.5);
	      }
	      if(!(fwrite(array_ints,sizeof(short int),n_cols,Tmp_fd_dy))) {
                fprintf(stderr,"Not enough disk space -- cannot write files\n");
                exit(0);
              }
            }
	    if (pcurv != NULL)
	    {
	      for (j = 0; j < n_cols; j++)
	      {
	        array_int[j] = (int) ((adxx[ngstc + j] * scik1) + 0.5);
	      }
	      if(!(fwrite(array_int,sizeof(int),n_cols,Tmp_fd_xx))) {
                fprintf(stderr,"Not enough disk space -- cannot write files\n");
                exit(0);
              }
            }
	    if (tcurv != NULL)
	    {
	      for (j = 0; j < n_cols; j++)
	      {
	        array_int[j] = (int) ((adyy[ngstc + j] * scik2) + 0.5);
	      }
	      if(!(fwrite(array_int,sizeof(int),n_cols,Tmp_fd_yy))) {
                fprintf(stderr,"Not enough disk space -- cannot write files\n");
                exit(0);
              }
            }
	    if (mcurv != NULL)
	    {
	      for (j = 0; j < n_cols; j++)
	      {
	        array_int[j] = (int) ((adxy[ngstc + j] * scik3) + 0.5);
	      }
	      if(!(fwrite(array_int,sizeof(int),n_cols,Tmp_fd_xy))) {
                fprintf(stderr,"Not enough disk space -- cannot write files\n");
                exit(0);
              }
            }

	}
    }				/* falls here if LINEQS() returns 0 */
    total++;
    free(array_ints);
    free(array_int);
/*
    fprintf(stderr,"interp. ends=%f\n",rsm);
    fprintf(stderr,"nszc %d nszr %d n_points %d\n",nszc, nszr, n_points);
*/
    return 1;
}




int 
POINT (

/*    double          xs0, ys0; */
    int n_points,
    struct triple *points
)
/*
c  interpolation check of z-values in given points
c
*/

{
    double          rfsta2, errmax, h, xx, yy, r2, hz, zz, err;
    int             n1, mm, m, mmax;


    errmax = .0;
    n1 = n_points + 1;
    for (mm = 1; mm <= n_points; mm++)
    {
	h = b[n1];
	for (m = 1; m <= n_points; m++)
	{
	    xx = points[mm - 1].x - points[m - 1].x;
	    yy = points[mm - 1].y - points[m - 1].y;
	    r2 = yy * yy + xx * xx;
	    if (r2 != 0.)
	    {
		rfsta2 = fstar2 * r2;
		h = h + b[m] * crst (rfsta2);
	    }
	}
	hz = (h * dnorm) + zmin;
	zz = (points[mm - 1].z * dnorm) + zmin;
	err = hz - zz;
/*	  fprintf (stdout,"h= %f hz= %f zz= %f err= %f \n", h, hz, zz, err); */
	if (err < 0)
	{
	    err = -err;
	}
	if (err >= errmax)
	{
	    errmax = err;
	    mmax = mm;
	}
    }

    ertot = amax1 (errmax, ertot);
/*
    if (errmax > ertre)
    {
	xmm = (points[mmax - 1].x * dnorm) + 
               ((struct quaddata *) (root->data))->x_orig;
	ymm = (points[mmax - 1].y * dnorm) + 
               ((struct quaddata *) (root->data))->y_orig;
	zmm = (points[mmax - 1].z * dnorm) + zmin;
	fprintf (stderr," max. error = %f at point i = %d \n", errmax, mmax);
	fprintf (stderr," x(i) = %f  y(i) = %f \n", xmm, ymm);
	fprintf (stderr," z(i) = %f \n", zmm);
    }
*/
    return 1;
}

void 
lntab (void)
{
int        i;
int        maxl=10000;
double     x,x1;
double     stepl=0.1;

for (i = 1; i <= maxl; ++i) 
 {
    x = i * stepl;
    if ( x == 0. ) x = 1.;
    x1 = x + stepl;
    alg[i] = log(x1/x) /*/stepl*/;
    blg[i] = log(x) - (alg[i]/stepl)*x+.57721566;
 /*blg[i]=log(x)-alg[i]*x+.57721566;*/
 }
}			   
/******************************************************************/

/* computes coef. of cubic spline aproximating CRS */
/*	subroutine spline (xt,yt,n,yd0,ydm,splcr)  */
/*c	yp1>1.e+30 -> natural spline               */
                                                      
        void 
spline (void)    
        {
 double      y2[1000],u[1000];
 double      yp1,ypn,sig,p,qn,un,hi,xt3,xt31;
 int         maxs=901;
 int         i,k,n;
/*   double       steps=0.05, stepl=0.1; */

        n=maxs;
	yp1=yd0;
	ypn=ydm;

	if (yp1 >= .99E+30)	
           {
		y2[1]=0.;
		u[1]=0.;
            }
		else
            {
		y2[1]=-.5;
	u[1]=(3./(xt[2]-xt[1]))*((yt[2]-yt[1])/(xt[2]-xt[1])-yp1);
           }
 /*       fprintf (stdout,"u1=%g\n", u[1]); */

          for (i=2; i<=n-1; ++i)
              {
		sig=(xt[i]-xt[i-1])/(xt[i+1]-xt[i-1]);
		p=sig*y2[i-1]+2.;
		y2[i]=(sig-1.)/p;
	u[i]=(6.*((yt[i+1]-yt[i])/(xt[i+1]-xt[i])-(yt[i]-yt[i-1])
     	     /(xt[i]-xt[i-1]))/(xt[i+1]-xt[i-1])-sig*u[i-1])/p;	
/*        fprintf (stdout,"i=%d,ui=%g\n",i,u[i]);              */
              }

	if (ypn>= .99E+30)	
           {
		qn=0.;
		un=0.;
           }
	else
           {
		qn=0.5;
	un=(3./(xt[n]-xt[n-1]))*(ypn-(yt[n]-yt[n-1])/(xt[n]-xt[n-1]));
           }

	y2[n]=(un-qn*u[n-1])/(qn*y2[n-1]+1.);

        for (k=n-1; k>=1; --k)
            {
        	y2[k]=y2[k]*y2[k+1]+u[k];
/*        fprintf (stdout,"k=%d,y2=%g\n",k,y2[k]); */

            }


        for (i=1; i<=n-1; ++i)
            {
	hi=xt[i+1]-xt[i];
        xt3=xt[i]*xt[i]*xt[i];
        xt31=xt[i+1]*xt[i+1]*xt[i+1];
/*        fprintf (stdout,"i=%d,xt=%g,hi=%g,xt3=%g,%g\n",i,xt[i],hi,xt3,xt31); */
        sp[i][1]=(xt[i+1]*yt[i]-xt[i]*yt[i+1])/hi
                +hi*(y2[i+1]*xt[i]-xt[i+1]*y2[i])/6.
                +(xt31*y2[i]-xt3*y2[i+1])/(6.*hi);
       	sp[i][2]=(yt[i+1]-yt[i])/hi+hi*(y2[i]-y2[i+1])/6.
               +(xt[i]*xt[i]*y2[i+1]-y2[i]*xt[i+1]*xt[i+1])/(2.*hi);
        sp[i][3]=(xt[i+1]*y2[i]-xt[i]*y2[i+1])/(2.*hi);
	sp[i][4]=(y2[i+1]-y2[i])/(6.*hi);
/*        fprintf (stdout,"splinekoefi= %d,c1=%g,c2=%g,c3=%g,c4=%g\n", i,sp[i][1],sp[i][2],sp[i][3],sp[i][4]);   */
              }
        }

/******************************************************************/

void s_table (void)
{
	int         mm;
        double      xtmm;
	double      shift=5.;
        int         maxs=901;
        double       steps=0.05, stepl=0.1;

        for ( mm=1; mm<=maxs; mm++ )
          {
           xt[mm] = (mm-1)*steps-shift;              
	   xtmm=xt[mm];
	   if (xt[mm]<0.) xtmm=-xt[mm];
           yt[mm] = crst(xtmm);
	   if (xt[mm]<0.) yt[mm]=-yt[mm];
 /*          fprintf (stdout,"mm=%d,x=%f, y=%g\n", mm, xt[mm], yt[mm] ); */
          }
        yd0 = (yt[2]-yt[1])/steps;
	ydm=  (yt[maxs]-yt[maxs-1])/steps;

/*        fprintf (stdout,"max=%d,xmax=%g,yd0=%f,ydm=%g\n",max,xt[max],yd0,ydm); */
      
        spline();
	for (mm=101; mm<=maxs; ++mm)
	{
	sp[mm-101][1]=sp[mm][1];
	sp[mm-101][2]=sp[mm][2]*stepl;
	sp[mm-101][3]=sp[mm][3]*stepl*stepl;
	sp[mm-101][4]=sp[mm][4]*stepl*stepl*stepl;

        sp[mm-101][5]=sp[mm][2];
        sp[mm-101][6]=2.*sp[mm][3]*stepl;
        sp[mm-101][7]=3.*sp[mm][4]*stepl*stepl;

        sp[mm-101][8]=4.*sp[mm][3]*stepl;
        sp[mm-101][9]=12.*sp[mm][4]*stepl*stepl;
	}
 }
