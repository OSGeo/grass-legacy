/* input.c (simlib), 20.nov.2002, JH */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "gis.h"
#include "site.h"
#include "bitmap.h"
#include "linkm.h"

#include "waterglobs.h"


/* ************************************************************** */
/*    			   GRASS input procedures, allocations    */
/* *************************************************************** */

int input_data()

{

	FCELL	 *cell1, *cell4b,*cell5;
        FCELL    *cell9, *cell10,*cell11;
        DCELL    *cell2, *cell3, *cell4, *cell4a,*cell12;
        int     fd1, fd2, fd3, fd4, fd4a, fd4b, fd5, row, row_rev;
	int 	fd9, fd10, fd11, fd12;
        int     l,j;
        int nn,cc,ii,dd;
	char *mapset;
	Site *site;
	char msg[1024];

        npoints = 0;
        npoints_alloc = 0;


/* put some warning messages about diff. in resol., etc. */

        if (sfile != NULL) {

        fw=fopen("simwe_data.txt","w"); /* open ascii file for ascii output of gamma */


  mapset = G_find_file ("site_lists", sfile, "");
  if (mapset == NULL)
  {
    sprintf (msg, "file [%s] not found", sfile);
    G_fatal_error (msg);
  }
  if ((fdsfile = G_fopen_sites_old (sfile, mapset)) == NULL)
  {
    sprintf (msg, "Cannot open %s", sfile);
    G_fatal_error (msg);
  }
    if (G_site_describe (fdsfile, &nn, &cc, &ii, &dd)!=0)
      G_fatal_error("failed to guess format");

    site = G_site_new_struct (cc, nn, ii, dd);
    fprintf (stderr, "Reading sites map (%s) ...\n\n\n", sfile);

/*        if (dd==0)
    {
      fprintf(stderr,"\n");
      G_warning("I'm finding records that do not have a floating point attributes (fields prefixed with '%').");
    } */

    while (G_site_get(fdsfile, site) >= 0)
    {

    if (npoints_alloc <= npoints)
    {
       npoints_alloc += 128;
       points = (struct Point*) G_realloc(points, npoints_alloc * sizeof (struct Point));
    }

        points[npoints].east = site->east * conv;
        points[npoints].north = site->north * conv;
        points[npoints].z1 = 0.; /*site->dbl_att[0];*/
/*printf("\n%f %f",points[npoints].east/conv,points[npoints].north/conv);*/
if((points[npoints].east/conv <= cellhd.east && points[npoints].east/conv >= cellhd.west) &&
        (points[npoints].north/conv <= cellhd.north && points[npoints].north/conv >= cellhd.south))
        npoints++;
    }
fclose(fdsfile);

	}

        cell1=G_allocate_f_raster_buf();
        cell2=G_allocate_d_raster_buf();
        cell3=G_allocate_d_raster_buf();
	  if(rain != NULL)
	cell4=G_allocate_d_raster_buf();
          if(infil != NULL)
        cell4a=G_allocate_d_raster_buf();
          if(traps != NULL)
        cell4b=G_allocate_f_raster_buf();
        cell5=G_allocate_f_raster_buf();
	  if(detin!=NULL)
        cell9=G_allocate_f_raster_buf();
          if(tranin!=NULL)
        cell10=G_allocate_f_raster_buf();
          if(tauin!=NULL)
        cell11=G_allocate_f_raster_buf();
	  if(wdepth!=NULL)
        cell12=G_allocate_d_raster_buf();

        zz = (float **)malloc(sizeof(float)*(my));
        v1 = (double **)malloc(sizeof(double)*(my));
        v2 = (double **)malloc(sizeof(double)*(my));
	if(rain != NULL)
        si = (double **)malloc(sizeof(double)*(my));
        if(infil != NULL)
        inf = (double **)malloc(sizeof(double)*(my));
        if(traps != NULL)
        trap = (float **)malloc(sizeof(float)*(my));
        cchez = (float **)malloc(sizeof(float)*(my));
          if(detin!=NULL)
        dc = (float **)malloc(sizeof(float)*(my));
          if(tranin!=NULL)
        ct = (float **)malloc(sizeof(float)*(my));
          if(tauin!=NULL)
        tau = (float **)malloc(sizeof(float)*(my));
	  if(wdepth!=NULL)
        gama = (double **)malloc(sizeof(double)*(my));

  for(l=0;l<my;l++)
   {
        zz[l]   = (float*)malloc(sizeof(float)*(mx));
        v1[l]   = (double*)malloc(sizeof(double)*(mx));
        v2[l]   = (double*)malloc(sizeof(double)*(mx));
        if(rain != NULL)
        si[l]   = (double*)malloc(sizeof(double)*(mx));
        if(infil != NULL)
        inf[l]   = (double*)malloc(sizeof(double)*(mx));
        if(traps != NULL)
        trap[l]   = (float*)malloc(sizeof(float)*(mx));
        cchez[l]   = (float*)malloc(sizeof(float)*(mx));
          if(detin!=NULL)
        dc[l]   = (float*)malloc(sizeof(float)*(mx));
          if(tranin!=NULL)
        ct[l]   = (float*)malloc(sizeof(float)*(mx));
          if(tauin!=NULL)
        tau[l]  = (float*)malloc(sizeof(float)*(mx));
          if(wdepth!=NULL)
	gama[l]  = (double*)malloc(sizeof(double)*(mx));
        }


  if((mapset=G_find_cell(elevin,""))==NULL)
  printf("cell file not found\n");

  if((mapset=G_find_cell(dxin,""))==NULL)
  printf("cell file not found\n");

  if((mapset=G_find_cell(dyin,""))==NULL)
  printf("cell file not found\n");

  if((mapset=G_find_cell(manin,""))==NULL)
  printf("cell file not found\n");

  if(rain != NULL){
	  if((mapset=G_find_cell(rain,""))==NULL)
	  printf("cell file not found\n");
	  fd4 = G_open_cell_old(rain,mapset);
  }

  if(infil != NULL){
          if((mapset=G_find_cell(infil,""))==NULL)
          printf("cell file not found\n");
          fd4a = G_open_cell_old(infil,mapset);
  }

  if(traps != NULL){
          if((mapset=G_find_cell(traps,""))==NULL)
          printf("cell file not found\n");
          fd4b = G_open_cell_old(traps,mapset);
  }

  if(detin != NULL){
          if((mapset=G_find_cell(detin,""))==NULL)
          printf("cell file not found\n");
          fd9 = G_open_cell_old(detin,mapset);
  }

  if(tranin != NULL){
          if((mapset=G_find_cell(tranin,""))==NULL)
          printf("cell file not found\n");
          fd10 = G_open_cell_old(tranin,mapset);
  }

  if(tauin != NULL){
          if((mapset=G_find_cell(tauin,""))==NULL)
          printf("cell file not found\n");
          fd11 = G_open_cell_old(tauin,mapset);
  }

  if(wdepth != NULL){
          if((mapset=G_find_cell(wdepth,""))==NULL)
          printf("cell file not found\n");
          fd12 = G_open_cell_old(wdepth,mapset);
  }

  fd1 = G_open_cell_old(elevin,mapset);
  fd2 = G_open_cell_old(dxin,mapset);
  fd3 = G_open_cell_old(dyin,mapset);
  fd5 = G_open_cell_old(manin,mapset);


  for (row=0; row<my; row++)
  {
          G_get_f_raster_row(fd1,cell1,row);
          G_get_d_raster_row(fd2,cell2,row);
          G_get_d_raster_row(fd3,cell3,row);
  	if(rain != NULL)
          G_get_d_raster_row(fd4,cell4,row);
        if(infil != NULL)
          G_get_d_raster_row(fd4a,cell4a,row);
        if(traps != NULL)
          G_get_f_raster_row(fd4b,cell4b,row);
          G_get_f_raster_row(fd5,cell5,row);
  	if(detin != NULL)
          G_get_f_raster_row(fd9,cell9,row);
  	if(tranin != NULL)
          G_get_f_raster_row(fd10,cell10,row);
  	if(tauin != NULL)
          G_get_f_raster_row(fd11,cell11,row);
        if(wdepth != NULL)
          G_get_d_raster_row(fd12,cell12,row);

        for (j=0; j<mx; j++)
        {
           row_rev = my - row - 1;
           zz[row_rev][j] = (float ) (conv * cell1[j]);
           v1[row_rev][j] = (double ) cell2[j];
           v2[row_rev][j] = (double ) cell3[j];
	  if(rain != NULL)
           si[row_rev][j] = (double ) cell4[j]; /* add conv */
          if(infil != NULL)
           inf[row_rev][j] = (double ) cell4a[j]; /* add conv */
          if(traps != NULL)
           trap[row_rev][j] = (float) cell4b[j]; /* add conv */
           cchez[row_rev][j] = (float ) cell5[j]; /* add conv? */
        if(detin != NULL)
           dc[row_rev][j] = (float ) cell9[j];
        if(tranin != NULL)
           ct[row_rev][j] = (float ) cell10[j];
        if(tauin != NULL)
           tau[row_rev][j] = (float ) cell11[j];
        if(wdepth != NULL)
           gama[row_rev][j] = (double ) cell12[j];
         }
   }
  G_close_cell(fd1);
  G_close_cell(fd2);
  G_close_cell(fd3);
  if(rain != NULL)
  G_close_cell(fd4);
  if(infil != NULL)
  G_close_cell(fd4a);
  if(traps != NULL)
  G_close_cell(fd4b);
  G_close_cell(fd5);
  if(detin != NULL)
  G_close_cell(fd9);
  if(tranin != NULL)
  G_close_cell(fd10);
  if(tauin != NULL)
  G_close_cell(fd11);
  if(wdepth != NULL)
  G_close_cell(fd12);

        return 1;
}

/* data preparations, sigma, shear, etc. */

int grad_check ()

{
    int k,l,i,j;
    double zx,zy,zd2,zd4,sinsl;

    double cc,cmul2;
    double sheer;
    double vsum = 0.;
    double vmax = 0.;
    double chsum = 0.;
    double zmin = 1.e12;
    double zmax = -1.e12;
    double zd2min = 1.e12;
    double zd2max = -1.e12;
    double smin = 1.e12;
    double smax = -1.e12;
    double infmin = 1.e12;
    double infmax = -1.e12;
    double sigmax = -1.e12;
    double cchezmax = -1.e12;
    double rhow = 1000.;
    double gacc = 9.81;
    double hh = 1.;
    double deltaw = 1.e12;
    
	sisum = 0.;
        infsum = 0.;
	cmul2 = rhow * gacc;
	
/* mandatory alloc. - should be moved to main.c*/

        slope = (double **)malloc(sizeof(double)*(my));

           for(l=0;l<my;l++)
              {
                slope[l]   = (double*)malloc(sizeof(double)*(mx));
              }
           for (j = 0; j < my; j++)
              {
                for (i = 0; i < mx; i++){

                   slope[j][i] = 0.;
		}
               }
/*** */


    for (k = 0; k < my; k++) {
        for (l = 0; l < mx; l++) {

            zx = v1[k][l];
            zy = v2[k][l];
            zd2 = zx * zx + zy * zy;
            sinsl = sqrt(zd2) / sqrt(zd2 + 1); /* sin(terrain slope) */
/* Computing MIN */
	    zd2 = sqrt(zd2);
            zd2min = amin1(zd2min,zd2);
/* Computing MAX */
            zd2max = amax1(zd2max,zd2);
            zd4 = sqrt(zd2); /* ^.25 */

            if (cchez[k][l] != 0.) {

                cchez[k][l] = 1. / cchez[k][l]; /* 1/n */

            } else {
	G_fatal_error(" Zero value in Mannings n");
                }

            if (zd2 == 0.) {
                v1[k][l] = 0.;
                v2[k][l] = 0.;
		slope[k][l] = 0.;
            } else {
		if (wdepth != NULL)
		hh = pow(gama[k][l],2./3.);
                v1[k][l] = (double)hh * cchez[k][l] * zx / zd4; /* hh = 1 if there is no water depth input */
                v2[k][l] = (double)hh * cchez[k][l] * zy / zd4;
		slope[k][l] = sqrt(v1[k][l] * v1[k][l] + v2[k][l] * v2[k][l]);
		}
	    

        if (wdepth != NULL) {
	    sheer = (double) (cmul2 * gama[k][l] * sinsl); /* shear stress */
	        if ((sheer <= tau[k][l]) || (ct[k][l] == 0.)) { /* if critical shear stress >= shear then all zero */
		si[k][l] = 0.;
		sigma[k][l] = 0.;
		} else
		{
		si[k][l] = (double) (dc[k][l] * (sheer - tau[k][l]));
                sigma[k][l] = (double) (dc[k][l]/ct[k][l])*(sheer-tau[k][l])/(pow(sheer,1.5)); /* rill erosion=1.5, sheet = 1.1 */
		}
	}

            sisum += si[k][l];
	    smin = amin1(smin,si[k][l]);
	    smax = amax1(smax,si[k][l]);
        if (inf != NULL) {
            infsum += inf[k][l];
            infmin = amin1(infmin,inf[k][l]);
            infmax = amax1(infmax,inf[k][l]);
	}
            vmax = amax1(vmax,slope[k][l]);
            vsum += slope[k][l];
            chsum += cchez[k][l];

            zmin = amin1(zmin,(double)zz[k][l]);
            zmax = amax1(zmax,(double)zz[k][l]); /* not clear were needed */
	if (wdepth != NULL)	
	    sigmax = amax1(sigmax,sigma[k][l]);
	    cchezmax = amax1(cchezmax,cchez[k][l]);

	cchez[k][l] *= sqrt(sinsl); /* saved sqrt(sinsl)*cchez to cchez array for output*/

        }
    }

    cc = (double) mx * my;
    si0 = sisum / cc;
    vmean = vsum / cc;
    chmean = chsum / cc;
        if (inf != NULL) 
    infmean = infsum / cc;

	if (wdepth != NULL)
    deltaw = 0.8/(sigmax * vmax);
    deltap = 0.25 * sqrt(stepx * stepy)/vmean;
        if(deltaw > deltap)
                timec = 4.;
        else
                timec = 1.25;

/*    deltap = amin1(deltap,deltaw);*/

    if (wdepth != NULL) deltap = 0.1; /* deltap for sediment is ar. average deltap and deltaw */

    miter = (int)(timesec / (deltap * timec)); /* number of iterations */
    iterout = (int)(iterout / (deltap * timec)); /* iterations for time series */

printf("\n");
printf("\n zmin,zmax %f %f",zmin,zmax);
printf("\n simean,vmean,chmean,deltap %f %f %f %f",si0,vmean,chmean,deltap);
if (wdepth != NULL) printf("\n sigmax,vmax,deltaw %f %f %f",sigmax,vmax,deltaw);
printf("\n MITER, timec %d %f",miter,timec);

    for (k = 0; k < my; k++) {
        for (l = 0; l < mx; l++) {

                v1[k][l] *= deltap; /* normalization */
                v2[k][l] *= deltap;

	if (inf!=NULL) inf[k][l] *= timesec; /* THIS IS CORRECT SOLUTION currently commented out*/

	if(wdepth != NULL) gama[k][l] = 0.;

		if(et!=NULL) {
		if(sigma[k][l] == 0. || slope[k][l] == 0.)
			si[k][l] = 0.;
		else
			si[k][l] = si[k][l] / (slope[k][l] * sigma[k][l]); /* temp for transp. cap. erod */
		}
        }
    }

	if(et!=NULL) {

          erod(si); /* compute divergence of t.capc */

	 if (output_et() != 1) G_fatal_error ("Cannot write et file");
	}

	if(wdepth != NULL) {

	    for (k = 0; k < my; k++) {
        	for (l = 0; l < mx; l++) {

		if(et!=NULL) si[k][l] = si[k][l] * slope[k][l] * sigma[k][l]; /* get back from temp */
		if( sigma[k][l] != 0.)
		sigma[k][l] = exp(-sigma[k][l] * deltap * slope[k][l]); /* not clear what's here :-\ */
		}
	   }
	}

return (1);
}


double amax1(arg1,arg2)
 double arg1;
 double arg2;
{
 double res;
 if (arg1>=arg2) {
   res = arg1;
 }
 else  {
   res = arg2;
 }
 return res;
}


double amin1(arg1,arg2)
 double arg1;
 double arg2;
{
 double res;
 if (arg1<=arg2) {
   res = arg1;
 }
 else  {
   res = arg2;
 }
 return res;
}


int min(arg1,arg2)
 int arg1;
 int arg2;
{
 int res;
 if (arg1 <= arg2)
  {
   res = arg1;
  }
 else
  {
   res = arg2;
   }
 return res;
}

int max(arg1,arg2)
 int arg1;
 int arg2;
{
 int res;
 if (arg1>=arg2) {
   res = arg1;
 }
 else  {
   res = arg2;
 }
 return res;
}


