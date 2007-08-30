/* input.c (simlib), 20.nov.2002, JH */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <grass/gis.h>
#include <grass/site.h>
#include <grass/bitmap.h>
#include <grass/glocale.h>
#include <grass/linkm.h>
#include <grass/waterglobs.h>


/* ************************************************************** */
/*    			   GRASS input procedures, allocations    */
/* *************************************************************** */

/*!
 * \brief allocate memory, read input rasters, assign UNDEF to NODATA
 *
 *  \return int
 */


int input_data (void)
{

	FCELL	 *cell1, *cell4b,*cell5;
        FCELL    *cell9, *cell10,*cell11;
        DCELL    *cell2, *cell3, *cell4, *cell4a,*cell12;
        int     fd1, fd2, fd3, fd4, fd4a, fd4b, fd5, row, row_rev;
	int 	fd9, fd10, fd11, fd12;
        int     l,j;
        int nn,cc,ii,dd;
        double unitconv=0.0000002; /* mm/hr to m/s */
	char *mapset;
	Site *site;

        npoints = 0;
        npoints_alloc = 0;

/* put some warning messages about diff. in resol., etc. */

        if (sfile != NULL) {
        fw=fopen("simwe_data.txt","w"); /* open ascii file for ascii output of gamma */

  mapset = G_find_sites (sfile, "");
  if (mapset == NULL)
    G_fatal_error (_("File [%s] not found"), sfile);

  if ((fdsfile = G_fopen_sites_old (sfile, mapset)) == NULL)
    G_fatal_error (_("Unable to open file [%s]"), sfile);

    if (G_site_describe (fdsfile, &nn, &cc, &ii, &dd)!=0)
      G_fatal_error (_("Failed to guess file format"));

    site = G_site_new_struct (cc, nn, ii, dd);
    G_message (_("Reading sites map (%s) ..."), sfile);

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

G_sites_close(fdsfile);
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

        zz = (float **)G_malloc (sizeof(float *)*(my));
        v1 = (double **)G_malloc (sizeof(double *)*(my));
        v2 = (double **)G_malloc (sizeof(double *)*(my));

	if(rain != NULL)
        si = (double **)G_malloc (sizeof(double *)*(my));

        if(infil != NULL)
        inf = (double **)G_malloc (sizeof(double *)*(my));

        if(traps != NULL)
        trap = (float **)G_malloc (sizeof(float *)*(my));
        cchez = (float **)G_malloc (sizeof(float *)*(my));

          if(detin!=NULL)
        dc = (float **)G_malloc (sizeof(float *)*(my));

          if(tranin!=NULL)
        ct = (float **)G_malloc (sizeof(float *)*(my));

          if(tauin!=NULL)
        tau = (float **)G_malloc (sizeof(float *)*(my));

	  if(wdepth!=NULL)
        gama = (double **)G_malloc (sizeof(double *)*(my));

  for(l=0;l<my;l++)
   {
        zz[l]   = (float*)G_malloc (sizeof(float)*(mx));
        v1[l]   = (double*)G_malloc (sizeof(double)*(mx));
        v2[l]   = (double*)G_malloc (sizeof(double)*(mx));

        if(rain != NULL)
        si[l]   = (double*)G_malloc (sizeof(double)*(mx));

        if(infil != NULL)
        inf[l]   = (double*)G_malloc (sizeof(double)*(mx));

        if(traps != NULL)
        trap[l]   = (float*)G_malloc (sizeof(float)*(mx));
        cchez[l]   = (float*)G_malloc (sizeof(float)*(mx));

          if(detin!=NULL)
        dc[l]   = (float*)G_malloc (sizeof(float)*(mx));

          if(tranin!=NULL)
        ct[l]   = (float*)G_malloc (sizeof(float)*(mx));

          if(tauin!=NULL)
        tau[l]  = (float*)G_malloc (sizeof(float)*(mx));

          if(wdepth!=NULL)
	gama[l]  = (double*)G_malloc (sizeof(double)*(mx));
        }

   G_message (_("Running MAY 10 version"));

  if((mapset=G_find_cell(elevin,""))==NULL)
  G_fatal_error(_("Raster map <%s> not found"), elevin);

  fd1 = G_open_cell_old(elevin,mapset);

  if((mapset=G_find_cell(dxin,""))==NULL)
  G_fatal_error(_("Raster map <%s> not found"), dxin);

  fd2 = G_open_cell_old(dxin,mapset);

  if((mapset=G_find_cell(dyin,""))==NULL)
  G_fatal_error(_("Raster map <%s> not found"), dyin);

  fd3 = G_open_cell_old(dyin,mapset);

  if((mapset=G_find_cell(manin,""))==NULL)
  G_fatal_error(_("Raster map <%s> not found"), manin);

  fd5 = G_open_cell_old(manin,mapset);

  if(rain != NULL){
	  if((mapset=G_find_cell(rain,""))==NULL)
	  G_fatal_error(_("Raster map <%s> not found"), rain);
	  fd4 = G_open_cell_old(rain,mapset);
  }

  if(infil != NULL){
          if((mapset=G_find_cell(infil,""))==NULL)
          G_fatal_error(_("Raster map <%s> not found"), infil);

          fd4a = G_open_cell_old(infil,mapset);
  }

  if(traps != NULL){
          if((mapset=G_find_cell(traps,""))==NULL)
          G_fatal_error(_("Raster map <%s> not found"), traps);

          fd4b = G_open_cell_old(traps,mapset);
  }

  if(detin != NULL){
          if((mapset=G_find_cell(detin,""))==NULL)
          G_fatal_error(_("Raster map <%s> not found"), detin);

          fd9 = G_open_cell_old(detin,mapset);
  }

  if(tranin != NULL){
          if((mapset=G_find_cell(tranin,""))==NULL)
          G_fatal_error(_("Raster map <%s> not found"), tranin);

          fd10 = G_open_cell_old(tranin,mapset);
  }

  if(tauin != NULL){
          if((mapset=G_find_cell(tauin,""))==NULL)
          G_fatal_error(_("Raster map <%s> not found"), tauin);

          fd11 = G_open_cell_old(tauin,mapset);
  }

  if(wdepth != NULL){
          if((mapset=G_find_cell(wdepth,""))==NULL)
          G_fatal_error(_("Raster map <%s> not found"), wdepth);

          fd12 = G_open_cell_old(wdepth,mapset);
  }

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

	   if(!G_is_f_null_value(cell1+j))
           zz[row_rev][j] = (float ) (conv * cell1[j]);
	   else
		   zz[row_rev][j] = UNDEF;

	   if(!G_is_d_null_value(cell2+j))
           v1[row_rev][j] = (double ) cell2[j];
	   else
		   v1[row_rev][j] = UNDEF;

	   if(!G_is_d_null_value(cell3+j))
           v2[row_rev][j] = (double ) cell3[j];
	   else
		   v2[row_rev][j] = UNDEF;

	if(v1[row_rev][j] == UNDEF || v2[row_rev][j] == UNDEF)
		zz[row_rev][j] = UNDEF; /* undef all area if something's missing */
	   
/* should be ? if(v1[row_rev][j] == UNDEF || v2[row_rev][j] == UNDEF || zz[row_rev][j] = UNDEF)
  * {v1[row_rev][j] == UNDEF;
  * v2[row_rev][j] == UNDEF;
  * zz[row_rev][j] = UNDEF;}
  * printout warning?
  */      
	  if (rain != NULL)
	  {
		  if(!G_is_d_null_value(cell4+j))
            si[row_rev][j] = ((double ) cell4[j]) * unitconv; /*conv mm/hr to m/s*/
 /*        printf("\n INPUTrain, convert %f %f",si[row_rev][j],unitconv); */

		  else {
			  si[row_rev][j] = UNDEF;
			zz[row_rev][j] = UNDEF;
		  }

          if (infil != NULL)
          {
		  if(!G_is_d_null_value(cell4a+j))
           inf[row_rev][j] = (double ) cell4a[j] * unitconv;  /*conv mm/hr to m/s*/
 /*        printf("\n INPUT infilt, convert %f %f",inf[row_rev][j],unitconv); */
		  else {
			  inf[row_rev][j] = UNDEF;
                        zz[row_rev][j] = UNDEF;
		  }
		  }

          if (traps != NULL)
          {
		  if(!G_is_f_null_value(cell4b+j))
           trap[row_rev][j] = (float) cell4b[j]; /* no conv, unitless */
		  else {
			  trap[row_rev][j] = UNDEF;
                        zz[row_rev][j] = UNDEF;
		  }
		  }
      }

	  if(!G_is_f_null_value(cell5+j))
           cchez[row_rev][j] = (float ) cell5[j]; /* units in manual */
	  else {
		  cchez[row_rev][j] = UNDEF;
                        zz[row_rev][j] = UNDEF;
	  }

        if (detin != NULL)
        {
		if(!G_is_f_null_value(cell9+j))
           dc[row_rev][j] = (float ) cell9[j]; /*units in manual*/
		else {
			dc[row_rev][j] = UNDEF;
                        zz[row_rev][j] = UNDEF;
		}
		}

        if (tranin != NULL)
        {
		if(!G_is_f_null_value(cell10+j))
           ct[row_rev][j] = (float ) cell10[j]; /*units in manual*/
		else {
			ct[row_rev][j] = UNDEF;
                        zz[row_rev][j] = UNDEF;
		}
		}

        if (tauin != NULL)
        {
		if(!G_is_f_null_value(cell11+j))
           tau[row_rev][j] = (float ) cell11[j]; /*units in manual*/
		else {
			tau[row_rev][j] = UNDEF;
                        zz[row_rev][j] = UNDEF;
		}
		}

        if (wdepth != NULL)
        {
		if(!G_is_d_null_value(cell12+j))
           gama[row_rev][j] = (double ) cell12[j]; /*units in manual*/
		else {
			gama[row_rev][j] = UNDEF;
                        zz[row_rev][j] = UNDEF;
		}
		}
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
int grad_check (void)
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
        slope = (double **)G_malloc (sizeof(double *)*(my));

           for(l=0;l<my;l++)
                slope[l]   = (double*)G_malloc (sizeof(double)*(mx));

           for (j = 0; j < my; j++)
              {
                for (i = 0; i < mx; i++)
                   slope[j][i] = 0.;
               }
/*** */

    for (k = 0; k < my; k++) {
        for (l = 0; l < mx; l++) {

		if (zz[k][l] != UNDEF) {
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
	G_fatal_error (_("Zero value in Mannings n"));
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
	} /* DEFined area */
        }
    }

    if (inf != NULL && smax < infmax)
        G_warning (_("Infiltration exceeds the rainfall rate everywhere! No overland flow."));

    cc = (double) mx * my;
    si0 = sisum / cc;
    vmean = vsum / cc;
    chmean = chsum / cc;

        if (inf != NULL) infmean = infsum / cc;

	if (wdepth != NULL) deltaw = 0.8/(sigmax * vmax);
        deltap = 0.25 * sqrt(stepx * stepy)/vmean;

        if(deltaw > deltap)
                timec = 4.;
        else
                timec = 1.25;

       fprintf (stderr, "\n");
       G_message ("zmin,zmax %f %f", zmin, zmax);
       G_message ("simean,vmean,chmean,deltap,deltaw %f %f %f %f %f",
           si0, vmean, chmean, deltap, deltaw);
       G_message ("MITER, timec %d %f", miter, timec);

      deltap = amin1(deltap,deltaw);

       if (wdepth != NULL)
           G_message ("sigmax,vmax,deltapused %f %f %f", sigmax, vmax, deltaw);

/*    if (wdepth != NULL) deltap = 0.1;  deltap for sediment is ar. average deltap and deltaw */
/*    if (wdepth != NULL) deltap = (deltaw+deltap)/2.;  deltap for sediment is ar. average deltap and deltaw */

    miter = (int)(timesec / (deltap * timec)); /* number of iterations */
    iterout = (int)(iterout / (deltap * timec)); /* iterations for time series */

/*! For each cell (k,l) compute the length s=(v1,v2) of the path 
 *  that the particle will travel per one time step
 *  \f$ s(k,l)=v(k,l)*dt \f$, [m]=[m/s]*[s]
 *  give warning if there is a cell that will lead to path longer than 2 cells 
 *
 *  if running erosion, compute sediment transport capacity for each cell si(k,l)
 *  \f$
 * T({\bf r})=K_t({\bf r}) \bigl[\tau({\bf r})\bigr]^p
 * =K_t({\bf r}) \bigl[\rho_w\, g h({\bf r}) \sin \beta ({\bf r}) \bigr]^p
 * \f$
 * [kg/ms]=...
*/
    for (k = 0; k < my; k++) {
        for (l = 0; l < mx; l++) {

        if (zz[k][l] != UNDEF) {
            v1[k][l] *= deltap; 
            v2[k][l] *= deltap;

/*      if(v1[k][l]*v1[k][l]+v2[k][l]*v2[k][l] > cellsize, warning, napocitaj
*       ak viac ako 10%a*/

	if (inf!=NULL) inf[k][l] *= timesec; /* THIS IS CORRECT SOLUTION currently commented out*/

	if(wdepth != NULL) gama[k][l] = 0.;

		if(et!=NULL) {
		if(sigma[k][l] == 0. || slope[k][l] == 0.)
			si[k][l] = 0.;
		else
			si[k][l] = si[k][l] / (slope[k][l] * sigma[k][l]); /* temp for transp. cap. erod */
		}
	      } /* DEFined area */
        }
    }

/*! compute transport capacity limted erosion/deposition et 
*   as a divergence of sediment transport capacity
*   \f$
    D_T({\bf r})= \nabla\cdot {\bf T}({\bf r})
*   \f$
*/
	if(et!=NULL) {
          erod(si); /* compute divergence of t.capc */

	 if (output_et() != 1)
	     G_fatal_error (_("Unable to write et file"));
	}

/*! compute the inversion operator and store it in sigma - note that after this
*   sigma does not store the first order reaction coefficient but the operator
*   WRITE the equation here
*/
	if(wdepth != NULL) {
	    for (k = 0; k < my; k++) {
        	for (l = 0; l < mx; l++) {
			if (zz[k][l] != UNDEF) {

		if(et!=NULL) si[k][l] = si[k][l] * slope[k][l] * sigma[k][l]; /* get back from temp */

		if( sigma[k][l] != 0.)
/* rate of weight loss - w=w*sigma , vaha prechadzky po n-krokoch je sigma^n */
		sigma[k][l] = exp(-sigma[k][l] * deltap * slope[k][l]); /* not clear what's here :-\ */
/* if(sigma[k][l]<0.5) warning, napocitaj, ak vacsie ako 50% skonci, zmensi deltap)*/
		}
		} /*DEFined area */
	   }
	}

    return 1;
}


double amax1 (double arg1, double arg2)
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


double amin1 (double arg1, double arg2)
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


int min (int arg1, int arg2)
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


int max (int arg1, int arg2)
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
