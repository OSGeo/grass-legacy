/* output.c (simlib), 20.nov.2002, JH */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "gis.h"
#include "site.h"
#include "bitmap.h"
#include "linkm.h"

#include "waterglobs.h"


int output_data(int tt, double ft)

{

   FCELL    *cell6, *cell7, *cell8;
   FCELL    *cell14, *cell15, *cell16;
   int      fd6, fd7, fd8;
   int      fd14, fd15,fd16;
   int      i,iarc,j;
   float    gsmax=0,dismax=0.,gmax=0.,ermax=-1.e+12,ermin=1.e+12;
   double   zx, zy, zd, sinsl;
   char     msg[100];
   struct  Colors colors;
   struct History hist, hist1; /* hist2, hist3, hist4, hist5*/
   char    *depth0=NULL,*disch0=NULL,*err0=NULL;
   char    *conc0=NULL,*flux0=NULL;
   char    *erdep0=NULL,*outwalk0=NULL;
   char *mapst = NULL;
   char *type;
   char buf[256];
   int ndigit;
   FCELL dat1, dat2;
   float a1,a2;
   Site_head walkershead;
   Site *sd;


	ndigit=2;
	if(timesec >= 10) ndigit=3;
	if(timesec >= 100) ndigit=4;
	if(timesec >= 1000) ndigit=5;
        if(timesec >= 10000) ndigit=6;

  if (outwalk != NULL)
  {
      if (tserie != NULL) {
        sprintf(buf,"%s.%.*d",outwalk,ndigit,tt);
	outwalk0 = G_store(buf);
	fdoutwalk = G_fopen_sites_new (outwalk0);
      }
	else 
	fdoutwalk = G_fopen_sites_new (outwalk);

    if (fdoutwalk == NULL)
    {
      sprintf (msg, "Cannot open %s", outwalk);
      G_fatal_error (msg);
    }
    else
    {
    if(NULL == (sd = G_site_new_struct(-1,2,0,1)))
        G_fatal_error("memory allocation failed for site");

      if (tserie != NULL)
      walkershead.name = outwalk0;
	else
      walkershead.name = outwalk;

      walkershead.desc = G_strdup ("output walkers");
      walkershead.desc = (char *) G_malloc (128 * sizeof (char));
      sprintf (walkershead.desc, "output walkers of %s [raster]",
               depth);
      walkershead.time = NULL;
      walkershead.stime = NULL;
      walkershead.labels = NULL;
      walkershead.form = NULL;

      G_site_put_head (fdoutwalk, &walkershead);

	for (i = 0; i < nstack; i++) {
	sd->east = (float)stack[i][1];
	sd->north = (float)stack[i][2];
	sd->fcat = (float)stack[i][3];
      G_site_put(fdoutwalk, sd); 
	}

    }
  }

   if (depth != NULL)
      {
      cell6 = G_allocate_f_raster_buf();
      if (tserie != NULL) {
        sprintf(buf,"%s.%.*d",depth,ndigit,tt);
        depth0 = G_store(buf);
        fd6 = G_open_fp_cell_new (depth0);
	}
	else
      fd6 = G_open_fp_cell_new (depth);
      if (fd6 < 0)
           {
           sprintf (msg, "unable to create raster map %s", depth);
           G_fatal_error (msg);
                  exit(1);
           }
       }

   if (disch != NULL)
      {
      cell7 = G_allocate_f_raster_buf();
      if (tserie != NULL) {
        sprintf(buf,"%s.%.*d",disch,ndigit,tt);
        disch0 = G_store(buf);
      fd7 = G_open_fp_cell_new (disch0);
	}
	else
      fd7 = G_open_fp_cell_new (disch);
      if (fd7 < 0)
           {
           sprintf (msg, "unable to create raster map %s", disch);
           G_fatal_error (msg);
                  exit(1);
           }
       }

   if (err != NULL)
      {
      cell8 = G_allocate_f_raster_buf();
      if (tserie != NULL) {
        sprintf(buf,"%s.%.*d",err,ndigit,tt);
        err0 = G_store(buf);
      fd8 = G_open_fp_cell_new (err0);
	}
	else
      fd8 = G_open_fp_cell_new (err);

      if (fd8 < 0)
           {
           sprintf (msg, "unable to create raster map %s", err);
           G_fatal_error (msg);
                  exit(1);
           }
       }


   if (conc != NULL)
      {
      cell14 = G_allocate_f_raster_buf();
      if (tserie != NULL) {
        sprintf(buf,"%s.%.*d",conc,ndigit,tt);
        conc0 = G_store(buf);
      fd14 = G_open_fp_cell_new (conc0);
        }
        else
      fd14 = G_open_fp_cell_new (conc);

      if (fd14 < 0)
           {
           sprintf (msg, "unable to create raster map %s", conc);
           G_fatal_error (msg);
                  exit(1);
           }
       }

   if (flux != NULL)
      {
      cell15 = G_allocate_f_raster_buf();
      if (tserie != NULL) {
        sprintf(buf,"%s.%.*d",flux,ndigit,tt);
        flux0 = G_store(buf);
      fd15 = G_open_fp_cell_new (flux0);
        }
        else
      fd15 = G_open_fp_cell_new (flux);

      if (fd15 < 0)
           {
           sprintf (msg, "unable to create raster map %s", flux);
           G_fatal_error (msg);
                  exit(1);
           }
       }

   if (erdep != NULL)
      {
      cell16 = G_allocate_f_raster_buf();
      if (tserie != NULL) {
        sprintf(buf,"%s.%.*d",erdep,ndigit,tt);
        erdep0 = G_store(buf);
      fd16 = G_open_fp_cell_new (erdep0);
        }
        else
      fd16 = G_open_fp_cell_new (erdep);

      if (fd16 < 0)
           {
           sprintf (msg, "unable to create raster map %s", erdep);
           G_fatal_error (msg);
                  exit(1);
           }
       }


     if(G_set_window (&cellhd) < 0)
           exit(3);

     if (my != G_window_rows())
       {
       fprintf (stderr, "OOPS: rows changed from %d to %d\n", mx, G_window_rows());
       exit(1);
       }
     if (mx != G_window_cols())
       {
       fprintf (stderr, "OOPS: cols changed from %d to %d\n", my, G_window_cols());
       exit(1);
       }

    for(iarc=0;iarc<my;iarc++)
      {
         i=my-iarc-1;
         if(depth!=NULL)
           {
            for(j=0;j<mx;j++)
              {
		a1 = pow(gama[i][j],3./5.);
                cell6[j]=(FCELL) a1;/* add conv? */
                gmax = amax1(gmax, a1);
               }
               G_put_f_raster_row (fd6, cell6);
            }

          if(disch!=NULL)
            {
             for(j=0;j<mx;j++)
               {
                a2 = step * gama[i][j] * cchez[i][j]; /* cchez incl. sqrt(sinsl) */
   		cell7[j]=(FCELL) a2;/* add conv? */
                dismax = amax1(dismax, a2); 
                }
              G_put_f_raster_row (fd7, cell7);
             }

           if(err!=NULL)
             {
              for(j=0;j<mx;j++)
                {
                  cell8[j]=(FCELL)gammas[i][j];
                  gsmax = amax1(gsmax, gammas[i][j]);/* add conv? */
                 }
               G_put_f_raster_row (fd8, cell8);
              }


           if(conc!=NULL)
             {
              for(j=0;j<mx;j++)
                {
                  cell14[j]=(FCELL)gama[i][j];
            /*      gsmax = amax1(gsmax, gama[i][j]); */
                 }
               G_put_f_raster_row (fd14, cell14);
              }


           if(flux!=NULL)
             {
              for(j=0;j<mx;j++)
                {
		  a2 = gama[i][j] * slope[i][j];
                  cell15[j]=(FCELL) a2;
                  dismax = amax1(dismax, a2);
                 }
               G_put_f_raster_row (fd15, cell15);
              }


           if(erdep!=NULL)
             {
              for(j=0;j<mx;j++)
                {
                  cell16[j]=(FCELL)er[i][j];
                  ermax = amax1(ermax, er[i][j]);
		  ermin = amin1(ermin, er[i][j]);
                 }
               G_put_f_raster_row (fd16, cell16);
              }

	}

                if(depth!=NULL)
          G_close_cell (fd6);
                if(disch!=NULL)
          G_close_cell (fd7);
                if(err!=NULL)
          G_close_cell (fd8);
                if(conc!=NULL)
          G_close_cell (fd14);
                if(flux!=NULL)
          G_close_cell (fd15);
                if(erdep!=NULL)
          G_close_cell (fd16);

        if (depth!=NULL) {

    G_init_colors(&colors);

    dat1 = (FCELL) 0.;
    dat2 = (FCELL) 0.001;
    G_add_f_raster_color_rule(&dat1,255,255,255,&dat2,255,255,0,&colors);
    dat1 = dat2;
    dat2 = (FCELL) 0.05;
    G_add_f_raster_color_rule(&dat1,255,255,0,&dat2,0,255,255,&colors);
    dat1 = dat2;
    dat2 = (FCELL) 0.1;
    G_add_f_raster_color_rule(&dat1,0,255,255,&dat2,0,127,255,&colors);
    dat1 = dat2;
    dat2 = (FCELL) 0.5;
    G_add_f_raster_color_rule(&dat1,0,127,255,&dat2,0,0,255,&colors);
    dat1 = dat2;
    dat2 = (FCELL) gmax;
    G_add_f_raster_color_rule(&dat1,0,0,255,&dat2,0,0,0,&colors);


    if (tserie != NULL) {
	    if ((mapst = G_find_file("fcell", depth0, "")) == NULL)
	    {
	        sprintf(msg, "cannot find file %s", depth0);
	        G_fatal_error(msg);
	    }
	    G_write_colors(depth0, mapst, &colors);
	    G_quantize_fp_map_range(depth0,mapst,0.,(FCELL)gmax,0,(CELL)gmax);
	    G_free_colors(&colors);
	    }
	else {
	    if ((mapst = G_find_file("fcell", depth, "")) == NULL)
	    {
	        sprintf(msg, "cannot find file %s", depth);
	        G_fatal_error(msg);
	    }
	    G_write_colors(depth, mapst, &colors);
	    G_quantize_fp_map_range(depth,mapst,0.,(FCELL)gmax,0,(CELL)gmax);
	    G_free_colors(&colors);
	    }

        }

        if (disch!=NULL) {

    G_init_colors(&colors);

    dat1 = (FCELL) 0.;
    dat2 = (FCELL) 0.0005;
    G_add_f_raster_color_rule(&dat1,255,255,255,&dat2,255,255,0,&colors);
    dat1 = dat2;
    dat2 = (FCELL) 0.005;
    G_add_f_raster_color_rule(&dat1,255,255,0,&dat2,0,255,255,&colors);
    dat1 = dat2;
    dat2 = (FCELL) 0.05;
    G_add_f_raster_color_rule(&dat1,0,255,255,&dat2,0,127,255,&colors);
    dat1 = dat2;
    dat2 = (FCELL) 0.1;
    G_add_f_raster_color_rule(&dat1,0,127,255,&dat2,0,0,255,&colors);
    dat1 = dat2;
    dat2 = (FCELL) dismax;
    G_add_f_raster_color_rule(&dat1,0,0,255,&dat2,0,0,0,&colors);

    if (tserie != NULL) {
	    if ((mapst = G_find_file("cell", disch0, "")) == NULL)
	    {
	        sprintf(msg, "cannot find file %s", disch0);
	        G_fatal_error(msg);
	    }
	    G_write_colors(disch0, mapst, &colors);
	    G_quantize_fp_map_range(disch0,mapst,0.,(FCELL)dismax,0,(CELL)dismax);    
            G_free_colors(&colors);
    }
	else {

	    if ((mapst = G_find_file("cell", disch, "")) == NULL)
	    {
	        sprintf(msg, "cannot find file %s", disch);
	        G_fatal_error(msg);
	    }
	    G_write_colors(disch, mapst, &colors);
	    G_quantize_fp_map_range(disch, mapst,0.,(FCELL)dismax,0,(CELL)dismax);
	    G_free_colors(&colors);
    } 
   }

        if (flux!=NULL) {

    G_init_colors(&colors);

    dat1 = (FCELL) 0.;
    dat2 = (FCELL) 0.001;
    G_add_f_raster_color_rule(&dat1,255,255,255,&dat2,255,255,0,&colors);
    dat1 = dat2;
    dat2 = (FCELL) 0.1;
    G_add_f_raster_color_rule(&dat1,255,255,0,&dat2,255,127,0,&colors);
    dat1 = dat2;
    dat2 = (FCELL) 1.;
    G_add_f_raster_color_rule(&dat1,255,127,0,&dat2,191,127,63,&colors);
    dat1 = dat2;
    dat2 = (FCELL) dismax;
    G_add_f_raster_color_rule(&dat1,191,127,63,&dat2,0,0,0,&colors);


            if ((mapst = G_find_file("cell", flux, "")) == NULL)
            {
                sprintf(msg, "cannot find file %s", flux);
                G_fatal_error(msg);
            }
            G_write_colors(flux, mapst, &colors);
            G_quantize_fp_map_range(flux, mapst,0.,(FCELL)dismax,0,(CELL)dismax);
            G_free_colors(&colors);
   }


        if (erdep!=NULL) {

    G_init_colors(&colors);

    dat1 = (FCELL) ermax;
    dat2 = (FCELL) 0.1;
    G_add_f_raster_color_rule(&dat1,0,0,0,&dat2,0,0,255,&colors);
    dat1 = dat2;
    dat2 = (FCELL) 0.01;
    G_add_f_raster_color_rule(&dat1,0,0,255,&dat2,0,191,191,&colors);
    dat1 = dat2;
    dat2 = (FCELL) 0.0001;
    G_add_f_raster_color_rule(&dat1,0,191,191,&dat2,170,255,255,&colors);
    dat1 = dat2;
    dat2 = (FCELL) 0.;
    G_add_f_raster_color_rule(&dat1,170,255,255,&dat2,255,255,255,&colors);
    dat1 = dat2;
    dat2 = (FCELL) -0.0001;
    G_add_f_raster_color_rule(&dat1,255,255,255,&dat2,255,255,0,&colors);
    dat1 = dat2;
    dat2 = (FCELL) -0.01;
    G_add_f_raster_color_rule(&dat1,255,255,0,&dat2,255,127,0,&colors);
    dat1 = dat2;
    dat2 = (FCELL) -0.1;
    G_add_f_raster_color_rule(&dat1,255,127,0,&dat2,255,0,0,&colors);
    dat1 = dat2;
    dat2 = (FCELL) ermin;
    G_add_f_raster_color_rule(&dat1,255,0,0,&dat2,255,0,255,&colors);

    if (tserie != NULL) {
            if ((mapst = G_find_file("cell", erdep0, "")) == NULL)
            {
                sprintf(msg, "cannot find file %s", erdep0);
                G_fatal_error(msg);
            }
            G_write_colors(erdep0, mapst, &colors);
            G_quantize_fp_map_range(erdep0,mapst,(FCELL)ermin,(FCELL)ermax,(CELL)ermin,(CELL)ermax);
            G_free_colors(&colors);

	    type = "raster";
	    G_short_history (erdep0, type, &hist1);
            sprintf (hist1.edhist[0], "The sediment flux file is %s", flux0);
            hist1.edlinecnt = 1;
            G_write_history (erdep0, &hist1);
    }
        else {

            if ((mapst = G_find_file("cell", erdep, "")) == NULL)
            {
                sprintf(msg, "cannot find file %s", erdep);
                G_fatal_error(msg);
            }
            G_write_colors(erdep, mapst, &colors);
            G_quantize_fp_map_range(erdep, mapst,(FCELL)ermin,(FCELL)ermax,(CELL)ermin,(CELL)ermax);
            G_free_colors(&colors);

            type = "raster";
            G_short_history (erdep, type, &hist1);
            sprintf (hist1.edhist[0], "The sediment flux file is %s", flux);
            hist1.edlinecnt = 1;
            G_write_history (erdep, &hist1);
    }
   }

/* history section */

   if (depth != NULL)
      {
	if (tserie == NULL) {
    mapst = G_find_file ("cell", depth, "");
    if (mapst == NULL)
    {
      fprintf (stderr, "file [%s] not found\n", depth);
      return -1;
    }
    type = "raster";
    G_short_history (depth, type, &hist);
	} else
    G_short_history (depth0, type, &hist);

/*    fprintf (stdout, "\n history initiated\n");
    fflush(stdout);*/

    sprintf(hist.edhist[0],"init.walk=%d, maxwalk=%d, remaining walkers=%d",nwalk,maxwa,nwalka);
    sprintf(hist.edhist[1],"duration (sec.)=%d, time-serie iteration=%d",timesec,tt);

    sprintf(hist.edhist[2],"written walkers=%d, deltap=%f, mean vel.=%f",
             lwwfin, deltap, vmean);

    sprintf(hist.edhist[3], "mean source (si)=%f, metric conversion factor=%f", si0, conv);

      sprintf (hist.datsrc_1, "input files: %s %s %s", elevin,dxin,dyin);
      sprintf (hist.datsrc_2, "input files: %s %s", rain,manin);
    hist.edlinecnt = 4;
    if (tserie!=NULL) G_write_history (depth0, &hist);
	else
    G_write_history (depth, &hist);
  }

   if (disch != NULL)
      {
	if (tserie == NULL) {
    mapst = G_find_file ("cell", disch, "");
    if (mapst == NULL)
    {
      fprintf (stderr, "file [%s] not found\n", disch);
      return -1;
    }
    type = "raster";
    G_short_history (disch, type, &hist);
	} else
    G_short_history (disch0, type, &hist);

/*    fprintf (stdout, "\n history initiated\n");
    fflush(stdout);*/

    sprintf(hist.edhist[0],"init.walk=%d, maxwalk=%d, remaining walkers=%d",nwalk,maxwa,nwalka);
    sprintf(hist.edhist[1],"duration (sec.)=%d, time-serie iteration=%d",timesec,tt);

    sprintf(hist.edhist[2],"written walkers=%d, deltap=%f, mean vel.=%f",
             lwwfin, deltap, vmean);

    sprintf(hist.edhist[3], "mean source (si)=%f, metric conversion factor=%f", si0, conv);

      sprintf (hist.datsrc_1, "input files: %s %s %s", elevin,dxin,dyin);
      sprintf (hist.datsrc_2, "input files: %s %s", rain,manin);
    hist.edlinecnt = 4;
    if (tserie!=NULL) G_write_history (disch0, &hist);
	else
    G_write_history (disch, &hist);
  }

   if (flux != NULL)
      {
        if (tserie == NULL) {
    mapst = G_find_file ("cell", flux, "");
    if (mapst == NULL)
    {
      fprintf (stderr, "file [%s] not found\n", flux);
      return -1;
    }
    type = "raster";
    G_short_history (flux, type, &hist);
	} else
    G_short_history (flux0, type, &hist);

/*    fprintf (stdout, "\n history initiated\n");
    fflush(stdout);*/

    sprintf(hist.edhist[0],"init.walk=%d, maxwalk=%d, remaining walkers=%d",nwalk,maxwa,nwalka);
    sprintf(hist.edhist[1],"duration (sec.)=%d, time-serie iteration=%d",timesec,tt);

    sprintf(hist.edhist[2],"written walkers=%d, deltap=%f, mean vel.=%f",
             lwwfin, deltap, vmean);

    sprintf(hist.edhist[3], "mean source (si)=%f, metric conversion factor=%f", si0, conv);

      sprintf (hist.datsrc_1, "input files: %s %s %s", wdepth,dxin,dyin);
      sprintf (hist.datsrc_2, "input files: %s %s %s %s", manin,detin,tranin,tauin);

    hist.edlinecnt = 4;
    if (tserie!=NULL) G_write_history (flux0, &hist);
	else
    G_write_history (flux, &hist);
  }




   return 1;
}


int output_et()

{

   FCELL    *cell13,*cell17;
   int      fd13,fd17;
   int      i,iarc,j;
   float    etmax=-1.e+12,etmin=1.e+12;
   float    trc;
   char     msg[100];
   struct  Colors colors;
   char *mapst = NULL;
/*   char buf[256]; */
   FCELL dat1, dat2;
/*   float a1,a2; */



   if (et != NULL)
      {
      cell17 = G_allocate_f_raster_buf();
/*      if (tserie != NULL) {
        sprintf(buf,"%s.%.*d",et,ndigit,tt);
        et0 = G_store(buf);
      fd17 = G_open_fp_cell_new (et0);
        }
        else*/
      fd17 = G_open_fp_cell_new (et);

      if (fd17 < 0)
           {
           sprintf (msg, "unable to create raster map %s", et);
           G_fatal_error (msg);
                  exit(1);
           }
       }

   if (tc != NULL)
      {
      cell13 = G_allocate_f_raster_buf();
   /*   if (tserie != NULL) {
        sprintf(buf,"%s.%.*d",tc,ndigit,tt);
        tc0 = G_store(buf);
      fd13 = G_open_fp_cell_new (tc0);
        }
        else*/
      fd13 = G_open_fp_cell_new (tc);

      if (fd13 < 0)
           {
           sprintf (msg, "unable to create raster map %s", tc);
           G_fatal_error (msg);
                  exit(1);
           }
       }


     if(G_set_window (&cellhd) < 0)
           exit(3);

     if (my != G_window_rows())
       {
       fprintf (stderr, "OOPS: rows changed from %d to %d\n", mx, G_window_rows());
       exit(1);
       }
     if (mx != G_window_cols())
       {
       fprintf (stderr, "OOPS: cols changed from %d to %d\n", my, G_window_cols());
       exit(1);
       }

    for(iarc=0;iarc<my;iarc++)
      {
         i=my-iarc-1;
         if(et!=NULL)
           {
            for(j=0;j<mx;j++)
              {
                cell17[j]=(FCELL) er[i][j];/* add conv? */
                etmax = amax1(etmax, er[i][j]);
                etmin = amin1(etmin, er[i][j]);
               }
               G_put_f_raster_row (fd17, cell17);
            }

           if(tc!=NULL)
             {
              for(j=0;j<mx;j++)
                {
		if (sigma[i][j] == 0.) trc = 0.;
			else
			trc = si[i][j] / sigma[i][j];
                  cell13[j]=(FCELL) trc;
                /*  gsmax = amax1(gsmax, trc); */
                 }
               G_put_f_raster_row (fd13, cell13);
              }
	}


                if(tc!=NULL)
          G_close_cell (fd13);

                if(et!=NULL)
          G_close_cell (fd17);

        if (et!=NULL) {

    G_init_colors(&colors);

    dat1 = (FCELL) etmax;
    dat2 = (FCELL) 0.1;
    G_add_f_raster_color_rule(&dat1,0,0,0,&dat2,0,0,255,&colors);
    dat1 = dat2;
    dat2 = (FCELL) 0.01;
    G_add_f_raster_color_rule(&dat1,0,0,255,&dat2,0,191,191,&colors);
    dat1 = dat2;
    dat2 = (FCELL) 0.0001;
    G_add_f_raster_color_rule(&dat1,0,191,191,&dat2,170,255,255,&colors);
    dat1 = dat2;
    dat2 = (FCELL) 0.;
    G_add_f_raster_color_rule(&dat1,170,255,255,&dat2,255,255,255,&colors);
    dat1 = dat2;
    dat2 = (FCELL) -0.0001;
    G_add_f_raster_color_rule(&dat1,255,255,255,&dat2,255,255,0,&colors);
    dat1 = dat2;
    dat2 = (FCELL) -0.01;
    G_add_f_raster_color_rule(&dat1,255,255,0,&dat2,255,127,0,&colors);
    dat1 = dat2;
    dat2 = (FCELL) -0.1;
    G_add_f_raster_color_rule(&dat1,255,127,0,&dat2,255,0,0,&colors);
    dat1 = dat2;
    dat2 = (FCELL) etmin;
    G_add_f_raster_color_rule(&dat1,255,0,0,&dat2,255,0,255,&colors);

/*    if (tserie != NULL) {
            if ((mapst = G_find_file("cell", et0, "")) == NULL)
            {
                sprintf(msg, "cannot find file %s", et0);
                G_fatal_error(msg);
            }
            G_write_colors(et0, mapst, &colors);
            G_quantize_fp_map_range(et0,mapst,(FCELL)etmin,(FCELL)etmax,(CELL)etmin,(CELL)etmax);
            G_free_colors(&colors);
    }
        else { */

            if ((mapst = G_find_file("cell", et, "")) == NULL)
            {
                sprintf(msg, "cannot find file %s", et);
                G_fatal_error(msg);
            }
            G_write_colors(et, mapst, &colors);
            G_quantize_fp_map_range(et, mapst,(FCELL)etmin,(FCELL)etmax,(CELL)etmin,(CELL)etmax);
            G_free_colors(&colors);
  /*  } */
   }


   return 1;
}

