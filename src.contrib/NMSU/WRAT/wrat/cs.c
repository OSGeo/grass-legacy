/**************************** cs  **************************************
*  cs is the quality program based on a simplified AGNPS and SWMM.
*  It models the contaminant source areas for a study area.
*
*************************************************************************/


# include "segment.h";

# include "gis.h";

# include "math.h";

# include "wrat.h";

SEGMENT soiltext_seg, landcover_seg, slope_seg, runoff_seg,
        ns_seg, ps_seg, cods_seg, seds_seg, rain_seg,
        Ksoil_seg;


cs() 

{

int lc,slop;


CELL *cell;


	soiltext_file = G_tempfile();
        landcover_file = G_tempfile();
        slope_file = G_tempfile();
        runoff_file = G_tempfile();
        Ksoil_file = G_tempfile();
        rain_file = G_tempfile();
        ns_file = G_tempfile();
        ps_file = G_tempfile();
        cods_file = G_tempfile();
        seds_file = G_tempfile();


	if (G_get_window (&window) < 0)
	   printf ( "can't read current window paramiters!"),
	   exit(1);

	rows = G_window_rows();
	cols = G_window_cols();
        ewres = window.ew_res;
        nsres = window.ns_res;
	cell = G_allocate_cell_buf();



	/*** find mapsets for input maps  ********/

	landcover_mapset=G_find_cell(landcover_map_name,"");
	runoff_mapset=G_find_cell(runoff_map_name,"");
	slope_mapset=G_find_cell(slope_map_name,"");
	Ksoil_mapset=G_find_cell(Ksoil_map_name,"");
	soiltext_mapset=G_find_cell(soiltext_map_name,"");
	if (rmap)
	   rain_mapset=G_find_cell(rain_map_name,"");


/*    open needed map layers    */

      slope_map_fd = G_open_cell_old(slope_map_name, slope_mapset);
	   if (slope_map_fd == 0)
	      printf("unable to open a required map"), exit(0);

      Ksoil_map_fd = G_open_cell_old(Ksoil_map_name, Ksoil_mapset);
	   if (Ksoil_map_fd == 0)
	      printf("unable to open a required map"), exit(0);

      soiltext_map_fd = G_open_cell_old(soiltext_map_name, soiltext_mapset);
	   if (soiltext_map_fd == 0)
	      printf("unable to open a required map"), exit(0);

      landcover_map_fd = G_open_cell_old(landcover_map_name, landcover_mapset);
	   if (landcover_map_fd == 0)
	      printf("unable to open a required map"), exit(0);

      runoff_map_fd = G_open_cell_old(runoff_map_name, runoff_mapset);
	   if (runoff_map_fd == 0)
	      printf("unable to open a required map"), exit(0);

      if(rmap)
          {
printf (" in cs.c rmap= %d\n",rmap);
          rain_map_fd = G_open_cell_old(rain_map_name, rain_mapset);
	  if (rain_map_fd == 0)
	     printf("unable to open a required map"), exit(0);
          }

       if(ns)
          {
	   ns_map_fd = G_open_cell_new(ns_map_name, ns_mapset);
           if (ns_map_fd == 0)
              printf("unable to open new  map."), exit(0);
           ns=1;
          }

        if(ps)
          {
	   ps_map_fd = G_open_cell_new(ps_map_name, ps_mapset);
           if (ps_map_fd == 0)
              printf("unable to open new map."), exit(0);
           ps=1;
           }

	if(cod)
           {
            cods_map_fd = G_open_cell_new(cods_map_name, cods_mapset);
            if (cods_map_fd == 0)
              printf("unable to open new  map."), exit(0);
            cod=1;
           }

	seds_map_fd = G_open_cell_new(seds_map_name, seds_mapset);
           if (seds_map_fd == 0)
              printf("unable to open new  map."), exit(0);


/*     set parameters for segments   */

	len = sizeof(CELL);
	srows = rows/6 + 2;
	scols = cols/6 + 2;


/* create segment files for elevation & drainage direction */

	slope_seg_fd = creat(slope_file, 0666);
	segment_format(slope_seg_fd, rows, cols, srows, scols, len);
	close(slope_seg_fd);

	Ksoil_seg_fd = creat(Ksoil_file, 0666);
	segment_format(Ksoil_seg_fd, rows, cols, srows, scols, len);
	close(Ksoil_seg_fd);

	soiltext_seg_fd = creat(soiltext_file, 0666);
	segment_format(soiltext_seg_fd, rows, cols, srows, scols, len);
	close(soiltext_seg_fd);

        if(rmap)
          {
 	  rain_seg_fd = creat(rain_file, 0666);
	  segment_format(rain_seg_fd, rows, cols, srows, scols, len);
	  close(rain_seg_fd);
          rain_seg_fd = open (rain_file,2);
	  ret = segment_init(&rain_seg, rain_seg_fd,3);
	   if (ret == -1)
              printf("could not initialize segment"), ret=0, exit(0);
	   if (ret == -2)
	      printf("out of memory."), ret=0, exit(0);
          }

	landcover_seg_fd = creat(landcover_file, 0666);
	segment_format(landcover_seg_fd, rows, cols, srows, scols, len);
	close(landcover_seg_fd);

	runoff_seg_fd = creat(runoff_file, 0666);
	segment_format(runoff_seg_fd, rows, cols, srows, scols, len);
	close(runoff_seg_fd);

        if(ns)
          {
   	  ns_seg_fd = creat(ns_file, 0666);
	  segment_format(ns_seg_fd, rows, cols, srows, scols, len);
	  close(ns_seg_fd);
          }
 
        if(ps)
          {
	  ps_seg_fd = creat(ps_file, 0666);
	  segment_format(ps_seg_fd, rows, cols, srows, scols, len);
	  close(ps_seg_fd);
          }

        if(cod)
          {
	  cods_seg_fd = creat(cods_file, 0666);
	  segment_format(cods_seg_fd, rows, cols, srows, scols, len);
	  close(cods_seg_fd);
          }

	seds_seg_fd = creat(seds_file, 0666);
	segment_format(seds_seg_fd, rows, cols, srows, scols, len);
	close(seds_seg_fd);


/*  open iniitialize & segment files  */

      slope_seg_fd = open (slope_file,2);
	ret = segment_init(&slope_seg, slope_seg_fd,3);
	   if (ret == -1)
              printf("could not initialize segment"), ret=0, exit(0);
	   if (ret == -2)
	      printf("out of memory."), ret=0, exit(0);

      Ksoil_seg_fd = open (Ksoil_file,2);
	ret = segment_init(&Ksoil_seg, Ksoil_seg_fd,3);
	   if (ret == -1)
              printf("could not initialize segment"), ret=0, exit(0);
	   if (ret == -2)
	      printf("out of memory."), ret=0, exit(0);

      soiltext_seg_fd = open (soiltext_file,2);
	ret = segment_init(&soiltext_seg, soiltext_seg_fd,3);
	   if (ret == -1)
              printf("could not initialize segment"), ret=0, exit(0);
	   if (ret == -2)
	      printf("out of memory."), ret=0, exit(0);

      if(rmap)
        {
        rain_seg_fd = open (rain_file,2);
	ret = segment_init(&rain_seg, rain_seg_fd,3);
	   if (ret == -1)
              printf("could not initialize segment"), ret=0, exit(0);
	   if (ret == -2)
	      printf("out of memory."), ret=0, exit(0);
        }

      landcover_seg_fd = open (landcover_file,2);
	ret = segment_init(&landcover_seg, landcover_seg_fd,3);
	   if (ret == -1)
              printf("could not initialize segment"), ret=0, exit(0);
	   if (ret == -2)
	      printf("out of memory."), ret=0, exit(0);

      runoff_seg_fd = open (runoff_file,2);
	ret = segment_init(&runoff_seg, runoff_seg_fd,3);
	   if (ret == -1)
              printf("could not initialize segment"), ret=0, exit(0);
	   if (ret == -2)
	      printf("out of memory."), ret=0, exit(0);

      if(ns)
        {
        ns_seg_fd = open (ns_file,2);
	ret = segment_init(&ns_seg, ns_seg_fd,3);
	   if (ret == -1)
              printf("could not initialize segment"), ret=0, exit(0);
	   if (ret == -2)
	      printf("out of memory."), ret=0, exit(0);
        }

      if(ps)
        {
        ps_seg_fd = open (ps_file,2);
	ret = segment_init(&ps_seg, ps_seg_fd,3);
	   if (ret == -1)
              printf("could not initialize segment"), ret=0, exit(0);
	   if (ret == -2)
	      printf("out of memory."), ret=0, exit(0);
        }

      if(cod)
        {
        cods_seg_fd = open (cods_file,2);
	ret = segment_init(&cods_seg, cods_seg_fd,3);
	   if (ret == -1)
              printf("could not initialize segment"), ret=0, exit(0);
	   if (ret == -2)
	      printf("out of memory."), ret=0, exit(0);
        }

      seds_seg_fd = open (seds_file,2);
	ret = segment_init(&seds_seg, seds_seg_fd,3);
	   if (ret == -1)
              printf("could not initialize segment"), ret=0, exit(0);
	   if (ret == -2)
	      printf("out of memory."), ret=0, exit(0);



   /***  read slope file into segment  ************/

	for (r=0;r<rows;r++) 
	    {
	    if (G_get_map_row (slope_map_fd, cell, r) <0)
	       exit(1);
	    segment_put_row(&slope_seg, cell, r);
	    }

   /***  read Ksoil file into segment  ************/

	for (r=0;r<rows;r++) 
	    {
	    if (G_get_map_row (Ksoil_map_fd, cell, r) <0)
	       exit(1);
	    segment_put_row(&Ksoil_seg, cell, r);
	    }

   /***  read runoff file into segment  ************/

	for (r=0;r<rows;r++) 
	    {
	    if (G_get_map_row (runoff_map_fd, cell, r) <0)
	       exit(1);
	    segment_put_row(&runoff_seg, cell, r);
	    }

   /***  if rmap  read rainfall file into segment  ************/
        if(rmap)
          {
	  for (r=0;r<rows;r++) 
	    {
	    if (G_get_map_row (rain_map_fd, cell, r) <0)
	       exit(1);
	    segment_put_row(&rain_seg, cell, r);
	    }
         }

   /***  read soil texture file into segment  ************/

	for (r=0;r<rows;r++) 
	    {
	    if (G_get_map_row (soiltext_map_fd, cell, r) <0)
	       exit(1);
	    segment_put_row(&soiltext_seg, cell, r);
	    }

   /***  read landcover file into segment  ************/

	for (r=0;r<rows;r++) 
	    {
	    if (G_get_map_row (landcover_map_fd, cell, r) <0)
	       exit(1);
	    segment_put_row(&landcover_seg, cell, r);
	    }



/********** call the functions that do the work  ***********/


for(r=1;r<rows;r++)
   {
   for(c=1;c<cols;c++)
      {
      sedsource();
      if(ns)
         nsource();
      if(ps)
         psource();
      if(cod)
         codsource();
      }
   fprintf(stderr,"processing row %d of %d \r",r,rows-2);
   fflush(stderr);

   }
   fprintf(stderr,"\n");
/**********************  write to cell file  ********************/


     if(ns)
        {
	segment_flush(&ns_seg);
	for (r=0;r<rows;r++)
	   {
	   segment_get_row(&ns_seg, cell, r);
	   if(G_put_map_row(ns_map_fd, cell, r)<0)
	      exit(1);
           }
        }

      if(ps)
        {
	segment_flush(&ps_seg);
	for (r=0;r<rows;r++)
	   {
	   segment_get_row(&ps_seg, cell, r);
	   if(G_put_map_row(ps_map_fd, cell, r)<0)
	      exit(1);
           }
        }

     if(cod)
        {
	segment_flush(&cods_seg);
	for (r=0;r<rows;r++)
	   {
	   segment_get_row(&cods_seg, cell, r);
	   if(G_put_map_row(cods_map_fd, cell, r)<0)
	      exit(1);
           }
        }

	segment_flush(&seds_seg);
	for (r=0;r<rows;r++)
	   {
	   segment_get_row(&seds_seg, cell, r);
	   if(G_put_map_row(seds_map_fd, cell, r)<0)
	      exit(1);
           }




/*************  release segments, close all files  ********************/

	segment_release(&slope_seg);
	segment_release(&Ksoil_seg);
	segment_release(&soiltext_seg);
	segment_release(&landcover_seg);
	segment_release(&runoff_seg);
        if(rmap)
	   segment_release(&rain_seg);
	segment_release(&seds_seg);
        if(ns)
	   segment_release(&ns_seg);
        if(ps)
	   segment_release(&ps_seg);
        if(cod)
	   segment_release(&cods_seg);
	

	close (slope_seg_fd);
	close (Ksoil_seg_fd);
	close (soiltext_seg_fd);
	close (landcover_seg_fd);
	close (runoff_seg_fd);
	close (seds_seg_fd);
	if(rmap)
          close (rain_seg_fd);
	if(ns)
          close (ns_seg_fd);
	if(ps)
	   close (ps_seg_fd);
	if(cod)
   	   close (cods_seg_fd);


	G_close_cell(slope_map_fd);
	G_close_cell(soiltext_map_fd);
	G_close_cell(landcover_map_fd);
        G_close_cell(rain_map_fd);
	G_close_cell(runoff_map_fd);
	G_close_cell(seds_map_fd);
        if(rmap)
	  G_close_cell(rain_map_fd);
        if(ns)
	  G_close_cell(ns_map_fd);
        if(ps)
	  G_close_cell(ps_map_fd);
        if(cod)
	  G_close_cell(cods_map_fd);

        return(1);

}  /***********************  end of main *************************/


/*********************** sedsource ********************************
*
*  sedsource gathers the terms for the universal soil loss equation 
*  or the sed portion of SWMM to produce a map of sediment 
*  originationg in each cell.
*
********************************************************************/

sedsource()
{

int  landcover, sedi=0, ki, raini, ret, runoff;
float ei, ls, cf, k, sed;
double raind;

segment_get(&runoff_seg, &runoff, r,c);
if(runoff > 0)
{
segment_get(&landcover_seg, &landcover, r, c);
if(landcover !=11 && landcover !=12 && landcover != 13 )
  {
  if(rain > 0)
    raind = rain;
  else
    {
    segment_get(&rain_seg, &raini, r, c);
    raind = raini;                        /* convert to inches & doub */
    raind = raind / 100;
    }

  if( landcover != 14)
     {
     segment_get(&Ksoil_seg, &ki, r,c);
     if(ki < 100)        /* k= 100 ie water or wetland */
       {
       k=ki;
         k=k/100;         /* K was stored as 100*K  */
       ret = Ei(&ei);
       ret = Ls(&ls);
       ret = Cf(&cf);
       sed=ei*k*ls*cf*2000;   /* sed in lbs/acre */
       }
    }  /* end landcover if */

  if( landcover == 14)
     {
     sed = drydays * 2.3 * ewres * 3.27 / 100;    /* from SWMM  */
     raind = raind*-4.6;                       /* conv. to double  */
     sed = sed *(1-exp(raind));
     sed = sed * (ewres*nsres /10000)/2.47; /* conv to lbs/a */
     }

  }

if(landcover == 11 || landcover ==12 || landcover == 13 || ki ==100 )
   sed = 0;

sed = sed *.453 * 2.43;                  /* Kg/hectar  */
sedi = sed;                              /* Kg/hectar  */

}                                 /* end if runoff > 0 */
segment_put(&seds_seg, &sedi, r, c);     /* Kg/hectar  */

return(sedi);

} /********************** end sedsource ***************************/


/****************************** EI *************************************
*
*  EI the erosivity index takes the yearly average EI for the area and
*  calculates the storm EI as the fraction of the yearly EI that storm
*  Represents of the yearly average annula precipitation.
*
***********************************************************************/

Ei(eip)
float *eip;
{
int raini;

   if( rmap )
       {
        segment_get(&rain_seg, &raini, r, c);
        rain = raini/100;
       }

   *eip = rei * rain / avrain;

   return (1);

} /*********************** end Ei ***********************************/


/*************************** Ls ********************************
*
*  The length slope factor is found by this function using
*  the formula fron USDA Ag. Handbook 537.  pg 12
*
*****************************************************************/

Ls(lsp)
float *lsp;
{

int     slopei;
float   slope, slopep, m;
double  lengthd;

lengthd = slopelen;
segment_get(&slope_seg, &slopei, r, c);
slope=slopei - 1;                       /* convert from GRASS > deg */


if( slope >= 1)                  /* minimum slope     */
    {
    slope=slope/57.296;         /* convert to radians for sin function */
    slopep = 100*sin(slope);    /* convert from degrees to % */
    } 
else
    {
    slope = .01; 
    slopep = 1;
    }

if(slopep>5)
  m=.5;

if(slopep <= 5 && slopep > 3.5)
  m=.4;

if(slopep <= 3.5 && slopep > 1)
  m=.3;

if(slopep <= 1)
  m=.2;

*lsp = pow(lengthd/72.6,m) * (65.41 * sin(sin(slope)/57.296) + 4.56 * sin(slope) + .065);

return(1);
} /********************** end Ls ********************************/

/*************************  Cf  *********************************
*
*  This function finds the Crop management factor for use inthe 
*  Universal soil loss equation based onm a look up table.
*  Adopted from AGNPS.
*
*****************************************************************/
Cf(cfp)
float *cfp;
{
static float last;
int landcover;

segment_get(&landcover_seg, &landcover, r, c);

switch (landcover)
   {
   case 1:                    /* corn */
     last = .43;
     *cfp = .43;
     break; 

  case 2:                      /* rye */
     last = .43;
     *cfp = .43;
     break; 

  case 3:                      /*  oats  */
     last = .43;
     *cfp = .43;
     break; 

  case 4:                     /* soybeans  */
     last = .43;
     *cfp = .43;
     break; 

  case 5:                     /* hay  */
     last = .03;
     *cfp = .03;
     break; 

  case 6:                     /* grass */
     last = .03;
     *cfp = .03;
     break; 

  case 7:                     /* old field (grass) */
     last = .03;
     *cfp = .03;
     break; 

  case 8:                    /* old field (shrub) */
     last = .02;
     *cfp = .02;
     break; 

  case 9:                    /* pasture  */
     last = .03;
     *cfp = .03;
     break; 

  case 10:                    /* forest */
     last = .02;
     *cfp = .02;
     break; 

  case 11:                   /* wetlands */
     last = 0;
     *cfp = 0;
     break; 

  case 12:                  /* fens  */
     last = 0;
     *cfp = 0;
     break; 

  case 13:                 /* water  */
     last = 0;
     *cfp = 0;
     break; 

  case 14:                /* built up  */
     last = .01;
     *cfp = .01;
     break; 

  case 15:                /* bare/barren */
     last = 1;
     *cfp = 1;
     break; 

  }
  *cfp = last;
return (1);                      /* if a landcover not found  */
                                /* returns the last Cf found */

}  /******************** end Cf ******************************/

/*************************** n source ***************************
*
*  n source identifies the amount of nitrogen nutrient leaving a 
*  given cell and recorcs that in the nsource map.  Both disolved 
*  nitrogen nutrients and nitrogen associated with sediment are
*  accounted for.
*
****************************************************************/

nsource()
{
float ron =0, czn, chn, xkfn1, xkfn2, efi, rain, coeff, rn, ro, efrain,
      por, value1, value2, gnro, tf, sed, nsed, totaln, er;

int   ret, roi, raini, sedi, tex, totalni=0, lc,  roni;

segment_get(&runoff_seg, &roi, r, c);   /* get runoff             */
if(roi > 0)
   {
segment_get(&soiltext_seg, &tex, r, c);
segment_get(&landcover_seg, &lc, r, c);
if( lc != 14 && lc != 13 && lc != 12 && lc!= 11 && tex != 5 )
  {
  if(rmap)
    {
    segment_get(&rain_seg, &raini, r, c); /* get rainfall           */
    rain=raini * .254;                    /* convert to mm & float  */
    }

  ro=roi;                                 /* convert to  float      */
  ro=ro * .254;                           /* convert to mm & float  */

  ret=Por(&por);                          /* find porosity          */

  czn=(.10*5*por)*.0001/por;              /* N available in soil    */
  chn=.0000008;                           /* N in rainfall          */
  xkfn1=.25/10*por;                       /* rate N into soil       */
  xkfn2=.05/10*por;                       /* infiltration for storm */
  efi=rain-(10*por)-ro;                   /* effictive rain mm      */
  coeff=.00001/por;                       /* a porosity factor      */
  rn=.8*rain*.01;                         /* contribution from rain */
  efrain=rain-(10*por);

  value1=exp(-xkfn1*efi);
  value2=exp(-xkfn1*efi-xkfn2*ro);
  ron=.892*((czn-chn)*value1-(czn-chn)*value2)/coeff+rn*ro/efrain;

  if(ron < 0)   /* error check */
     {
      ron = 0;
      printf ("found a ron < 0 and fixed it ar %d  %d  \n",r,c);
     }
 
  gnro = ((ro/10*2.54*nsres*100*ewres*100)/1000)*ron/1000;
  gnro = gnro/(ewres*nsres/10000);                     /* g/hectar */
     
  segment_get(&seds_seg, &sedi, r, c);
  segment_get(&soiltext_seg, &tex, r, c);    /*  Kg/hectar  */
  sed=sedi;
  sed = sed *.9084;                      /* conv to lbs/acre    */

  switch (tex)                           /* find texture correction */
       {
       case (1):                         /* clay soils              */
           tf = 1.15;
           break;
       case (2):                         /* silt soils              */
           tf = 1.00;
           break;
       case (3):                         /* sand soils              */
           tf = .85;
           break;
       case (4):                         /* peat soils              */
           tf = 1.50;
           break;
       }
  if(sed>0)
    {
     er = 7.4 * (1/pow(sed,.2))*tf;      /*  enrichment ratio     */
     nsed = .001*sed*er*.892;            /* lbs N / acre          */
     nsed = nsed * 2.54*453;             /* g/hectar              */  
    }
  totaln = gnro + nsed;                  /* add N in ro & with sed*/
  totalni = totaln;                      /* convert to integer    */

  }   /* end if lc rural */

if (lc==11 || lc==12 ||lc==13 || tex==5)     /* if wetland or water */
    totalni = 0;


if(lc == 14 )                         /* if urban  */
  {
   segment_get(&seds_seg, &sedi, r, c);
   totalni = sedi * nitro;                /* nitro from assump file  */
  }
if( totalni < 0 )
    totalni = 0;
}                                       /* end if roi > 0 */

segment_put(&ns_seg, &totalni, r,c);          /* g/hectar   */

return(totalni);

} /***************** end nsource **********************************/



/**********************  p source *********************************
*
*  P source calculates the soluble phosphorous contribution into
*  the runoff originating in a cell.  It isa based on the CREAMS
*  model with defaults taked from AGNPS.
*
******************************************************************/

psource()
{
int runoff, sedi, tex, totalpi=0, lc, ropi;
float ro, Cb = 2, rop, exk2 = .025, gpro, tf, sed, psed, totalp, er;

segment_get(&runoff_seg, &runoff, r, c);
if(runoff > 0)
{
segment_get(&soiltext_seg, &tex, r, c);
segment_get(&landcover_seg, &lc, r, c);
if(lc!=11 && lc!=12 && lc!=13 &&lc!=14 && tex!=5)   /* if not wetland or water */
  {
  ro = runoff;                             /* convert to  float */
  ro = ro * .254;                          /* convert to mm       */
  rop = Cb * exk2 * ro *.1;                       /* from CREAMS */

  gpro = ((ro/10*2.54*nsres*100*ewres*100)/1000)*rop/1000;

  gpro = gpro/(ewres*nsres/10000);         /* g/hectar        */


  segment_get(&seds_seg, &sedi, r, c);    /* Kg/hectar          */
  sed=sedi;
  sed = sed *.9084;                      /* conv to lbs/acre    */

  switch (tex)                         /* find texture correction */
       {
       case 1:                         /* clay soils              */
           tf = 1.15;
           break;
       case 2:                         /* silt soils              */
           tf = 1.00;
           break;
       case 3:                         /* sand soils              */
           tf = .85;
           break;
       case 4:                         /* peat soils              */
           tf = 1.50;
           break;
       }

  if(sed>0)
     {
     er = 7.4 * (1/pow(sed,.2))*tf;         /*  enrichment ratio    */
     psed = .0005*sed*er*.892;              /* lbs N / acre         */
     psed = psed * 453 * 2.47;             /*  g /hectar          */
     }

  totalp = gpro + psed;                /* add N in ro & with sed  */
  totalpi = totalp;                    /* convert to integer      */
  }


if (lc==11 || lc==12 ||lc==13 || tex==5)     /* if wetland or water */
   totalpi = 0;

if(lc == 14 )                         /* if urban  */
  {
   segment_get(&seds_seg, &sedi, r, c);
   totalpi = sedi * pho;                /*  pho con. P in sed  */
  }


if(totalpi < 0 )
   totalpi = 0;
}                                      /* end if runoff > 0 */

segment_put(&ps_seg, &totalpi, r,c);       /*  g/hectar  */

return (totalpi);

} /*********************** end psource **********************/


/*********************** codsource **************************
*
*  cod source calculates the COD leaving a cell based on 
*  loading factors adopted from AGNPS and NURP (EPA 1983)
*
************************************************************/
codsource()
{
int roi, landcover, gcodi,tex;
float ro, cod, gcodro;

segment_get(&soiltext_seg, &tex, r, c); 
segment_get(&landcover_seg, &landcover, r,c);
segment_get(&runoff_seg, &roi, r, c); 
ro=roi;
ro = (ro/100)*2.54;           /* convert to cm & float  */

switch (landcover)
       {
	case 1:              /* corn */
	   cod = 120;
           break;
	case 2:              /* rye */
	   cod = 120;
           break;
	case 3:              /*  oats  */
	   cod = 120;
           break;
	case 4:              /* soybeans  */
	   cod = 120;
           break;
	case 5:              /* hay  */
	   cod = 50;
           break;
	case 6:              /* grass */
	   cod = 50;
           break;
	case 7:              /* old field (grass) */
	   cod = 50;
           break;
	case 8:              /* old field (shrub) */
	   cod = 60;
           break;
	case 9:              /* pasture  */
	   cod = 50;
           break;
	case 10:             /* forest */
	   cod = 50;
           break;
	case 11:             /* wetlands */
	   cod = 25;
           break;
	case 12:             /* fens  */
	   cod = 25;
           break;
	case 13:             /* water  */
	   cod = 0;
           break;
	case 14:             /* built up  */
	   cod = 290;                       /* from NURP  */
           break;
	case 15:             /* bare/barren */
	   cod = 80;
           break;
	default:
	   cod = 80;
           break;
        } /*** end switch  ***/

if(tex == 5)                  /* if soil map says water */
   cod = 0;

gcodro = (((ro*nsres*100*ewres*100)/1000)*cod)/1000; /* g COD/cell */
gcodro = gcodro/(ewres*nsres/10000);  /* g COD/hectare */

gcodi = gcodro;                    /* convert to integer */

segment_put(&cods_seg, &gcodi, r, c);   /*   g/hectar   */
return(gcodi);

} /*********************end codsource ************************/

/*********************** por *********************************
*
*   Por finds the porosity of a soil based on the bulkdensity
*   which is infered from the soil texture.
*
*************************************************************/
Por(point)
float *point;
{
int soil;

segment_get(&soiltext_seg, &soil, r, c);

switch (soil)
     {
     case 1:
         *point = 1.4;
         return (1);       /* clay      */
         break;

     case 2:
         *point = 1.3;
         return (1);       /* silt loam */
         break;

     case 3:
         *point = 1.6;
         return (1);       /* sandy loam */
         break;

     default:
         *point = 1.45;
         return (1);     /*  middle value */
         break;
     }
}  /******************** end por **************************/

