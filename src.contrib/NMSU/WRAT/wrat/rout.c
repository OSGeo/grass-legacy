/************************* rout.c  ******************************
*   
*  This program routs contaminants through a watershed
*
******************************************************************/

# include "segment.h";

# include "gis.h";

# include "wrat.h";


SEGMENT  dir_seg, da_seg, seds_seg, ns_seg, ps_seg, cods_seg,
         bmps_seg, bmpn_seg, bmpp_seg, bmpc_seg,
         sedf_seg, nf_seg, pf_seg, codf_seg;

int  RR, CC, bmpn, bmpp, bmpc;

rout()
{

CELL *cell;

	dir_file = G_tempfile();
	da_file = G_tempfile();


	if (G_get_window (&window) < 0)
	   printf ( "can't read current window paramiters!"),
	   exit(1);

	rows = G_window_rows();
	cols = G_window_cols();
        ewres = window.ew_res;
        nsres = window.ns_res;
	cell = G_allocate_cell_buf();

             /* find mapsets */


     da_mapset = G_find_file("cell", da_map_name,"");
     dir_mapset = G_find_file("cell", dir_map_name,"");
     if(seds)
        {
         seds_mapset = G_find_file("cell", seds_map_name,"");
         sedf_mapset = G_mapset();
        }
     if(ns)
        {
         ns_mapset = G_find_file("cell", ns_map_name,"");
         nf_mapset = G_mapset();
        }
     if(ps)
        {
         ps_mapset = G_find_file("cell", ps_map_name,"");
         pf_mapset = G_mapset();
        }
     if(cods)
        {
         cods_mapset = G_find_file("cell", cods_map_name,"");
         codf_mapset = G_mapset();
        }
     if(bmps)
        {
         bmps_mapset = G_find_file("cell", bmps_map_name,"");
        }
     if(bmpn)
        {
         bmpn_mapset = G_find_file("cell", bmpn_map_name,"");
        }
     if(bmpp)
        {
         bmpp_mapset = G_find_file("cell", bmpp_map_name,"");
        }
     if(bmpc)
        {
         bmpc_mapset = G_find_file("cell", bmpc_map_name,"");
        }
        


        /*    open needed map layers    */

	dir_map_fd = G_open_cell_old(dir_map_name, dir_mapset);
        if (dir_map_fd == 0)
            printf("unable to open drainage direction map."), exit(0);

	da_map_fd = G_open_cell_old(da_map_name, da_mapset);
        if (da_map_fd == 0)
            printf("unable to open drainage accumulation map."), exit(0);

        if(seds)
           {
	    seds_map_fd = G_open_cell_old(seds_map_name, seds_mapset);
            if (seds_map_fd == 0)
               printf("unable to open sediment map."), exit(0);
	    sedf_map_fd = G_open_cell_new(sedf_map_name, sedf_mapset);
            if (sedf_map_fd == 0)
               printf("unable to open sed. routing map."), exit(0);
           }

        if(ns)
          {
	  ns_map_fd = G_open_cell_old(ns_map_name, ns_mapset);
          if (ns_map_fd == 0)
               printf("unable to open nitrogen source map."), exit(0);
	  nf_map_fd = G_open_cell_new(nf_map_name, nf_mapset);
          if (nf_map_fd == 0)
              printf("unable to open nitrogen routing map."), exit(0);
          }

        if(ps)
          {
	  ps_map_fd = G_open_cell_old(ps_map_name, ps_mapset);
          if (ps_map_fd == 0)
               printf("unable to open phosphorous source map."), exit(0);
	  pf_map_fd = G_open_cell_new(pf_map_name, pf_mapset);
          if (pf_map_fd == 0)
              printf("unable to open phosphorous routing map."), exit(0);
          }

        if(cods)
          {
	  cods_map_fd = G_open_cell_old(cods_map_name, cods_mapset);
          if (cods_map_fd == 0)
               printf("unable to open COD source map."), exit(0);
	  codf_map_fd = G_open_cell_new(codf_map_name, codf_mapset);
          if (codf_map_fd == 0)
              printf("unable to open COD routing map."), exit(0);
          }

        if(bmps)
          {
	  bmps_map_fd = G_open_cell_old(bmps_map_name, bmps_mapset);
          if (bmps_map_fd == 0)
               printf("unable to open sediment BMP map."), exit(0);
          }

        if(bmpn)
          {
	  bmpn_map_fd = G_open_cell_old(bmpn_map_name, bmpn_mapset);
          if (bmpn_map_fd == 0)
               printf("unable to open nitrogen BMP map."), exit(0);
          }

        if(bmpp)
          {
	  bmpp_map_fd = G_open_cell_old(bmpp_map_name, bmpp_mapset);
          if (bmpp_map_fd == 0)
               printf("unable to open phosphorous BMP map."), exit(0);
          }

        if(bmpc)
          {
	  bmpc_map_fd = G_open_cell_old(bmpc_map_name, bmpc_mapset);
          if (bmpc_map_fd == 0)
               printf("unable to open COD BMP map."), exit(0);
          }


/*     set parameters for segments   */
	len = sizeof(CELL);
	srows = rows/5 + 2;
	scols = cols/5 + 2;


/* create segment files */

	dir_seg_fd = creat(dir_file, 0666);
	segment_format(dir_seg_fd, rows, cols, srows, scols, len);
	close(dir_seg_fd);

	da_seg_fd = creat(da_file, 0666);
	segment_format(da_seg_fd, rows, cols, srows, scols, len);
	close(da_seg_fd);

        if(seds)
           {
   	    seds_file = G_tempfile();
	    sedf_file = G_tempfile();
	    seds_seg_fd = creat(seds_file, 0666);
	    segment_format(seds_seg_fd, rows, cols, srows, scols, len);
	    close(seds_seg_fd);
	    sedf_seg_fd = creat(sedf_file, 0666);
	    segment_format(sedf_seg_fd, rows, cols, srows, scols, len);
	    close(sedf_seg_fd);
           }

	if(ns)
           {
	   ns_file = G_tempfile();
	   nf_file = G_tempfile();
           ns_seg_fd = creat(ns_file, 0666);
	   segment_format(ns_seg_fd, rows, cols, srows, scols, len);
	   close(ns_seg_fd);
           nf_seg_fd = creat(nf_file, 0666);
	   segment_format(nf_seg_fd, rows, cols, srows, scols, len);
	   close(nf_seg_fd);
           }

	if(ps)
           {
	   ps_file = G_tempfile();
	   pf_file = G_tempfile();
           ps_seg_fd = creat(ps_file, 0666);
	   segment_format(ps_seg_fd, rows, cols, srows, scols, len);
	   close(ps_seg_fd);
           pf_seg_fd = creat(pf_file, 0666);
	   segment_format(pf_seg_fd, rows, cols, srows, scols, len);
	   close(pf_seg_fd);
           }


	if(cods)
           {
	   cods_file = G_tempfile();
	   codf_file = G_tempfile();
           cods_seg_fd = creat(cods_file, 0666);
	   segment_format(cods_seg_fd, rows, cols, srows, scols, len);
	   close(cods_seg_fd);
           codf_seg_fd = creat(codf_file, 0666);
	   segment_format(codf_seg_fd, rows, cols, srows, scols, len);
	   close(codf_seg_fd);
           }

	if(bmps)
           {
	   bmps_file = G_tempfile();
           bmps_seg_fd = creat(bmps_file, 0666);
	   segment_format(bmps_seg_fd, rows, cols, srows, scols, len);
	   close(bmps_seg_fd);
           }

	if(bmpn)
           {
	   bmpn_file = G_tempfile();
           bmpn_seg_fd = creat(bmpn_file, 0666);
	   segment_format(bmpn_seg_fd, rows, cols, srows, scols, len);
	   close(bmpn_seg_fd);
           }

	if(bmpp)
           {
	   bmpp_file = G_tempfile();
           bmpp_seg_fd = creat(bmpp_file, 0666);
	   segment_format(bmpp_seg_fd, rows, cols, srows, scols, len);
	   close(bmpp_seg_fd);
           }

	if(bmpc)
           {
	   bmpc_file = G_tempfile();
           bmpc_seg_fd = creat(bmpc_file, 0666);
	   segment_format(bmpc_seg_fd, rows, cols, srows, scols, len);
	   close(bmpc_seg_fd);
           }


/*  open iniitialize & segment files  */

      dir_seg_fd = open (dir_file,2);
	ret = segment_init(&dir_seg, dir_seg_fd,3);
	   if (ret == -1)
              printf("could not initialize dir_seg"), ret=0, exit(0);
	   if (ret == -2)
	      printf("out of memory."), ret=0, exit(0);

      da_seg_fd = open (da_file,2);
	ret = segment_init(&da_seg, da_seg_fd,3);
	   if (ret == -1)
              printf("could not initialize da_seg"), ret=0, exit(0);
	   if (ret == -2)
	      printf("out of memory."), ret=0, exit(0);
        
        if(seds)
           {
            seds_seg_fd = open (seds_file,2);
	    ret = segment_init(&seds_seg, seds_seg_fd,3);
	    if (ret == -1)
                printf("could not initialize seds_seg"), ret=0, exit(0);
	    if (ret == -2)
	        printf("out of memory."), ret=0, exit(0);
            sedf_seg_fd = open (sedf_file,2);

	    ret = segment_init(&sedf_seg, sedf_seg_fd,3);
	    if (ret == -1)
                printf("could not initialize sedf seg"), ret=0, exit(0);
	    if (ret == -2)
	        printf("out of memory."), ret=0, exit(0);
           }

        if(ns)
           {
           ns_seg_fd = open (ns_file,2);
   	   ret = segment_init(&ns_seg, ns_seg_fd,3);
	   if (ret == -1)
              printf("could not initialize ns_seg"), ret=0, exit(0);
	   if (ret == -2)
	      printf("out of memory."), ret=0, exit(0);

           nf_seg_fd = open (nf_file,2);
  	   ret = segment_init(&nf_seg, nf_seg_fd,3);
	   if (ret == -1)
              printf("could not initialize nf_ seg"), ret=0, exit(0);
	   if (ret == -2)
	      printf("out of memory."), ret=0, exit(0);
           }

        if(ps)
           {
           ps_seg_fd = open (ps_file,2);
   	   ret = segment_init(&ps_seg, ps_seg_fd,3);
	   if (ret == -1)
              printf("could not initialize ps_seg"), ret=0, exit(0);
	   if (ret == -2)
	      printf("out of memory."), ret=0, exit(0);

           pf_seg_fd = open (pf_file,2);
  	   ret = segment_init(&pf_seg, pf_seg_fd,3);
	   if (ret == -1)
              printf("could not initialize pf_ seg"), ret=0, exit(0);
	   if (ret == -2)
	      printf("out of memory."), ret=0, exit(0);
           }

        if(cods)
           {
           cods_seg_fd = open (cods_file,2);
   	   ret = segment_init(&cods_seg, cods_seg_fd,3);
	   if (ret == -1)
              printf("could not initialize cods_seg"), ret=0, exit(0);
	   if (ret == -2)
	      printf("out of memory."), ret=0, exit(0);

           codf_seg_fd = open (codf_file,2);
  	   ret = segment_init(&codf_seg, codf_seg_fd,3);
	   if (ret == -1)
              printf("could not initialize codf_ seg"), ret=0, exit(0);
	   if (ret == -2)
	      printf("out of memory."), ret=0, exit(0);
           }

        if(bmps)
           {
           bmps_seg_fd = open (bmps_file,2);
   	   ret = segment_init(&bmps_seg, bmps_seg_fd,3);
	   if (ret == -1)
              printf("could not initialize bmps_seg"), ret=0, exit(0);
	   if (ret == -2)
	      printf("out of memory."), ret=0, exit(0);
           }

        if(bmpn)
           {
           bmpn_seg_fd = open (bmpn_file,2);
   	   ret = segment_init(&bmpn_seg, bmpn_seg_fd,3);
	   if (ret == -1)
              printf("could not initialize bmpn_seg"), ret=0, exit(0);
	   if (ret == -2)
	      printf("out of memory."), ret=0, exit(0);
           }

        if(bmpp)
           {
           bmpp_seg_fd = open (bmpp_file,2);
   	   ret = segment_init(&bmpp_seg, bmpp_seg_fd,3);
	   if (ret == -1)
              printf("could not initialize bmpp_seg"), ret=0, exit(0);
	   if (ret == -2)
	      printf("out of memory."), ret=0, exit(0);
           }

        if(bmpc)
           {
           bmpc_seg_fd = open (bmpc_file,2);
   	   ret = segment_init(&bmpc_seg, bmpc_seg_fd,3);
	   if (ret == -1)
              printf("could not initialize bmpc_seg"), ret=0, exit(0);
	   if (ret == -2)
	      printf("out of memory."), ret=0, exit(0);
           }


/***  read maps into segment  ************/

	for (r=0;r<rows;r++) 
	    {
	    if (G_get_map_row (dir_map_fd, cell, r) <0)
	       exit(1);
	    segment_put_row(&dir_seg, cell, r);
	   
            G_get_map_row (da_map_fd, cell, r); 
	    segment_put_row(&da_seg, cell, r);
	   
            if(seds)
               { 
                G_get_map_row (seds_map_fd, cell, r); 
	        segment_put_row(&seds_seg, cell, r);
               }
	   
            if(ns)
               { 
                G_get_map_row (ns_map_fd, cell, r); 
	        segment_put_row(&ns_seg, cell, r);
               }
	   
            if(ps)
               { 
                G_get_map_row (ps_map_fd, cell, r); 
	        segment_put_row(&ps_seg, cell, r);
               }

            if(cods)
               { 
                G_get_map_row (cods_map_fd, cell, r); 
	        segment_put_row(&cods_seg, cell, r);
               }

            if(bmps)
               { 
                G_get_map_row (bmps_map_fd, cell, r); 
	        segment_put_row(&bmps_seg, cell, r);
               }

            if(bmpn)
               { 
                G_get_map_row (bmpn_map_fd, cell, r); 
	        segment_put_row(&bmpn_seg, cell, r);
               }

            if(bmpc)
               { 
                G_get_map_row (bmpc_map_fd, cell, r); 
	        segment_put_row(&bmpc_seg, cell, r);
               }

	    }

/********** call the functions that do the work  ***********/
        

	gafind();


/***********  write da to cell file  *********************/

	segment_flush(&sedf_seg);

	for (r=0;r<rows;r++)
	   {
           if(seds)
              {
	       segment_get_row(&sedf_seg, cell, r);
	       if(G_put_map_row(sedf_map_fd, cell, r)<0)
	          exit(1);
	      }

           if(ns)
              {
	       segment_get_row(&nf_seg, cell, r);
	       if(G_put_map_row(nf_map_fd, cell, r)<0)
	          exit(1);
	      }

           if(ps)
              {
	       segment_get_row(&pf_seg, cell, r);
	       if(G_put_map_row(pf_map_fd, cell, r)<0)
	          exit(1);
	      }

           if(cods)
              {
	       segment_get_row(&codf_seg, cell, r);
	       if(G_put_map_row(codf_map_fd, cell, r)<0)
	          exit(1);
	      }
          }

/*************  release segments, close all files  ********************/

	segment_release(&dir_seg);
	segment_release(&da_seg);
	segment_release(&seds_seg);
	segment_release(&sedf_seg);
	segment_release(&ns_seg);
	segment_release(&nf_seg);
	segment_release(&ps_seg);
	segment_release(&pf_seg);
	segment_release(&cods_seg);
	segment_release(&cods_seg);

	close (dir_seg_fd);
	close (da_seg_fd);
	close (seds_seg_fd);
	close (sedf_seg_fd);
	close (ns_seg_fd);
	close (nf_seg_fd);
	close (ps_seg_fd);
	close (pf_seg_fd);
	close (cods_seg_fd);
	close (codf_seg_fd);

	G_close_cell(dir_map_fd);
	G_close_cell(da_map_fd);
	G_close_cell(seds_map_fd);
	G_close_cell(sedf_map_fd);
	G_close_cell(ns_map_fd);
	G_close_cell(nf_map_fd);
	G_close_cell(ps_map_fd);
	G_close_cell(pf_map_fd);
	G_close_cell(cods_map_fd);
	G_close_cell(codf_map_fd);


return(0);

}  /************************  end of main *****************************/





/****************************  gafind  *********************************/
/**  gafind() searches the da_map for cells marked 1 in findtops()    **/
/**  from each top dafind flows down hill according to dir_map adding **/
/**  up the drainage accumulation along the way.  Once the path       **/
/**  intersects an exisisting path the tributary area is added to     **/
/**  each cell in the existing "stream" until it exits the map.       **/
/***********************************************************************/


gafind()
{
	int r, c, s, da_value, so, no, po, co;

	  for (RR=1; RR<=rows-2; RR++)
	    {
            fprintf(stderr,"processing row %d out of %d\r",RR,rows-2);
	    fflush(stderr);
	    for (CC=1; CC<=cols-2; CC++)
		{
	        segment_get(&da_seg, &da_value, RR, CC);
		if (da_value == 1)
		   {
		   r=RR, c=CC;
                   so=0, no=0, po=0, co=0;

		   if(seds)
                      sr(r,c, &so);
		   if(ns)
                      nr(r,c, &no);
		   if(ps)
                      pr(r,c, &po);
                   if(cods)
                      codr(r,c,&co);

                   nextrc(&r, &c);

		   while (!cexit(r,c) && !rout_stream(r,c) )
	               {
		        if(seds)
                           sr(r,c, &so);
		        if(ns)
                           nr(r,c, &no);
		        if(ps)
                           pr(r,c, &po);
                        if(cods)
                           codr(r,c, &co);
                        nextrc(&r, &c);
		       }


		  while ( !cexit(r, c) )
		       {
		        if(seds)
                          {
                           segment_get(&sedf_seg, &s, r, c);
                           s=s+so;
                           segment_put(&sedf_seg, &s, r, c);
                          }
		        if(ns)
                          {
                           segment_get(&nf_seg, &s, r, c);
                           s=s+no;
                           segment_put(&nf_seg, &s, r, c);
                          }
		        if(ps)
                          {
                           segment_get(&pf_seg, &s, r, c);
                           s=s+po;
                           segment_put(&pf_seg, &s, r, c);
                          }
                        if(cods)
                          {
                           segment_get(&codf_seg, &s, r, c);
                           s=s+co;
                           segment_put(&codf_seg, &s, r, c);
                          }
                        nextrc(&r, &c);
		       }
		 }  /********** end if da[RR][CC] ==1 *****/
	    }  /***** end for C ********/
     }  /*****  end for R *****/
  fprintf(stderr,"\n");

} /************************ end gfind **************************/


sr(r,c,so)
int r,c, *so;

 {
 int si, soi, bmp = 100;
 float  sif, sof;
 if(bmps)
    segment_get(&bmps_seg, &bmp, r, c);

 segment_get(&seds_seg, &si, r, c);

 soi = *so;
 sof = soi;
 sif = si;
 sif = sif / (ewres * nsres / 10000);    /* conv from kg/hectar to kg */
 sof = (sof + sif) * bmp / 100;
 soi = sof;
 *so = soi;
 segment_put(&sedf_seg, so, r, c); 
 return(1);
 }

nr(r,c,no)
int r,c, *no;
 {
 int ni, noi, bmp = 100;
 float nif, nof;

 if(bmps)
    segment_get(&bmpn_seg, &bmp, r, c);

 segment_get(&ns_seg, &ni, r, c);
 nif = ni;
 noi = *no;
 nof = noi;
 nif = nif * (ewres * nsres / 10000);    /* conv from g/hectar to g */
 nof = (nof + nif) * bmp / 100;
 noi = nof;
 *no = noi;

 segment_put(&nf_seg, no, r, c); 
 return (1);
 }

pr(r,c, po)
int r,c, *po;
 {
 int  pi, poi, bmp = 100;
 float pif, pof;

 if(bmps)
    segment_get(&bmpp_seg, &bmp, r, c);

 segment_get(&ps_seg, &pi, r, c);
 pif = pi;
 poi = *po;
 pof = poi;
 pif = pif * (ewres * nsres / 10000);    /* conv from g/hectar to g */
 pof = (pof + pif) * bmp / 100;
 poi = pof;
 *po = poi;
 segment_put(&pf_seg, po, r, c); 
 return(1);
 }

codr(r,c, co)
int r,c, *co;
 {
 int ci, coi, bmp = 100;
 float cif, cof;

 if(bmps)
    segment_get(&bmpc_seg, &bmp, r, c);

 segment_get(&cods_seg, &ci, r, c);
 cif = ci;
 coi = *co;
 cof = coi;
 cif = cif * (ewres * nsres / 10000);    /* conv from g/hectar to g */
 cof = (cof + cif) * bmp / 100;
 coi = cof;
 *co = coi;
 segment_put(&codf_seg, co, r, c);          /* note because co is a pointer */
 return(1);                                 /* no & before it  */
 }


/***************************** cstream ****************************/
/**   cstream() checks for the intersection with an established  **/
/**   stream from the draining of a previous path                **/
/******************************************************************/

rout_stream(r,c)
int r,c;
{
int out_value;
     if(seds)
        {
	segment_get(&sedf_seg, &out_value, r, c);
        if(out_value > 0 )
           return 1;
        }
     if(ns)
        {
	segment_get(&nf_seg, &out_value, r, c);
        if(out_value > 0 )
           return 1;
        }
     if(ps)
        {
	segment_get(&pf_seg, &out_value, r, c);
        if(out_value > 0 )
           return 1;
        }
     if(cods)
        {
	segment_get(&codf_seg, &out_value, r, c);
        if(out_value > 0 )
           return 1;
        }

 return 0;

}  /*********************   end cstreasm  ****************/



