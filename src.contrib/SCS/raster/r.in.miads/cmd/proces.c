/* @(#)proces.c	2.0  10/2/87 */
/* purpose:  Program for GIS  MIADS conversion to r.in.ll (Mimport.ll3.0) format
 *         finds:  UTM north, south, east, and west
 *                 rows and columns counts
 *         for Mimport.ll header information.
 *         creates: one Mimport formatted file for each
 *                 strip of MIADS data
 */
#include <ctype.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include "gis.h"
#include "miad.h"

/*#define DEBUG*/

int proces (struct MIAD_INFO *Mhd)
{

	FILE *output_file, *tmp_file, *rpt_file;

/* initialize */
       	   reccnt = recnt = outcnt = swit = 0; cnt = -1;
	   row=col=icode=0;
           str = STRIP; lin = LINE;
	   maxN = maxE = 0; minS = minW = 999999999;

/* init. category file */
	       for (i=0; i<=999; i++)
		    {
		    sprintf (category_file[i],"%s","  ");
		    }

/* open input */
	   if((input_file = open (Mhd->MIADS_MAP_NAME,0)) == -1)
		{
                G_fatal_error ("can't open Miads data file");
		return (1);
		}
/* open report file for writing*/
           sprintf (Output_name, "%s.rept",Mhd->OUTPUT_FILE);
           rpt_file = fopen (Output_name,"w");
	   if (! rpt_file)
		{
	        return (1);
		}
/* ================================================================ */
/* main process */
	    for (record=0;;++record)
	        {
                if((n_read = read(input_file, buffer, 81)) == 0) 
                   {
                   DONE = 1;
                   goto AT_END;
                   }
                if (n_read < 0)
                   {
                   G_fatal_error ("read error in Miads data file");
                   return(1);
                   }
                n_read--;
                buffer[n_read] = '\0';
  /* save latest record */
		sprintf (sav_bufr,"%s",buffer); reccnt++;
#ifdef  DEBUG 
fprintf(stderr," rec# %d\n %s\n",reccnt,sav_bufr);
#endif
   NEXT_STRIP:
                 mvbyt (1, &sav_bufr[0], &DUM[0]);
  /* check for title */
                 if ((strncmp(DUM,"$",1) != 0))
 		    {
  /* strip process */
    /* get the strip and line no. */
                     mvbyt (2, &sav_bufr[74], &STRIP[0]);
                     mvbyt (4, &sav_bufr[76], &LINE[0]);
    /* convert to numbers */
                     strip_no = atoi (str);
                     line_no = atoi (lin);
#ifdef  DEBUG 
fprintf(stderr," lst strip %d,  strip# %d,  line# %d\n",last_strip,strip_no,line_no);
#endif
    /* initialize counters */
                      if (swit == 0)
			  {
                          Mhd->mins=Mhd->maxs=strip_no;
                          Mhd->minl=Mhd->maxl=line_no;
			  Mhd->minc=36;
 			  Mhd->maxc=0;
   /* open a temp file for writing*/
  	                  tempname = G_tempfile() ;
	                  if((tmp_file = fopen (tempname,"a+")) == NULL)
		            {
                            G_fatal_error ("can't open temp data file");
	        	    return (1);
		            }
			  last_strip = strip_no;
                          swit++;
                          }
    /* check strip number, look for change in numbers */
		      if (last_strip != strip_no)
			  {
    /* -------------------------------------------------------------- */
    /* end of strip */
      AT_END :
	       fprintf(stderr," Processing Strip %d\n",last_strip);
      /* put some info. about this strip no. in report file */
  	       sprintf (buffer," Strip %d \n",last_strip);
	       fputs (buffer, rpt_file);      
               sprintf (buffer,"\t Minimum line %d, maximum line %d\n",Mhd->minl,Mhd->maxl);
	       fputs (buffer, rpt_file);
	       Mhd->minc = Mhd->minc++;
	       Mhd->maxc = Mhd->maxc++;
               sprintf (buffer,"\t Minimum cell %d, maximum cell %d\n",Mhd->minc,Mhd->maxc);
	       fputs (buffer, rpt_file);
      /* produce Mimportcell format for the strip */
      /* calculate parameters */
#ifdef  DEBUG 
fprintf(stderr," maxs %d, mins %d\n",Mhd->maxs,Mhd->mins);
fprintf(stderr," maxl %d, minl %d\n",Mhd->maxl,Mhd->minl);
fprintf(stderr," maxc %d, minc %d\n",Mhd->maxc,Mhd->minc);
#endif
               rows = Mhd->maxl - Mhd->minl + 1;
               cols = (Mhd->maxs - Mhd->mins + 1) * 36;
      /* if strip is to left of origin strip  */
               if (last_strip < Mhd->ORIGIN_STRIP )
		  {
		  E_bnd = Mhd->UTM_EAST - Mhd->ORIGIN_CELL*Mhd->CELL_SIZE - ((Mhd->ORIGIN_STRIP-last_strip)-1)*(36*Mhd->CELL_SIZE);
		  W_bnd = E_bnd - 36*Mhd->CELL_SIZE;
		  }
      /* if strip is to right of origin strip  */
	       if (last_strip > Mhd->ORIGIN_STRIP )
		  {
		  W_bnd = Mhd->UTM_EAST + (36-Mhd->ORIGIN_CELL)*Mhd->CELL_SIZE + ((last_strip-Mhd->ORIGIN_STRIP)-1)*(36*Mhd->CELL_SIZE);
		  E_bnd = W_bnd + 36*Mhd->CELL_SIZE;
		  }
      /* if strip is origin strip */
	       if (last_strip == Mhd->ORIGIN_STRIP )
		  {
		  E_bnd = Mhd->UTM_EAST + ((Mhd->maxc - Mhd->ORIGIN_CELL + 1) * Mhd->CELL_SIZE);
/*		  W_bnd = Mhd->UTM_EAST - ((Mhd->ORIGIN_CELL - Mhd->minc) * Mhd->CELL_SIZE); */
		  W_bnd = E_bnd - 36*Mhd->CELL_SIZE;
		  }
      /* whatever the strip postion */
               N_bnd = Mhd->UTM_NORTH + ((Mhd->ORIGIN_LINE - Mhd->minl) * Mhd->CELL_SIZE);
               S_bnd = Mhd->UTM_NORTH - ((Mhd->maxl - Mhd->ORIGIN_LINE) * Mhd->CELL_SIZE);
      /* reset project N, S, E, and W as required */
               if (minS > S_bnd) minS = S_bnd;
               if (maxN < N_bnd) maxN = N_bnd;
	       if (minW > W_bnd) minW = W_bnd;
               if (maxE < E_bnd) maxE = E_bnd;
      /* put N, S, E, W, in report file */
               sprintf (buffer,"\t North: %.2f, South: %.2f\n",N_bnd,S_bnd);
	       fputs (buffer, rpt_file);
               sprintf (buffer,"\t East:  %.2f, West:  %.2f\n",E_bnd,W_bnd);
	       fputs (buffer, rpt_file);
      /* Create a strip name */
       	       sprintf (Output_name,"%s.strip%d",Mhd->OUTPUT_FILE,last_strip);
      /* Open  output file */
	       output_file = fopen (Output_name,"w");
      /* Output the Mimportcell header info. */
               sprintf(buffer,"north: %.2f\nsouth: %.2f\n",N_bnd,S_bnd);
	       fputs (buffer, output_file);
               sprintf(buffer,"east: %.2f\nwest: %.2f\n",E_bnd,W_bnd);
	       fputs (buffer, output_file);
               sprintf(buffer,"rows: %d\ncols: %d\n",rows,cols);
	       fputs (buffer, output_file);

      /* alloc memory for a [rows][cols] int. array*/
	       ptr = (unsigned char *) G_calloc(cols,rows);
#ifdef  DEBUG 
fprintf(stderr,"alloc %d cols, %d rows\n",cols,rows);
#endif

      /* Read tmp_file and process the records */
		rewind (tmp_file);
                for (recrd=0;;++recrd)
		   {
      /* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
  		   if (!fgets (buffer, 81, tmp_file)) break;
		   recnt = recnt++;
		   mvbyt (1, &buffer[0], &DUM[0]);
		   if ((strncmp(DUM,"$",1) != 0))
		      {
		      mvbyt(72, &buffer[0], &Data[0]);
		      mvbyt(2, &buffer[74], &STRIP[0]);
		      mvbyt(4, &buffer[76], &LINE[0]);
		      row = (atoi (lin)) - Mhd->minl;
		      col = ((atoi (str)) - Mhd->mins) * 36;
       /* handle data fields */
#ifdef  DEBUG 
fprintf(stderr,"strip %s, line %s\n",str,lin);
fprintf (stderr,"%s\n",Data);
sleep(1); 
#endif
		      k = 0;
		      while (k <= 71)
			 {
			 mvbyt (2, &Data[k], &Bytes[0]);
#ifdef  DEBUG 
fprintf(stderr," k= %d,  Byte in !%s!  ",k,Bytes);
#endif
		 	 m = col + k / 2;
			 icode = 0;
        /* look for an empty field, skip it */
			 if ((strncmp(Bytes,"  ",2) != 0))
			    {
	/* find input string in category file, assign numeric value of the
		   array element position to memory location */
               		    for (icode=1; icode<=999; icode++)
			       {
	/* compare for match */
	        	       if (strncmp(Bytes,category_file[icode],2) == 0)
				  {
	/* match, assigned already */
		  	          break;
			          }else
	/* no match, is this element blank (unassigned) */
	                  if (strncmp(category_file[icode]," ",1) == 0)
				{
	/* blank, assign input value to this element */
				    sprintf (category_file[icode],"%s",Bytes);
#ifdef  DEBUG 
fprintf(stderr," new code - cat (%d) !%s!\n",icode,category_file[icode]);
#endif
				    cat_cnt++;
				    break;
        /* NOT blank, look at next entry */
				}else
				 continue;
		           }
			 }
        /* assign code value to memory array */
				*(ptr+row*cols+m) = icode;
			        if ( icode != 0) data_cell_cnt++;
#ifdef  DEBUG 
fprintf(stderr," map[%d] = %d \n",row*cols+m,*(ptr+row*cols+m));
sleep(2);
#endif
				cellcnt++;k++;k++;
		      }
		    }else
        /* empty data field, look at next */
	             continue;
        /* end of strip data */
        /* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
		}

        /* output Mimport file from data in memory array */
	      i = 0;
              while ( i <= cellcnt-1)
                  {
	          sprintf(DUM,"%d ",*(ptr+i));
#ifdef  DEBUG 
fprintf (stderr," map[%d] = %s\n",i,DUM);
#endif
                  fputs (DUM,output_file);
	          i++;
                  }
        /* Put some info. about this strip in report file */
              sprintf (buffer,"\t number of data cells %d\n",data_cell_cnt);
	      fputs (buffer, rpt_file);
	      data_cell_cnt=0;
              sprintf (buffer,"\t number of new categories %d\n",cat_cnt-last_cat_cnt-1);
	      fputs (buffer, rpt_file);
   /*  strip complete, clean up */
              fclose (tmp_file);
              fclose(output_file);
              G_free (ptr);
	      cellcnt = swit = 0;
	      last_cat_cnt = cat_cnt-1;
	      sprintf (buffer, "%s", sav_bufr);
              if (DONE) break;
   /* --------------------------------------------------------------- */
	      }
/*  same strip */
/* reset max & mins as required */
                           if (Mhd->mins > strip_no) Mhd->mins = strip_no;
                           if (Mhd->maxs < strip_no) Mhd->maxs = strip_no;
                           if (Mhd->minl > line_no) Mhd->minl = line_no;
                           if (Mhd->maxl < line_no) Mhd->maxl = line_no;
                           if (Mhd->maxl < line_no) Mhd->maxl = line_no;
/* reset max and min cell values as required */
		           k = 0;
		           while (k <= 71)
			     {
			     mvbyt (2, &sav_bufr[k], &Bytes[0]);
   /* look for an empty field, skip it */
			     if ((strncmp(Bytes,"  ",2) != 0))
			        {
				if (Mhd->maxc < k/2 ) Mhd->maxc = k/2;
				if (Mhd->minc > k/2 ) Mhd->minc = k/2;
		                }
			     k++;k++;
			     }
 /* output buffer to tmp_file */
#ifdef  DEBUG 
fprintf(stderr,"%s\n", sav_bufr);
#endif
			   fputs (sav_bufr, tmp_file);
                       }else
			   {
 /* output the Title to the screen */
                           fprintf (stderr,"%s\n",sav_bufr);
 /* put Title in report file */
      		           fputs (sav_bufr, rpt_file);
                	   }
                           continue;
               }
/* =================================================================== */
/* output category file   */
CAT_OUT:
         sprintf (Output_name,"%s.cats",Mhd->OUTPUT_FILE);
	 output_file = fopen (Output_name,"w");

  /* Put in header info. */
	 sprintf (buffer,"# %d categories\n",cat_cnt--);
	 fputs (buffer, output_file);
	 sprintf (buffer, "unknown data set name\n\n0.00 0.00 0.00 0.00\n");
	 fputs (buffer, output_file);
	 sprintf (buffer, "0:no data\n");
	 fputs (buffer, output_file);
         for ( i=1; i<=999; i++)
	    {
	    if ((strncmp(category_file[i],"  ",2) != 0))
		  {
		  sprintf (buffer,"%d:%s\n",i,category_file[i]);
		  fputs (buffer, output_file);
		  }
             } 
 /* all done, good-bye */
         close(input_file);
         fclose(output_file);
  	 fclose(tmp_file);
 /* Put project summary in report file */
         sprintf (buffer,"\t\n\n Project Window :\n");
	 fputs (buffer, rpt_file);
         sprintf (buffer,"\t  north:  %.2f\n",maxN);
	 fputs (buffer, rpt_file);
         sprintf (buffer,"\t  south:  %.2f\n",minS);
	 fputs (buffer, rpt_file);
         sprintf (buffer,"\t  east:   %.2f\n",maxE);
	 fputs (buffer, rpt_file);
         sprintf (buffer,"\t  west:   %.2f\n",minW);
	 fputs (buffer, rpt_file);
         fclose (rpt_file);
  /* Print project summary */
         fprintf (stderr,"\n A full report is available on %s.rept\n",Mhd->OUTPUT_FILE);
	 fprintf (stderr,"\n\n Files created :\n");
	 sprintf (buffer, "ls %s*\n",Mhd->OUTPUT_FILE);
	 system (buffer);
	 return (0);
}

int mvbyt (int tcnt, char *addr1, char *addr2)
{
	int mcnt;
        for (mcnt=0; mcnt<tcnt; ++mcnt){
		*(addr2+mcnt) = *(addr1+mcnt);
		}
	*(addr2+mcnt)='\0';

	return 0;
}
