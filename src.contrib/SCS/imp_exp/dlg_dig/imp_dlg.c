/*  @(#)imp_dlg.c	2.1  6/26/87  
*  Created by:  CERL, original in mapdev/dlg_to_bdlg
*  modified:    added category code insertion 
*                                by Paul Carlson, SCS, 10-10-87 
*  modified:    refined error output warnings
*                                by R.Glenn, SCS  11-21-87       
*  modified:    moved from mapdev/dlg_to_bdlg to importdlg routines,
*               included ground coordinate conversion
*                                by R.Glenn, SCS  12-1-87      
*  modified:    added check for AREA coord. record, a la INTERGRAPH DLG
* 				 by R.Glenn, SCS  03-05-88
*  modified:    added universal polygon flag, rev3.1 dlg export
* 				 by R.Glenn, SCS  03-05-88
*/
#include <stdio.h>
#include <ctype.h>
#define COOR_MAX		4096
#define MAXLINE		90
#define FGET 		if (fgets(buff,MAXLINE,dlg) == NULL) strcpy(buff,"E")

/*
 * imp_dlg  reads the body of a dlg file in "optional" ascii format
 *    and writes it bdlg in CERL binary format.   This binary form
 *    is exactly equivalent to the "optional" format.
 */

#include "dlghead.h"
#include "format.h"

/* -------------------------------------- cats_fd added   PWC --- */
imp_dlg(dlg, bin, cats_fd,univ)
	FILE *dlg ;
	FILE *bin ;
	FILE *cats_fd;
	int univ;
{
	char buff[128] ;
/* --------------------------------- coeff params. added   RLG --- */
	double A1, A2, A3, A4 ;     

	double x, y, N, S, E, W ;
	int ii, n_lines, n_atts, n_isles ;
	int start_node, end_node, last_line ;
	int left_area, right_area ;
	int n_coors, num, n_read ;
	int line_buf[COOR_MAX] ;
	int att_buff[COOR_MAX] ;
	double coor_buff[COOR_MAX * 2] ;
	int num_nodes, num_areas, num_lines, num_undef ;

	extern	int	new_format ;
/*--------------------------------------------------------PWC---*/
	char *ptr;
	int last_case, area_num, add_cats, cat_code;

	add_cats = (cats_fd != NULL);
/*----------------------------------------- 12-1-87 ------RLG---*/
/*   Added following pointers to convert import coord. values to
         true ground coord. values as per given parameters in DLG header.  */
	A1 = *(coeff_params);
	A2 = *(coeff_params + 1);
	A3 = *(coeff_params + 2);
	A4 = *(coeff_params + 3);
/*--------------------------------------------------PWC-&-RLG---*/


	num_nodes = 0 ;
	num_areas = 0 ;
	num_lines = 0 ;
	num_undef = 0 ;

	for(;;)      /* Process nodes (N), areas (A), and lines (L) */
 	  {
	  FGET ;			/* read a record */
/*fprintf(stderr,"%s",buff); sleep(1);*/
	  switch (*buff)
	    {
	    case 'N':
		num_nodes++ ;
		sscanf(buff, "%*1c%5d%14lf%14lf%*6c%6d",
			&num, &x, &y, &n_lines) ;

		if (n_lines)            /* read line links to this node */
			if (n_read = read_int(dlg, n_lines, line_buf) )
			  {
			  fprintf (stdout,"Error: Missing %d lines for node %d\n",
					n_read, num) ;
			  n_lines -= n_read ;
			  }
			
		n_atts = 0 ;
/*-------------------------------------------- 12-1-87 ---RLG----*/
		x = A1 * x + A2 * y + A3;
		y = A1 * y - A2 * x + A4;
/*-------------------------------------------------------RLG----*/
				/* write the binary file */
		fwrite("N",     sizeof(char),   1, bin) ;
		fwrite(&num,    sizeof(num),    1, bin) ;
		fwrite(&x,      sizeof(x),      1, bin) ;
		fwrite(&y,      sizeof(y),      1, bin) ;
		fwrite(&n_lines,sizeof(n_lines),1, bin) ;
		fwrite(&n_atts, sizeof(n_atts), 1, bin) ;
		if (n_lines)
			fwrite (line_buf, sizeof(*line_buf), n_lines, bin) ;
/*-------------------------------------------- 11-21-87 ---RLG---*/
		last_case = 1;
/*--------------------------------------------------------RLG---*/
		break ;

	    case 'A':
/*-------------------------------------------- 11-21-87 ---RLG---*/
		if (last_case == 1)
		   fprintf (stdout,"\nSTATUS:\n\t Finished node processing");
/*--------------------------------------------------------RLG---*/

		num_areas++ ;
		sscanf(buff, "%*1c%5d%14lf%14lf%*6c%6d%6d%6d%*6c%6d",
		   &num, &x, &y, &n_lines, &n_coors, &n_atts, &n_isles);
/*-------------------------------------------- 06-21-88 ---RLG---*/
                
		if (num < 3 && univ == 1)
                   {          /* ignore universal & border areas */
                              /* read line record */
		   n_read = read_int(dlg, n_lines, line_buf);
                   if (num == 1) last_line = line_buf[0];
                              /* read attribute record */
		   if (n_atts) n_read = read_int(dlg, n_atts * 2, att_buff) ;
                   num_areas--;
                   last_case = 2; break;
                   }
/*--------------------------------------------------------RLG---*/
		if (n_lines) /* read line links to this area */
		   {
		   n_read = read_int(dlg, n_lines, line_buf);
		   if (n_read > 0 ) 
		      {
		      fprintf (stdout,"Error: Missing %d lines for area %d\n",
						n_read, num) ;
		      n_lines -= n_read ;
		      }
		   if (n_read < 0 )
		      {
		      n_lines--;
		      n_isles--;
		      }
                   }
/*------------------------------------------ 03/05/88 ----RLG---*/
/*Some systems provide an optional set of area coord. GRASS does not use
  them, so read and  never write */
   		if (n_coors)            /* read area coord. records */
 		        n_read = read_doubles(dlg, n_coors * 2, coor_buff);
/*--------------------------------------------------------RLG---*/
	 	if (n_atts)  /* read attributes for this area */
		   if (n_read = read_int(dlg, n_atts * 2, att_buff) )
		      {
		      fprintf (stdout,
		      "Error: Missing or zero value attributes for area %d\n",
		      num) ;
		      n_atts -= n_read / 2 ;
		      }
/*-------------------------------------------------------PWC----*/
		if (add_cats)           /* if category codes required */
		   {     /* get area number from categories file */

                   rewind (cats_fd);
                         /* read the file, looking for area num */
                   for (;;)
		     {
		     fscanf(cats_fd, "%d", &area_num);
				      /* check for match with dlg area number */
		     if (area_num != num)
		        {
			if (fgets(buff, MAXLINE, cats_fd) == NULL)
		           {
		           fprintf (stdout,"\nERROR - Area ");
		           fprintf (stdout,"%d not found in categories file\n\n",num);
		           fclose(dlg);
		           fclose(bin);
		           fclose(cats_fd);
		           exit(-1);
		           }	          
                         }
                      else break;
		      }
							/* get rest of record */
		   fgets(buff, MAXLINE, cats_fd);
				          /* move past whitespace before name */
		   ptr = buff;
		   while (isspace(*ptr++));
							/* move past next tab */
		   while (*ptr++ != '\t')
  		   if (*ptr == '\0') break;
						      /* if nothing's left... */
		   if (*ptr == '\0')
		      {
		      fprintf (stdout,"\nNo code for area %d ", area_num);
		      fprintf (stdout,"in categories file\n");
		      fclose(dlg);
		      fclose(bin);
		      fclose(cats_fd);
		      exit(-1);
		      }
						   /* this should be the code */
		   cat_code = atoi(ptr);
/*----------------------------------------- 04-29-88 -----RLG----*/
						   /* If other atts exist */
                   if (n_atts > 0)
		      {
                      ii=n_atts;
		      while (ii > 0)
			  {     /* shift other atts to right */
  	                  att_buff[2*ii+1] = att_buff[2*ii-1];
			  att_buff[2*ii] = att_buff[2*ii-2];
			  ii--;
			  }    /* insert GRASS category code */
		      att_buff[0] = 999;
		      att_buff[1] = cat_code;
                      }
                   if (n_atts == 0)
		      {       /* insert GRASS category code */
		      att_buff[2*n_atts] = 999;
		      att_buff[2*n_atts+1] = cat_code;
		      }
		   n_atts++;
/*-------------------------------------------------------RLG----*/
		   }
/*-------------------------------------------------------PWC----*/
/*----------------------------------------- 12-1-87 -----RLG----*/
		   x = A1 * x + A2 * y + A3;
		   y = A1 * y - A2 * x + A4;
/*-------------------------------------------------------RLG----*/
				/* write the binary file */
		   fwrite("A",     sizeof(char),   1, bin) ;
		   fwrite(&num,    sizeof(num),    1, bin) ;
		   fwrite(&x,      sizeof(x),      1, bin) ;
		   fwrite(&y,      sizeof(y),      1, bin) ;
		   fwrite(&n_lines,sizeof(n_lines),1, bin) ;
		   fwrite(&n_atts, sizeof(n_atts), 1, bin) ;
		   fwrite(&n_isles,sizeof(n_isles),1, bin) ;
		   if (n_lines)
			fwrite (line_buf, sizeof(*line_buf), n_lines, bin) ;
		   if (n_atts)
			fwrite (att_buff, sizeof(*att_buff), n_atts * 2, bin) ;
/*-------------------------------------------- 11-21-87 ---RLG---*/
		   last_case = 2;
/*-------------------------------------------------------RLG----*/
		   break ;

	      case 'L':
/*-------------------------------------------- 11-21-87 ---RLG---*/
		   if (last_case == 2)
			   fprintf (stdout,"\n\t Finished area processing");
/*--------------------------------------------------------RLG---*/
		   num_lines++ ;
		   sscanf(buff, "%*1c%5d%6d%6d%6d%6d%*12c%6d%6d",
				&num, &start_node, &end_node,
				&left_area, &right_area, &n_coors, &n_atts ) ;
		   if (n_coors) /* read coord. record(s) */
		      {
		      if (n_read = read_doubles(dlg, n_coors * 2, coor_buff) )
			 {
			 fprintf (stdout,"Error: Missing %d coordinates for line %d\n",
						n_read, num) ;
			 n_coors -= n_read / 2 ;
			 }

		      if (new_format)
				bound_box(coor_buff, n_coors, &N, &S, &E, &W) ;
		      }
		   else
		      {
		      N = 0.0 ;
		      S = 0.0 ;
		      E = 0.0 ;
		      W = 0.0 ;
		      }

		   if (n_atts) /* read attributes for this line */
		   if (n_read = read_int(dlg, n_atts * 2, att_buff) )
		      {
		      fprintf (stdout,"Error: Missing %d attributes for line %d\n",
						n_read, num) ;
		      n_atts -= n_read / 2 ;
		      }

/*-------------------------------------------- 06-21-88 ---RLG---*/
                           /* don't write the univ border */
                   if (last_line == -num)
                     {
                     num_lines--;
                     break;
                     }
/*--------------------------------------------------------RLG---*/
				/* write the binary file */
		   fwrite("L",         sizeof(char),      1, bin) ;
		   fwrite (&num,       sizeof(num),       1, bin) ;
		   fwrite (&start_node,sizeof(start_node),1, bin) ;
		   fwrite (&end_node,  sizeof(end_node),  1, bin) ;
		   fwrite (&left_area, sizeof(left_area), 1, bin) ;
		   fwrite (&right_area,sizeof(right_area),1, bin) ;
		   fwrite (&n_coors,   sizeof(n_coors),   1, bin) ;
		   fwrite (&n_atts,    sizeof(n_atts),    1, bin) ;

		  if (new_format)
		     {
		     fwrite (&N,         sizeof(N),         1, bin) ;
		     fwrite (&S,         sizeof(S),         1, bin) ;
		     fwrite (&E,         sizeof(E),         1, bin) ;
		     fwrite (&W,         sizeof(W),         1, bin) ;
		     }

		  if (n_coors)
			fwrite (coor_buff, sizeof(*coor_buff), n_coors * 2, bin) ;
		  if (n_atts)
			fwrite (att_buff, sizeof(*att_buff), n_atts * 2, bin) ;
/*-------------------------------------------- 11-21-87 ---RLG---*/
		  last_case = 3;
/*--------------------------------------------------------RLG---*/
		  break ;
		
	      case 'E':
/*-------------------------------------------- 11-21-87 ---RLG---*/
		  if (last_case == 3)
			fprintf (stdout,"\n\t Finished line processing\n\nSUMMARY :");
/*--------------------------------------------------------RLG---*/
			fprintf (stdout,"\n\t    nodes: %d\n", num_nodes) ;
			fprintf (stdout,"\t    areas: %d\n", num_areas) ;
			fprintf (stdout,"\t    lines: %d\n", num_lines) ;
			fprintf (stdout,"\t  unknown: %d\n", num_undef) ;
			return(0) ;

	      default:
/*-------------------------------------------- 11-21-87 ---RLG---*/
		  if (last_case == 1)
		     fprintf(stderr," WARNING: There are more than %d line IDs in node %d\n",n_lines,num);
		  if (last_case == 2)
		     fprintf(stderr," WARNING: There are more than %d line IDs in area %d\n",n_lines,num);
		  if (last_case == 3)
		     fprintf(stderr," WARNING: There are more than %d coordinates in line %d\n",n_coors,num);
/*--------------------------------------------------------RLG---*/
		  num_undef++ ;
		  }
	}
}
