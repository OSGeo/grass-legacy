/*  @(#)imp_gef.c	1.0  7/18/89  
*  Created by:  R.Glenn, SCS
*/
#include <stdio.h>
#include <ctype.h>
#include <math.h>
#define MAXLINE	80
#define FGET 	if (fgets(buff,MAXLINE,temp) == NULL) strcpy(buff,"-end")

#include "format.h"

imp_gef(gef_info,gef_File,cat_file,asc_dig,dig_att)
	FILE *gef_info, *gef_File;
	char *cat_file ;
	FILE *asc_dig, *dig_att;
{
	char buff[128], buff_start[4], out_buff[128] ;
	char soil[12], type_code[3], number[16];
	float x1, y1, x2, y2, x3, y3,lat, lon;
	double orig_scale, N,S,W,E, distance, X, Y ;
	int i, cat_num, level=0, code_no, n_coors;
	int num_lines, num_labels;
        struct bdig_head head;
	FILE *temp;

	extern	int	new_format ;
	extern	int	gef_format ;
	extern	int	cat_cnt ;

	num_lines = 0 ;
	num_labels = 0 ;
		     /* start with the header info */
        read_info(gef_info,&head);

                                        /* generate asc_dig head */
        sprintf(out_buff,"ORGANIZATION: %s", head.organization);
        fputs(out_buff,asc_dig);
        sprintf(out_buff,"DIGIT DATE:   %s", head.date);
        fputs(out_buff,asc_dig);
        sprintf(out_buff,"DIGIT NAME:   %s", head.your_name);
        fputs(out_buff,asc_dig);
        sprintf(out_buff,"MAP NAME:     %s", head.map_name);
        fputs(out_buff,asc_dig);
        sprintf(out_buff,"MAP DATE:     %s", head.date);
        fputs(out_buff,asc_dig);
        sprintf(out_buff,"MAP SCALE:    %13.2lf\n", head.orig_scale);
        fputs(out_buff,asc_dig);
        sprintf(out_buff,"OTHER INFO:   %s", head.other_info);
        fputs(out_buff,asc_dig);
        sprintf(out_buff,"ZONE:         %d\n", head.plani_zone);
        fputs(out_buff,asc_dig);
        sprintf(out_buff,"WEST EDGE:    %13.2lf\n",head.W);
        fputs(out_buff,asc_dig);
        sprintf(out_buff,"EAST EDGE:    %13.2lf\n",head.E);
        fputs(out_buff,asc_dig);
        sprintf(out_buff,"SOUTH EDGE:   %13.2lf\n",head.S);
        fputs(out_buff,asc_dig);
        sprintf(out_buff,"NORTH EDGE:   %13.2lf\n",head.N);
        fputs(out_buff,asc_dig);
        sprintf(out_buff,"MAP THRESH:   %13.2lf\n", head.map_thresh);
        fputs(out_buff,asc_dig);
        sprintf(out_buff,"VERTI:\n");
        fputs(out_buff,asc_dig);
/*      head.plani_zone = head.plani_zone * -1; */

	temp = gef_File;
	fprintf(stderr,"\n\n\tGraphics processed :      ");

	for(;;)      /* Process lines and text */
 	  {
	  FGET ;			/* read a record */
/*fprintf(stderr,"%s",buff); */
	  
	  if (strncmp(buff,"-line",4) == 0)
             {
	     fprintf(stderr,"\n");
   	     fprintf(stderr,"\n\n\tLabels processed :      "); 
	     level = 2;
	     continue;
	     }

	  if (strncmp(buff,"-end",4) == 0) return(0) ;

	  if (level == 1)     /* line processing */
             {
	     num_lines++;
	     fprintf(stderr,"\b\b\b\b\b%5d",num_lines);
	     if (!gef_format) 
                {             /* old 24 character format */
	        mvbyt(5,&buff[10],&number[0]);
		sscanf(number,"%d",&n_coors);
	        }
	     else
                {             /* new 40 character format */
	        mvbyt(7,&buff[0],&number[0]);
		sscanf(number,"%d",&code_no);
	        mvbyt(5,&buff[35],&number[0]);
		sscanf(number,"%d",&n_coors);
	        }
	     
	     if (n_coors > 2)
		{                        /* generate header */
                if (!gef_format)    /* for areas */
                   sprintf(out_buff,"A%3d\n",n_coors);
                else
		   {   /* gef data could be area or feature */
                   if (code_no == 9000435 ||
		       code_no == 9000436 ||
		       code_no == 9000437 ||
		       code_no == 9000438 )    /* for features */
                                    sprintf(out_buff,"L%3d\n",n_coors);
                   else                        /* for areas */
                      sprintf(out_buff,"A%3d\n",n_coors);
		   }
                fputs(out_buff,asc_dig);
             
                while (n_coors > 0)
                   {
	           FGET ;			/* read a record */
		   sscanf(buff, "%f%f", &x1, &y1);

		    /* convert input to ground coordinates */
                   convert (&x1,&y1,&N,&E,&head);

                                        /* generate coords. */
                   sprintf(out_buff,"%13.2lf%13.2lf\n",N,E);
                   fputs(out_buff,asc_dig);
                   n_coors-=1 ;
                   }  /* end while coord loop */
                }

/* GRASS doesn't like line segments less than map thresh, check */
	     if (n_coors == 2)
		{
	        FGET ;			/* read a record */
		sscanf(buff, "%f%f", &x1, &y1);
	        FGET ;			/* read a record */
		sscanf(buff, "%f%f", &x2, &y2);

		if ((x2 - x1 > 0) && (y2 - y1 > 0))
		     {
		     X = x2 - x1;
		     Y = y2 - y1;
		     x3 = x2 - X/2;
		     y3 = y2 - Y/2;
		     }
		else
		     {
		     X = x1 - x2;
		     Y = y1 - y2;
		     x3 = x1 - X/2;
		     y3 = y1 - Y/2;
		     }
                distance = X * X + Y * Y;

                convert (&x1,&y1,&N,&E,&head);

		                        /* generate header */
                if (!gef_format)    /* for areas */
                   sprintf(out_buff,"A%3d\n",n_coors + 1);
                else
		   {   /* gef data could be area or feature */
                   if (code_no == 9000435 ||
		       code_no == 9000436 ||
		       code_no == 9000437 ||
		       code_no == 9000438 )    /* for features */
                                    sprintf(out_buff,"L%3d\n",n_coors + 1);
                   else                        /* for areas */
                      sprintf(out_buff,"A%3d\n",n_coors + 1);
		   }
                fputs(out_buff,asc_dig);

                                             /* generate coords. orig*/
                sprintf(out_buff,"%13.2lf%13.2lf\n",N,E);
                fputs(out_buff,asc_dig);

		if (sqrt(distance) <= head.map_thresh)
		     {
                     convert (&x3,&y3,&N,&E,&head);
                                             /* generate coords. new*/
                     sprintf(out_buff,"%13.2lf%13.2lf\n",N,E);
                     fputs(out_buff,asc_dig);
		     }
                 convert (&x2,&y2,&N,&E,&head);
                                             /* generate coords. orig*/
                 sprintf(out_buff,"%13.2lf%13.2lf\n",N,E);
                 fputs(out_buff,asc_dig);
                }  /* end for special 2 coord. gef lines */

/* feature data may also be single point data */
	     if (n_coors == 1)
		{
	        FGET ;			/* read a record */
		sscanf(buff, "%f%f", &x1, &y1);

                sprintf(out_buff,"P%3d\n",n_coors);

		    /* convert input to ground coordinates */
                convert (&x1,&y1,&N,&E,&head);

                                        /* generate coords. */
                sprintf(out_buff,"%13.2lf%13.2lf\n",N,E);
                fputs(out_buff,asc_dig);
                }
	  }


	  if (level == 2)  /* text processing */
             {
	     num_labels++;
             fprintf(stderr,"\b\b\b\b\b%5d",num_labels);
	     if (!gef_format) 
                 {             /* old 24 character format */
  	         mvbyt(1,&buff[0],&number[0]);
		 sscanf(number,"%s",type_code);
  	         mvbyt(10,&buff[1],&number[0]);
		 sscanf(number,"%s",soil);
	         mvbyt(7,&buff[13],&number[0]);
		 sscanf(number,"%f",&x1);
	         mvbyt(7,&buff[21],&number[0]);
		 sscanf(number,"%f",&y1);
	         }
             else
                 {             /* new 40 character format */
  	         mvbyt(7,&buff[0],&number[0]);
		 sscanf(number,"%d",&code_no);
  	         mvbyt(9,&buff[7],&number[0]);
		 sscanf(number,"%s",soil);
	         mvbyt(12,&buff[16],&number[0]);
		 sscanf(number,"%f",&x1);
	         mvbyt(12,&buff[28],&number[0]);
		 sscanf(number,"%f",&y1);
	         }

	     if (x1 != 0.0 && y1 != 0.0)
		{
                convert (&x1,&y1,&N,&E,&head);

                name_to_number(cat_file,soil,&cat_num);
	        if (cat_num < 0)
		    {
		    fprintf(stderr,"Error: Category code creation\n");
		    return(-1);
		    }
                                            /* make attr. record */
                if (code_no == 9000330 || !strcmp(type_code,"T"))
                   {                       /* for areas */
                   sprintf(out_buff,"A  %13.2lf %13.2lf     %5d\n",E,N,cat_num);
                   fputs(out_buff,dig_att);
                   }
                else
                   {                       /* for symbols (sites) */
                   sprintf(out_buff,"P  %13.2lf %13.2lf     %5d\n",E,N,cat_num);
                   fputs(out_buff,dig_att);
                   sprintf(out_buff,"P  2\n",n_coors);
                   fputs(out_buff,asc_dig);
                   sprintf(out_buff,"%13.2lf%13.2lf\n",N,E);
                   fputs(out_buff,asc_dig);
                   sprintf(out_buff,"%13.2lf%13.2lf\n",N,E);
                   fputs(out_buff,asc_dig);
                   }
		}
             }  /* end of level2 (text) processing */

	  if (strncmp(buff,"-head",4) == 0) level = 1;

	  }
}

name_to_number(file_name,name,cat)
	char *file_name, *name;
	int *cat;
{
    int ccode=0, search, recd, icode ;
    char buf[1024], cat_name[40] ;
    char *cptr ;
    FILE *cat_file ;

    if ( (cat_file = fopen(file_name, "r")) == NULL)
	   {
	   *(cat) = -1;
	   return ;
	   }
	   
	/* find input string in category file, assign category value to the
		    area_name based on category file record number*/
    search = 1;
    icode = 0;
    rewind (cat_file);
    while ( search )
	{
	for (recd=0;;++recd)
	    {		 /* category file search */
	    if (!fgets (buf, 82, cat_file)) goto at_end;
 	    sscanf (buf,"%d:%s%",&ccode,cat_name);
	    cptr = cat_name;
	/* compare for match */
/*fprintf(stderr,"compare |%s| to |%s|\n",name,cptr);*/
	    if (strcmp(name,cptr) == 0)
	        {       /* match, assigned already */
	        icode = ccode;
/*fprintf(stderr,"match, code= %d\n",icode);*/
	        fclose(cat_file);
	        break;
	        }
	    }         /* end of category search */
at_end:
/*fprintf(stderr,"EOF, icode= %d\n",icode); */
  	if (icode == 0)
	    {      /* no match, add this name */
	    fclose (cat_file);
            cat_file = fopen (file_name,"a");
	    icode = ccode+1;
/*fprintf(stderr,"no match, new code= %d\n",icode); */
	    sprintf (buf,"%d:%s\n\0",icode,name);
	    fputs (buf, cat_file);
	    fclose (cat_file);
	    cat_cnt++;
	    break;
	    }
	search = 0;
	}             /* end while loop */
	*(cat) = icode;
	return;
}

read_info(info,HEAD)
    FILE *info;
    struct bdig_head *HEAD;
{
	char buff[128];
	FILE *temp;

	temp = info;
	FGET ;			/* read a record */
	strcpy(HEAD->organization,buff);
	FGET ;			/* read a record */
	strcpy(HEAD->date,buff);
	FGET ;			/* read a record */
	strcpy(HEAD->your_name,buff);
	FGET ;			/* read a record */
	strcpy(HEAD->map_name,buff);
	FGET ;			/* read a record */
	sscanf(buff,"%E",&HEAD->orig_scale);
	FGET ;			/* read a record */
	strcpy(HEAD->other_info,buff);
	FGET ;			/* read a record */
	sscanf(buff,"%E",&HEAD->N);
	FGET ;			/* read a record */
	sscanf(buff,"%E",&HEAD->S);
	FGET ;			/* read a record */
	sscanf(buff,"%E",&HEAD->E);
	FGET ;			/* read a record */
	sscanf(buff,"%E",&HEAD->W);
	FGET ;			/* read a record */
	sscanf(buff,"%E",&HEAD->map_thresh);
	FGET ;			/* read a record */
	sscanf(buff,"%E",&HEAD->xofset);
	FGET ;			/* read a record */
	sscanf(buff,"%E",&HEAD->yofset);
	FGET ;			/* read a record */
	sscanf(buff,"%d",&HEAD->stzone);
	FGET ;			/* read a record */
	sscanf(buff,"%d",&HEAD->plani_zone);
	FGET ;			/* read a record */
	strcpy(HEAD->in_proj,buff);
	FGET ;			/* read a record */
	strcpy(HEAD->out_proj,buff);
	    
}

convert(x,y,nor,eas,HEAD)
float *x, *y;
double *nor,*eas;
struct bdig_head *HEAD;
{
	int i;
	float lat, lon;
	double NOR, EAS;

		    /* convert input to lat/long */
        if (strncmp(HEAD->in_proj,"ll",2) == 0)
	   {
	   lat = *y;
	   lon = *x;
	   }
        else if (strncmp(HEAD->in_proj,"utm",2) == 0)
	        {
	        EAS = *x;
	        NOR = *y;
                utmll(&lat,&lon,&NOR,&EAS,&HEAD->plani_zone);
	        }
	      else             /* convert stp/tab */
                {
                if (strncmp(HEAD->in_proj,"stp",3) == 0)
	           {
	           EAS = *x;
	           NOR = *y;
	           }
                if (strncmp(HEAD->in_proj,"tab",3) == 0)
	           {
	           EAS = *x * (HEAD->orig_scale/12.) + HEAD->xofset;
                   NOR = *y * (HEAD->orig_scale/12.) + HEAD->yofset;
	           }
                stpll(&HEAD->stzone,&lat,&lon,&NOR,&EAS);
	        }

        if (strncmp(HEAD->out_proj,"ll",2) == 0)
           {
           NOR = lat;
           EAS = lon;
           }
        else  /* convert to required coord. system */
           {
           if (strncmp(HEAD->out_proj,"utm",3) == 0)
                 llutm(&lat,&lon,&NOR,&EAS,&i);
           if (strncmp(HEAD->out_proj,"alb",3) == 0)
                 llalbr(&lat,&lon,&NOR,&EAS);
           if (strncmp(HEAD->out_proj,"stp",3) == 0)
                 llstp(&HEAD->stzone,&lat,&lon,&NOR,&EAS);
           }
       *(nor) = NOR;
       *(eas) = EAS;
/*printf("convert: lon %lf lat %lf   E %lf N %lf\n",lon,lat,EAS,NOR);*/
}
