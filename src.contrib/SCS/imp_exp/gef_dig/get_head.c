/* %W% %G% */
#include <ctype.h>
#include <stdio.h>
#include <string.h>
#include "format.h"

#define METERS_PER_INCH	0.0254

#define TEXT2	\
	"                 SCSGEF IMPORT REFERENCE INFORMATION"
#define TEXT4	\
	"          GRASS requires information NOT available in SCSGEF ."
#define TEXT5	\
	"          Please answer the following questions :"
#define TEXT6	\
	""
#define TEXT7	\
	"          Your Organization : "
#define TEXT8	\
	"          Digitized Date : "
#define TEXT9	\
	"          Map Name : "
#define TEXT10	\
	"          Map Location : "
#define TEXT11	\
        "          Map Scale 1: "
#define TEXT12	\
	"          State FIPS code : "
#define TEXT13  \
	"          County FIPS code : "
#define TEXT14  \
        "          Present GEF Coord. System (table, stplane, ll, utm) : "
#define TEXT15  \
        "          Coord. System Desired (utm, stplane, ll, albers) : "
#define TEXT22	\
	"         HIT INTERRUPT (cntrl-C, RUB, or DEL) TO EXIT FROM GIS, OR"
#define MAXLINE	80
#define FGET 		if (fgets(buff,MAXLINE,gef_file) == NULL) strcpy(buff,"E_")

get_dig_head (gef, reg)
   char *gef, *reg;
{
        char *tpntr;
	char buff[128], buff_start[4], out_buff[128] ;
	char map_name[31], organization[31], date[16], type[30];
	char area[30], coord_sys[30], projection[10];
	char scale[16], *scl;
	char filename[128];
	int  i, ier, stzone ;
	float xdeg, xmin, xsec, ydeg, ymin, ysec;
        float x1, y1, lat, lon;
        double easting, northing;
	FILE *gef_file, *gef_info, *reg_info;
        struct bdig_head dig;
	extern	int	gef_format ;

        G_clear_screen();

        dig.orig_scale = dig.map_thresh = dig.xofset = dig.yofset = 0.0;
	dig.N = dig.S = dig.W = dig.E = 0.0;
	dig.plani_zone = dig.st = dig.cnty = 0;
	dig.in_proj[0] = dig.out_proj[0] = dig.date[0] = '\0';
	dig.map_name[0] = dig.source_date[0] = dig.other_info[0] = '\0';

/* Print warning */
        sprintf(filename,"%s",gef);
	if ( (gef_file = fopen(filename, "r")) == NULL)
	   {
	   fprintf (stdout,"Can't find GEF input file <%s>\n", gef) ;
	   return (-1);
	   }

        sprintf(filename,"%s.info",gef);
	if ( (gef_info = fopen(filename, "w")) == NULL)
	   {
	   fprintf (stdout,"Can't open new %s info file\n", gef) ;
	   return (-1);
	   }

	sprintf(filename,"%s/%s/reg/%s",G_location_path(),G_mapset(),reg);
	if ( (reg_info = fopen(filename, "w")) == NULL)
           {
	   fprintf (stdout,"Can't create reg file  %s\n", reg) ;
	   return (-1) ;
           }

/* read header info */
	FGET ;			/* read a record */
/*fprintf(stderr,"record 1:\n%s",buff); */
                                               /* record 1 */
	if (!gef_format)  /* old format */
           {
  	   mvbyt(30,&buff[0],&map_name[0]);
	   mvbyt(15,&buff[30],&scale[0]);
	   mvbyt(27,&buff[45],&area[0]);
	   }
        else 
	   {
	   mvbyt(30,&buff[0],&map_name[0]);
	   mvbyt(7,&buff[30],&scale[0]);
  	   mvbyt(7,&buff[37],&projection[0]);
	   mvbyt(27,&buff[45],&area[0]);
	   }

			  /* get past a 1: if it exists */
	if (strchr(scale,':'))
	   {
	   sscanf(strchr(scale,':')+1,"%s",buff);
	   tpntr = buff;
	   scl = scale;
	   while(*tpntr != '\0')
	      {
              if (isdigit(*tpntr)) 
	         {
	         *(scl) = *(tpntr);
	         scl++;
	         }
	      tpntr++;
	      }
           *(scl++) = '\0';
	}
	     
			   /* remove 1: from scale if it exists */
	sscanf(scale, "%E", &dig.orig_scale);

        FGET;                              /* record 2 */
/*fprintf(stderr,"record 2:\n%s",buff);*/
	if (!gef_format)  /* old format */
	   {
  	   mvbyt(30,&buff[0],&organization[0]);
	   mvbyt(15,&buff[30],&date[0]);
	   mvbyt(27,&buff[45],&type[0]);
	   }
        else 
	   {
	   mvbyt(30,&buff[0],&organization[0]);
	   mvbyt(8,&buff[30],&date[0]);
	   mvbyt(7,&buff[38],&type[0]);
	   mvbyt(27,&buff[45],&coord_sys[0]);
	   }
	
	dig.map_thresh = .001 * dig.orig_scale;

/* generate some dig head info */
	strcpy(dig.organization,"USDA, SCS");
	strcpy(dig.date,date);
	strcpy(dig.your_name,"SCS GEF Import");
	strcpy(dig.map_name,map_name);
	strcpy(dig.area,area);
	strcpy(dig.source_date,date);
	strcpy(dig.other_info,organization);

        show_info(&dig);
	if (fipsin(&dig.st,&dig.cnty,&dig.stzone) < 0) return(-1);

        FGET;                              /* record 3  SW corner*/
/*fprintf(stderr,"record 3:\n%s",buff);*/
	if (!gef_format)  		/* old format */
  		 sscanf(buff, "%*2s%7f%7f%*9s%3f%2f%2f%*8s%3f%2f%2f", 
			 &x1, &y1, &xdeg, &xmin, &xsec, &ydeg, &ymin, &ysec);
        else              		/* new format */
		 sscanf(buff, "%*2s%12f%12f%*9s%3f%2f%2f%*8s%3f%2f%2f",
			 &x1, &y1, &xdeg, &xmin, &xsec, &ydeg, &ymin, &ysec);

        lon = xdeg + xmin/60 + xsec/3600;
	lat = ydeg + ymin/60 + ysec/3600;

		    /* convert input to lat/long */
        if (strncmp(dig.in_proj,"ll",2) == 0)
	   {
	   lat = y1;
	   lon = x1;
	   }
        else if (strncmp(dig.in_proj,"utm",2) == 0)
	        {
	        dig.W = y1;
	        dig.S = x1;
                utmll(&lat,&lon,&dig.S,&dig.W,&dig.plani_zone);
	        }
	   else             /* convert stp/tab */
              {
              if (strncmp(dig.in_proj,"stp",3) == 0)
	         {
	         dig.W = x1;
	         dig.S = y1;
	         }
              if (strncmp(dig.in_proj,"tab",3) == 0)
	         {
	         llstp(&dig.stzone,&lat,&lon,&dig.S,&dig.W);
	         dig.xofset = dig.W - x1 * (dig.orig_scale/12.);
	         dig.yofset = dig.S - y1 * (dig.orig_scale/12.);
	         }
              stpll(&dig.stzone,&lat,&lon,&dig.S,&dig.W);
	      }

        if (strncmp(dig.out_proj,"ll",2) == 0)
	   {
	   dig.S = lat;
	   dig.W = lon;
	   }
        else  /* convert to required coord. system */
	   {
           if (strncmp(dig.out_proj,"utm",3) == 0)
              llutm(&lat,&lon,&dig.S,&dig.W,&dig.plani_zone);
           if (strncmp(dig.out_proj,"alb",3) == 0)
              llalbr(&lat,&lon,&dig.S,&dig.W);
           if (strncmp(dig.out_proj,"stp",3) == 0)
	      llstp(&dig.stzone,&lat,&lon,&dig.S,&dig.W);
	   }

        sprintf(out_buff,"%14.6lf %15.7lf\n", dig.W,dig.S);
        fputs(out_buff,reg_info);

	FGET;                         /* record 4  NW corner */
/*fprintf(stderr,"record 4:\n%s",buff); */
	if (!gef_format)  		/* old format */
  		 sscanf(buff, "%*2s%7f%7f%*9s%3f%2f%2f%*8s%3f%2f%2f", 
			 &x1, &y1, &xdeg, &xmin, &xsec, &ydeg, &ymin, &ysec);
        else              		/* new format */
		 sscanf(buff, "%*2s%12f%12f%*9s%3f%2f%2f%*8s%3f%2f%2f",
			 &x1, &y1, &xdeg, &xmin, &xsec, &ydeg, &ymin, &ysec);

        lon = xdeg + xmin/60 + xsec/3600;
	lat = ydeg + ymin/60 + ysec/3600;

		    /* convert input to lat/long */
        if (strncmp(dig.in_proj,"ll",2) == 0)
	   {
	   lat = y1;
	   lon = x1;
	   }
        else if (strncmp(dig.in_proj,"utm",2) == 0)
	        {
	        easting = y1;
	        northing = x1;
                utmll(&lat,&lon,&northing,&easting,&dig.plani_zone);
	        }
	   else             /* convert stp/tab */
              {
              if (strncmp(dig.in_proj,"stp",3) == 0)
	         {
	         easting = x1;
	         northing = y1;
	         }
              if (strncmp(dig.in_proj,"tab",3) == 0)
	         {
	         llstp(&dig.stzone,&lat,&lon,&northing,&easting);
	         }
              stpll(&dig.stzone,&lat,&lon,&northing,&easting);
	      }

        if (strncmp(dig.out_proj,"ll",2) == 0)
	   {
	   northing = lat;
	   easting = lon;
	   }
        else  /* convert to required coord. system */
	   {
           if (strncmp(dig.out_proj,"utm",3) == 0)
              llutm(&lat,&lon,&northing,&easting,&i);
           if (strncmp(dig.out_proj,"alb",3) == 0)
              llalbr(&lat,&lon,&northing,&easting);
           if (strncmp(dig.out_proj,"stp",3) == 0)
	      llstp(&dig.stzone,&lat,&lon,&northing,&easting);
	   }

        sprintf(out_buff,"%14.6lf %15.7lf\n", easting,northing);
        fputs(out_buff,reg_info);

        FGET;                              /* record 5  NE corner*/
/*fprintf(stderr,"record 5:\n%s",buff); */
	if (!gef_format)  /* old format */
		 sscanf(buff, "%*2s%7f%7f%*9s%3f%2f%2f%*8s%3f%2f%2f",
			 &x1, &y1, &xdeg, &xmin, &xsec, &ydeg, &ymin, &ysec);
        else     sscanf(buff, "%*2s%12f%12f%*9s%3f%2f%2f%*8s%3f%2f%2f",
			 &x1, &y1, &xdeg, &xmin, &xsec, &ydeg, &ymin, &ysec);
        lon = xdeg + xmin/60 + xsec/3600;
	lat = ydeg + ymin/60 + ysec/3600;

		    /* convert input to lat/long */
        if (strncmp(dig.in_proj,"ll",2) == 0)
	   {
	   lat = y1;
	   lon = x1;
	   }
        else if (strncmp(dig.in_proj,"utm",2) == 0)
	        {
	        dig.E = y1;
	        dig.N = x1;
                utmll(&lat,&lon,&dig.N,&dig.E,&dig.plani_zone);
	        }
	   else             /* convert stp/tab */
              {
              if (strncmp(dig.in_proj,"stp",3) == 0)
	         {
	         dig.E = x1;
	         dig.N = y1;
	         }
              if (strncmp(dig.in_proj,"tab",3) == 0)
	         {
  	         llstp(&dig.stzone,&lat,&lon,&dig.N,&dig.E); 
	         }
              stpll(&dig.stzone,&lat,&lon,&dig.N,&dig.E);
	      }

        if (strncmp(dig.out_proj,"ll",2) == 0)
	   {
	   dig.N = lat;
	   dig.E = lon;
	   }
        else  /* convert to required coord. system */
	   {
           if (strncmp(dig.out_proj,"utm",3) == 0)
              llutm(&lat,&lon,&dig.N,&dig.E,&i);
           if (strncmp(dig.out_proj,"alb",3) == 0)
              llalbr(&lat,&lon,&dig.N,&dig.E);
           if (strncmp(dig.out_proj,"stp",3) == 0)
	      llstp(&dig.stzone,&lat,&lon,&dig.N,&dig.E);
	   }

        sprintf(out_buff,"%14.6lf %15.7lf\n", dig.E,dig.N);
        fputs(out_buff,reg_info);

	FGET;                         /* record 6  SE corner */
/*fprintf(stderr,"record 6:\n%s",buff);*/
	if (!gef_format)  		/* old format */
  		 sscanf(buff, "%*2s%7f%7f%*9s%3f%2f%2f%*8s%3f%2f%2f", 
			 &x1, &y1, &xdeg, &xmin, &xsec, &ydeg, &ymin, &ysec);
        else              		/* new format */
		 sscanf(buff, "%*2s%12f%12f%*9s%3f%2f%2f%*8s%3f%2f%2f",
			 &x1, &y1, &xdeg, &xmin, &xsec, &ydeg, &ymin, &ysec);

        lon = xdeg + xmin/60 + xsec/3600;
	lat = ydeg + ymin/60 + ysec/3600;

		    /* convert input to lat/long */
        if (strncmp(dig.in_proj,"ll",2) == 0)
	   {
	   lat = y1;
	   lon = x1;
	   }
        else if (strncmp(dig.in_proj,"utm",2) == 0)
	        {
	        easting = y1;
	        northing = x1;
                utmll(&lat,&lon,&northing,&easting,&dig.plani_zone);
	        }
	   else             /* convert stp/tab */
              {
              if (strncmp(dig.in_proj,"stp",3) == 0)
	         {
	         easting = x1;
	         northing = y1;
	         }
              if (strncmp(dig.in_proj,"tab",3) == 0)
	         {
	         llstp(&dig.stzone,&lat,&lon,&northing,&easting);
	         }
              stpll(&dig.stzone,&lat,&lon,&northing,&easting);
	      }

        if (strncmp(dig.out_proj,"ll",2) == 0)
	   {
	   northing = lat;
	   easting = lon;
	   }
        else  /* convert to required coord. system */
	   {
           if (strncmp(dig.out_proj,"utm",3) == 0)
              llutm(&lat,&lon,&northing,&easting,&i);
           if (strncmp(dig.out_proj,"alb",3) == 0)
              llalbr(&lat,&lon,&northing,&easting);
           if (strncmp(dig.out_proj,"stp",3) == 0)
	      llstp(&dig.stzone,&lat,&lon,&northing,&easting);
	   }

        sprintf(out_buff,"%14.6lf %15.7lf\n", easting,northing);
        fputs(out_buff,reg_info);
	fclose(reg_info);


                                        /* generate dig head info */
        sprintf(out_buff,"%s\n", dig.organization);
        fputs(out_buff,gef_info);
        sprintf(out_buff,"%s\n", dig.date);
        fputs(out_buff,gef_info);
        sprintf(out_buff,"%s\n", dig.your_name);
        fputs(out_buff,gef_info);
        sprintf(out_buff,"%s, %s\n", dig.area,dig.map_name);
        fputs(out_buff,gef_info);
        sprintf(out_buff,"%13.2lf\n", dig.orig_scale);
        fputs(out_buff,gef_info);
        sprintf(out_buff,"%s\n", dig.other_info);
        fputs(out_buff,gef_info);
        sprintf(out_buff,"%13.2lf\n",dig.N);
        fputs(out_buff,gef_info);
        sprintf(out_buff,"%13.2lf\n",dig.S);
        fputs(out_buff,gef_info);
        sprintf(out_buff,"%13.2lf\n",dig.E);
        fputs(out_buff,gef_info);
        sprintf(out_buff,"%13.2lf\n",dig.W);
        fputs(out_buff,gef_info);
        sprintf(out_buff,"%13.2lf\n", dig.map_thresh);
        fputs(out_buff,gef_info);
        sprintf(out_buff,"%13.2lf\n", dig.xofset);
        fputs(out_buff,gef_info);
        sprintf(out_buff,"%13.2lf\n", dig.yofset);
        fputs(out_buff,gef_info);
        sprintf(out_buff,"%d\n", dig.stzone);
        fputs(out_buff,gef_info);
        sprintf(out_buff,"%d\n", dig.plani_zone);
        fputs(out_buff,gef_info);
        sprintf(out_buff,"%s\n", dig.in_proj);
        fputs(out_buff,gef_info);
        sprintf(out_buff,"%s\n", dig.out_proj);
        fputs(out_buff,gef_info);
	fclose(gef_info);
        rewind(gef_file);
	fclose(gef_file);

	return (0);

}

show_info(HEAD)
   struct bdig_head *HEAD;
{
	int  i, ier, repeat = 1   ;
	char answer[10], bufr[100], *gets() ;

/* Gather all existing variables **********************************************/

	while (repeat)
	{
		V_clear();
		ier = 0;
		V_line ( 2, TEXT2 ) ;
		V_line ( 4, TEXT4 ) ;
		V_line ( 5, TEXT5 ) ;
		V_line ( 6, TEXT6 ) ;
		V_line ( 7, TEXT7 ) ;
		V_line ( 8, TEXT8 ) ;
		V_line ( 9, TEXT9 ) ;
		V_line (10, TEXT10 ) ;
		V_line (11, TEXT11 ) ;
		V_line (12, TEXT12 ) ;
		V_line (13, TEXT13 ) ;
		V_line (14, TEXT14 ) ;
		V_line (15, TEXT15 ) ;
		V_line (22, TEXT22 ) ;

		V_ques(HEAD->organization, 's', 7, 31, 29) ;
		V_ques(HEAD->date ,        's', 8, 28, 20) ;
		V_ques(HEAD->map_name ,    's', 9, 22, 20) ;
		V_ques(HEAD->area ,        's', 10, 26, 20) ;
		V_ques(&HEAD->orig_scale , 'd', 11, 24, 15) ;
		V_ques(&HEAD->st ,         'i', 12, 29, 20) ;
		V_ques(&HEAD->cnty ,       'i', 13, 30, 20) ;
		V_ques(HEAD->in_proj ,     's', 14, 63, 10) ;
		V_ques(HEAD->out_proj ,    's', 15, 64, 10) ;

		V_intrpt_ok();	
		if (!V_call())
			exit(1) ;
		V_clear();

	/*   Check values for update, all values should have changed
		from their initialization */
	if (HEAD->st == 0 || HEAD->cnty == 0)
		{
		fprintf(stderr,"\n\n\tThe State and County FIPS codes are required.\n");
		ier = 1;
		}
	if (HEAD->out_proj != 0)
		{
		if (strncmp(HEAD->out_proj,"utm",3) == 0)
                   {
                   while(1)
                     {
		     fprintf(stderr,"\n");
                     sprintf(bufr,"Current UTM zone %d, will be used",G_zone());
                     if (! G_yes(bufr,1))
                        {
                        fprintf(stderr,"which zone : ");
                        gets(answer);
                        if (strlen(answer) != 0)
                          {
                          sscanf(answer,"%d",&HEAD->plani_zone);
                          ier = 0;
                          break;
                          }
                        else 
                          {
                          fprintf(stderr,"Invalid zone\n");
                          sleep(2);
                          }
                        }
		     else 
			{
			ier = 0;
			break;
			}
                     }
		   }
		if (strncmp(HEAD->out_proj,"stp",3) == 0) ier = 0;
		if (strncmp(HEAD->out_proj,"ll",2) == 0) ier = 0;
		if (strncmp(HEAD->out_proj,"alb",3) == 0) ier = 0;
		}
	     else
                {
                ier = 1;
                fprintf(stderr,"Invalid output projection\n");
                sleep(2);
                }

	if (HEAD->in_proj != 0)
		{
		if (strncmp(HEAD->in_proj,"tab",3) == 0) ier = 0;
		if (strncmp(HEAD->in_proj,"stp",3) == 0) ier = 0;
		if (strncmp(HEAD->in_proj,"ll",2) == 0) ier = 0;
                }
	     else
                {
                ier = 2;
                fprintf(stderr,"Invalid input projection\n");
                sleep(2);
                }

	/*   
             Check for error flag */
	if (ier > 0)
		{
		fprintf(stderr,"\n\t\tHit RETURN -->");
		gets(answer);
		}
	if (ier == 0) repeat = 0;

	}
	/*
	     got changes, ok   */
        return;

}

mvbyt (tcnt, addr1, addr2)
int tcnt;
char *addr1, *addr2;
{
	int mcnt;
	for (mcnt=0; mcnt<tcnt; ++mcnt){
		if ( (*(addr1+mcnt) == '\000') ||
		     (*(addr1+mcnt) == '\012') ||
		     (*(addr1+mcnt) == '\015') ) break;
		*(addr2+mcnt) = *(addr1+mcnt);
		}
	*(addr2+mcnt) = 000;
}

fipsin(SFIPS,CFIPS,STZON)
	int *SFIPS, *CFIPS;
	int *STZON;
{
	int ier, icode, fipscode, NUM_ZON, lookup;
	int record, reccnt, sfips, cfips;
	char buffer[80], STabbr[2], COname[30], TXT_ZONE[10];
	char FIPSfile[60];
	FILE *fipsfile;

	sprintf(FIPSfile,"%s/etc/FIPS.code",getenv("GISBASE"));

/* combine SFIPS and CFIPS to make lookup */
	lookup = *SFIPS * 1000 + *CFIPS;
/*  get the STZON for this SFIPS,CFIPS */

/* open input */
	   if ((ier = access(FIPSfile,4)) != 0)
		{
		fprintf(stderr,"ERROR: FIPS.code file NOT available\n");
		sleep(2);
	        *STZON = -1;
		return(-1);
		}
	   fipsfile = fopen (FIPSfile,"r");
/* read the input, get STZON */
	   for (record=0;;++record)
	        {
		icode = 0;
		if (!fgets (buffer, 80, fipsfile)) break;
                reccnt++;
 		sscanf (buffer,"%d%d%s%s%d%s",&sfips,&cfips,STabbr,COname,&NUM_ZON,TXT_ZONE);
/* put st & co fips together */
		fipscode = sfips * 1000 + cfips;
/* compare for match */
	       	if (lookup == fipscode )
		   {
		   icode = 1;
		   break;
		   }
                }      			/* end file search */
  	if (icode == 0)
	        {                    /* no match */
		fclose (fipsfile);
		fprintf(stderr,"ERROR: Invalid ST and CNTY FIPS code\n");
		sleep(2);
		*STZON = -2;
		return(-2);
	        }
 /* success, good-bye */
        fclose(fipsfile);
	*STZON = NUM_ZON;
	return(0);
}
