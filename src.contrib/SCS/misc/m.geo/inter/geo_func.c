#ident "  @(#)geo_func.c    1.0  3/18/91  "
/*
**  created by R.L.Glenn
**  USDA Soil Conservation Service, CGIS Division
*
*   set of re-usables for geo
*/
#include <stdio.h>
#include <ctype.h>
#include <math.h>
#include "geo.h"

get_enz()
{
	 for(;;)
	 {
         G_clear_screen();
         fprintf(stderr,"                   Coordinate Conversions\n\n");
	 fprintf(stderr,"                 Coord->Lat/Long Conversion\n\n");
	 fprintf(stderr,"\n\n");
	 if (!get_easting())
	    {
	    if (!ier) return(0);
	    else continue;
	    }
	 if (!get_northing())
	    {
	    if (!ier) return(0);
	    else continue;
	    }
	 if (strcmp(proj_name, "utm", 3) == 0)
            {
	    fprintf(stderr,"\n    Enter Zone     : ");
            if (!get_zone()) 
	       {
	       if (!ier) return(0);
	       else continue;
	       }
	    }
         break;
	 }
         return(1);
}

get_easting()
{
	 answer[0] = '\0';
         fprintf(stderr,"\n    Enter Easting  : ");
	 gets(answer);
         if (strlen(answer) == 0)
	    {
	    ier = 0;
	    return(0);
	    }
	 get_num(answer,0);
	 if (ier) return(0);
         return(1);
}

get_northing()
{
	 answer[0] = '\0';
         fprintf(stderr,"\n    Enter Northing : ");
	 gets(answer);
         if (strlen(answer) == 0)
	    {
	    ier = 0;
	    return(0);
	    }
	 get_num(answer,1);
	 if (ier) return(0);
         return(1);
}

get_zone()
{
	 answer[0] = '\0';
	 gets(answer);
         if (strlen(answer) == 0) 
	    {
	    ier = 0;
	    return(0);
	    }
	 get_num(answer,2);
fprintf(stderr,"ZONE= %d\n",ZONE);
	 if (ier) return(0);
         return(1);
}

get_ll()
{
	 for(;;)
	 {
	 ZONE = 0;
         G_clear_screen();
         fprintf(stderr,"                   Coordinate Conversions\n\n");
         fprintf(stderr,"                 Lat/Long->Coord Conversion\n\n");
	 fprintf(stderr,"\n\n");
	 if (!get_lat())
	    {
	    if (!ier) return(0);
	    else continue;
	    }
	 if (!get_lon())
	    {
	    if (!ier) return(0);
	    else continue;
	    }
	 if (strcmp(proj_name, "utm", 3) == 0)
            {
	    fprintf(stderr,"\n    Enter Zone <CR for default> : "); 
  	    if (!get_zone()) 
	       {
	       ZONE = (int)((186.0 + LON)/6.0);
	       return(1);
	       }
            }
         break;
         }
         return(1);
}

/*
	 gets(answer);
         if (strlen(answer) == 0) return(0);
	 for (ptr = answer; *ptr; ptr++)
	     {
	     if (*ptr == '\n')
	        {
	        *ptr = 0;
	        break;
	        }
	     if (!isdigit(*ptr))
	     if (*ptr != '\056' && *ptr != '\055')
	        {
	        fprintf(stderr,"  *** INVALID coord *** \n");
	        sleep(2);
	        return(0);
	        }
	     }
*/

get_lat()
{
	 answer[0] = '\0';
         fprintf(stderr,"\n    Enter Latitude < Deg Min Sec>");
         fprintf(stderr,"\n    Latitude is (+ or n) for US   : ");
	 gets(answer);
         if (strlen(answer) == 0)
	    {
	    ier = 0;
	    return(0);
	    }
	 LAT = 0.0;
	 if (!get_deg(answer,1)) 
	    {
	    ier = 1;
	    return(0);
	    }
	 sscanf(answer,"%lf",&LAT);
         return(1);
}

get_lon()
{
	 answer[0] = '\0';
         fprintf(stderr,"\n    Enter Longitude <Deg Min Sec>");
         fprintf(stderr,"\n    Longitude is (- or w) for US  : ");
	 gets(answer);
         if (strlen(answer) == 0)
	    {
	    ier = 0;
	    return(0);
	    }
	 LON = 0.0;
	 if (!get_deg(answer,0)) 
	    {
	    ier = 1;
	    return(0);
	    }
	 sscanf(answer,"%lf",&LON);
         return(1);
}


/*
*      Get the Prime Meridian value and std parallel value
*****/
get_PM(method)
int method;
{

/*  get LONCEN value arguements */
        fprintf(stderr,"\n    Enter Meridian Longitude value :");
	gets(answer);
        if (strlen(answer) == 0)
	   {
	   LONCEN = 0.0;
	   return(0);
	   }
	if (!get_deg(answer,0))
	    {
	    ier = 1;
	    return(0);
	    }
	sscanf(answer,"%lf",&LONCEN);
/*  get LATPAR value arguements */
        fprintf(stderr,"\n    Enter Parallel latitude value :");
	gets(answer);
        if (strlen(answer) == 0)
	   {
	   LATPAR = 0.0;
	   return(0);
	   }
	if (!get_deg(answer,1)) 
	    {
	    ier = 1;
	    return(0);
	    }
	sscanf(answer,"%lf",&LATPAR);
	return(1);
}

get_num(strng, swt)
char *strng;
int swt;
{
	 double num;

	 for (ptr = answer; *ptr; ptr++)
	     {
	     if (*ptr == '\n')
	        {
	        *ptr = 0;
	        break;
	        }
	     if (!isdigit(*ptr))
	        if (*ptr != '\056' && *ptr != '\055')
	           {
	           fprintf(stderr,"  *** INVALID entry *** \n");
	           sleep(2);
	           ier = 1;
		   return;
	           }
	     }
         sscanf(answer,"%lf", &num);
	 if (swt == 0) EAS = num;
	 if (swt == 1) NOR = num;
	 if (swt == 2) ZONE = (int)(num);
	 ier = 0;
	 return;
}

get_FIP_names()
{
	FILE *fipsfile;
        char buff[81];
	int sfips, cfips, lookup, i;

	sprintf(FIPSfile,"%s/etc/FIPS.code\0",getenv("GISBASE"));

                    /* open input */
        fipsfile = fopen (FIPSfile,"r");

	for (record=0;;++record)
	        {
		icode = 0;
		reccnt++;
		if (fgets(buff,80,fipsfile) == NULL) break;
		sscanf (buff,"%d%d%s%s%d%s",
		    &sfips,&cfips,STabbr,COname,&NUM_ZON,TXT_ZONE);
/* compare for match */
	       if (SFIPS == sfips && CFIPS == cfips)
				{
				icode = 1;
				break;
				}
			}      			/* end file search */
  	        if (icode == 0)
			{                    /* no match */
			fprintf(stderr," No match of fips state %d county %d \n",SFIPS,CFIPS);
			fclose (fipsfile);
			}

 /* all done, good-bye */
        fclose(fipsfile);
	return;
}

get_CO_code()
{
	FILE *fipsfile;


        G_clear_screen();
        sprintf(aline,"%s/etc/FIPS.code\0",getenv("GISBASE"));
          /* open FIPS file */
        if ((ier = access(aline,4)) != 0)
          {
          fprintf(stderr," ERROR: Can not open FIPS code file\n") ;
          sleep(2);
          return(0);
          }
        fipsfile = fopen (aline,"r");
        SFIPS = CFIPS = 0;
        get_fips(&SFIPS,&CFIPS,fipsfile);
        fclose(fipsfile);
        if (SFIPS == 0 && CFIPS == 0) return(0);;
        get_FIP_names();
        if ((COzone = get_stp_num(SFIPS,CFIPS)) == 0)
          {
          fprintf(stderr,
            " Could not get State Plane code for FIPS %d %d\n",
          SFIPS,CFIPS);
          sleep(2);
          return(0);
          }
        return(1);
}

DMS() 
{

/*  convert lat/lon to dd mm ss.ss */
      float Q;

      if (LON > 0) IDEG = LON;
      else IDEG = LON * -1.0;
      Q = IDEG;
      if (LON > 0) Q = (LON-Q)*60.;
      else Q = ((LON * -1.0)-Q)*60.;
      IMIN = Q;
      XSEC = (Q-IMIN)*60.;
      if (XSEC >= 59.99)
	 {
	 IMIN++; XSEC = XSEC - 60.;
	 }
      if (IMIN >= 60)
	 {
	 IDEG++; IMIN = IMIN - 60;
	 }
      if (XSEC < 0.0) XSEC =  0.0;

      if (LAT > 0) JDEG = LAT;
      else JDEG = LAT * -1.0;
      Q = JDEG;
      if (LAT > 0) Q = (LAT-Q)*60.;
      else Q = ((LAT * -1.0)-Q)*60.;
      JMIN = Q;
      YSEC = (Q-JMIN)*60.;
      if (YSEC >= 59.99)
	 {
	 JMIN++; YSEC = YSEC - 60.;
	 }
      if (JMIN >= 60)
	 {
	 JDEG++; JMIN = JMIN - 60;
	 }
      if (YSEC < 0.0) YSEC =  0.0;
      if(LON < 0) IDEG = -IDEG;
      if(LAT < 0) JDEG = -JDEG;
}
