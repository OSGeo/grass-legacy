#ident "  @(#)geo_func.c    1.0  3/18/91  "
/*
**  created by R.L.Glenn
**  USDA Soil Conservation Service, CGIS Division
*
*   set of re-usables for geo
*/
#include <math.h>
#include <gis.h>
#include "geo.h"

get_enz()
{
	 for(;;)
	 {
	 /*
         G_clear_screen();
	 */
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
	 if (ier) exit(0);
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

get_KFACT(in)
int in;
{
	 answer[0] = '\0';
         if (in == 1) 
           fprintf(stderr,"\n    Enter Input %s : ",DESC[KFACT]);
         else
           fprintf(stderr,"\n    Enter Output %s : ",DESC[KFACT]);
	 gets(answer);
         if (strlen(answer) == 0)
	    {
	    ier = 0;
	    return(0);
	    }
         if (in == 1) 
	   get_num(answer,4);
         else
	   get_num(answer,5);
	 if (ier) return(0);
         return(1);
}




get_zone(in)
int in;
{
	 answer[0] = '\0';
         if (in == 1) {
	   fprintf(stderr,"\n    Enter Input Zone     : ");
	   gets(answer);
           if (strlen(answer) == 0) 
	    {
	      ier = 0;
	      return(0);
	    }
	   get_num(answer,2);
	   if (ier) return(0);
         }
         else {
	   fprintf(stderr,"\n    Enter Output Zone     : ");
	   gets(answer);
           if (strlen(answer) == 0) 
	    {
	      ier = 0;
	      return(0);
	    }
	   get_num(answer,3);
	   if (ier) return(0);
         }
         return(1);
}

get_ll()
{
	 for(;;)
	 {
            /*G_clear_screen();*/
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
         fprintf(stderr,"\n    Enter Latitude <%s>:", G_lat_format_string());
	 gets(answer);
         if (strlen(answer) == 0)
	 {
	    ier = 0;
	    return(0);
	 }
	 LAT = 0.0;
	 if (!get_deg(answer,&LAT,1)) 
	 {
	   ier = 1;
	   return(0);
	 }
         return(1);
}

get_lon()
{
	 answer[0] = '\0';
         fprintf(stderr,"\n    Enter Longitude <%s>:", G_lon_format_string());
	 gets(answer);
         if (strlen(answer) == 0)
	 {
	    ier = 0;
	    return(0);
	 }
	 LON = 0.0;
	 if (!get_deg(answer, &LON, 0)) 
	 {
	    ier = 1;
	    return(0);
	 }
         return(1);
}


/*
*      Get the Prime Meridian value and std parallel value
*****/
get_LL_stuff(in,lat,proj_index,index)
int in;
int lat;
int index;
{
    char lat_strng[50], lon_strng[50];
    double deg;

/*  get LONCEN value arguements */
        if (in == 1) {
          if (TABLE[proj_index][index].def_exists == 1) {
            if (lat == 1)
	    {
	      G_format_northing(TABLE[proj_index][index].deflt, lat_strng,
				   PROJECTION_LL);
              fprintf(stderr,"\n    Enter Input %s (%s) :", DESC[index], lat_strng);
            }
            else 
	    {
	      G_format_easting(TABLE[proj_index][index].deflt, lon_strng,
				   PROJECTION_LL);
              fprintf(stderr,"\n    Enter Input %s (%s) :", DESC[index], lon_strng);
	    }
	    gets(answer);
            if (strlen(answer) == 0) {
	      USED_in[index].val = TABLE[proj_index][index].deflt;
	      return(1);
              }
          }
          else {
            fprintf(stderr,"\n    Enter Input %s :", DESC[index]);
	    gets(answer);
            if (strlen(answer) == 0) {
	      USED_in[index].val = 0.0;
	      return(0);
            }
          }
        }
        else {
          if (TABLE[proj_index][index].def_exists == 1) {
            if (lat == 1)
	    {
	      G_format_northing(TABLE[proj_index][index].deflt, lat_strng,
				   PROJECTION_LL);
              fprintf(stderr,"\n    Enter Input %s (%s) :", DESC[index], lat_strng);
            }
            else 
	    {
	      G_format_easting(TABLE[proj_index][index].deflt, lon_strng,
				   PROJECTION_LL);
              fprintf(stderr,"\n    Enter Input %s (%s) :", DESC[index], lon_strng);
	    }
	    gets(answer);
            if (strlen(answer) == 0) {
	      USED_out[index].val = TABLE[proj_index][index].deflt;
	      return(1);
            }
          }
          else {
            fprintf(stderr,"\n    Enter Output %s :", DESC[index]);
	    gets(answer);
            if (strlen(answer) == 0) {
	      USED_out[index].val = 0.0;
	      return(0);
            }
	  }
        }
        if (lat == 1) {
	  if (!get_deg(answer, &deg, 1))
	   {
	    ier = 1;
	    return(0);
	   }
         }
         else {
	   if (!get_deg(answer, &deg, 0))
	    {
	    ier = 1;
	    return(0);
	    }
        }
	if (in == 1) USED_in[index].val = deg;
	else USED_out[index].val = deg;
	return(1);
}

get_num(strng, swt)
char *strng;
int swt;
{
	 double num;
	 int int_num;

	 for (ptr = strng; *ptr; ptr++)
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
         sscanf(strng,"%lf", &num);
	 if (swt == 0) EAS = num;
	 if (swt == 1) NOR = num;
	 if (swt == 2) USED_in[ZONE].val = num;
	 if (swt == 3) USED_out[ZONE].val = num;
	 if (swt == 4) USED_in[KFACT].val = num;
	 if (swt == 5) USED_out[KFACT].val = num;
	 if (swt == 6) radius_in = num;
	 if (swt == 7) radius_out = num;
	 if (swt == 10) unit_fact_in = num;
	 if (swt == 11) unit_fact_out = num;
	 ier = 0;
	 return;
}


DMS(in) 
{

/*  convert lat/lon to dd mm ss.ss */
      float Q;
      double ltt,lng;

      if (in == 1) 
      {
        ltt = LAT;
        lng = LON;
      }
      else {
        ltt = LAT_res;
        lng = LON_res;
      }
      if (lng > 0) IDEG = lng;
      else IDEG = lng * -1.0;
      Q = IDEG;
      if (lng > 0) Q = (lng-Q)*60.;
      else Q = ((lng * -1.0)-Q)*60.;
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

      if (ltt > 0) JDEG = ltt;
      else JDEG = ltt * -1.0;
      Q = JDEG;
      if (ltt > 0) Q = (ltt-Q)*60.;
      else Q = ((ltt * -1.0)-Q)*60.;
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
      if(lng < 0) IDEG = -IDEG;
      if(ltt < 0) JDEG = -JDEG;
}
