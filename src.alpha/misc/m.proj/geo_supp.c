
#ident "  @(#)geo_supp.c    1.0  3/18/91  "
/*
**  created by R.L.Glenn
**  USDA Soil Conservation Service, CGIS Division
*
*   set of support function for geo
*/
#include <math.h>
#include "gis.h"
#include "geo.h"

 /*  show the user results and/or put to a file */
Write_results(end)
int end;
{
  char line[80];
  char lat_strng[40], lon_strng[40];

  G_format_easting(cur_LON, lon_strng, PROJECTION_LL);
  G_format_northing(cur_LAT, lat_strng, PROJECTION_LL);
  if (!end)
  {
    if (strncmp(proj_name_in,"ll",2) == 0)
    {  
      if (strncmp(proj_name_out,"ll",2) == 0)    /* Lat/Lon -> Lat/Lon */
      {
        if (output_typ == 1)
        {                                    /*  put out the heading */
          fprintf(stderr,"\n\n    %s -> %s  Conversion:\n",proj_title_in,proj_title_out);
          fprintf(stderr,"\n\n    No conversion needed !!! \n");
          fprintf(stderr,"\n\n    Longitude      Latitude \n");
          fprintf(stderr,"   -----------    ---------- \n");
          sprintf(line,"   %-15s   %-15s\0", lon_strng, lat_strng);
          fprintf(stderr,"\n%s\n",line);
	  fprintf(stderr,"\n                                         To continue    -Hit any key- ");
	  gets(answer);
	}
        else
	{
	  sprintf(line,"   %-15s   %-15s   %-15s   %-15s\n", lon_strng, lat_strng,  lon_strng, lat_strng);
	  fputs(line, Out_file);
	}
      } 
      else
      {
        if (output_typ == 1)                 /* Lat/Lon -> Coord */
        {                                    /*  put out the heading */
          fprintf(stderr,"\n\n    %s -> %s  Conversion:\n",proj_title_in,proj_title_out);
          fprintf(stderr,"\n\n    Longitude      Latitude         X (%s)           Y (%s)\n", units_out, units_out);
          fprintf(stderr,"   -----------    ----------       ------------        ------------\n");
          sprintf(line,"   %-15s   %-15s      %10.2f         %10.2f\0", lon_strng, lat_strng, EAS_res,NOR_res);
          fprintf(stderr,"\n%s\n",line);
	  fprintf(stderr,"\n                                         To continue    -Hit any key- ");
	  gets(answer);
	}
        else
	{
          sprintf(line,"   %-15s   %-15s      %10.2f         %10.2f\0", lon_strng, lat_strng, EAS_res,NOR_res);
	  fputs(line, Out_file);
	}
      }
    }
    else
    {
      if (strncmp(proj_name_out,"ll",2) == 0)   /* Coord -> Lat/Lon */
      {  
        if (output_typ == 1)
        {                                    /*  put out the heading */
          fprintf(stderr,"\n\n    %s -> %s  Conversion:\n",proj_title_in,proj_title_out);
          fprintf(stderr,"\n\n    X (%s)       Y (%s)       Longtitude          Latitude\n", units_in, units_in);
          fprintf(stderr,"   ------------     -------------     -----------        ---------\n");
          sprintf(line,"%10.2f     %10.2f           %-15s    %-15s", EAS, NOR, lon_strng, lat_strng);
          fprintf(stderr,"\n%s\n",line);
	  fprintf(stderr,"\n                                         To continue    -Hit any key- ");
	  gets(answer);
	}
        else
	{
          sprintf(line,"%10.2f     %10.2f           %-15s    %-15s", EAS, NOR, lon_strng, lat_strng);
	  fputs(line, Out_file);
	}
      } 
      else 
      {
        if (output_typ == 1)                 /* Coord -> Coord */
        {                                    /*  put out the heading */
          fprintf(stderr,"\n\n    %s -> %s  Conversion:\n",proj_title_in,proj_title_out);
          fprintf(stderr,"\n\n   X_in (%s)    Y_in (%s)      X_out (%s)       Y_out (%s)\n", units_in, units_in, units_out, units_out);
          fprintf(stderr,"   ------------    -----------       -------------        -------------\n");
	  sprintf(line,"%10.2f        %10.2f         %10.2f         %10.2f\0",EAS,NOR,EAS_res,NOR_res);
	  fprintf(stderr,"\n%s\n",line);
	  fprintf(stderr,"\n                                         To continue    -Hit any key- ");
	  gets(answer);
        }
        else
        {
	  sprintf(line,"%10.2f        %10.2f         %10.2f         %10.2f\n",EAS,NOR,EAS_res,NOR_res);
	  fputs(line, Out_file);
        }
      }
    }
  }

  if (end)
  {
    if (output_typ == 2)
    {
      fclose(Out_file);
      sprintf(line,"   %6d coordinates processed, to continue    -Hit any key- ",rec_cnt);
      fprintf(stderr,"\n%s\n",line);
      gets(answer);
    }
  }
}

get_file(method)
int method;
{
	 G_clear_screen();
         fprintf(stderr," Enter File Name > ");
	 gets(answer);
         if (method == 1)
            {
	    sprintf(in_file,"%s",answer);
	    if ( (In_file = fopen(in_file, "r")) == NULL)
	       {
	       fprintf(stderr,"\n File name could not be found/created\n");
	       sleep(2);
	       input_typ = 0;
	       }
            }
         if (method == 2)
            {
	    sprintf(out_file,"%s",answer);
	    if ( (Out_file = fopen(out_file, "w")) == NULL)
	       {
	       fprintf(stderr,"\n File name could not be found/created\n");
	       sleep(2);
	       output_typ = 0;
	       }
            }
}
