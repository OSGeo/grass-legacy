/*
 * r.sunmask:
 *   Calculates the cast shadow areas from a DEM
 *
 * input: DEM (int, float, double)
 * output: binary shadow map
 *    no shadow: null()
 *    shadow:    1
 *
 * Algorithm source: unknown (Janne Soimasuo?)
 * Original module author: Janne Soimasuo, Finland 1994
 *
 * GPL >= 2
 *
 **********************
 * Added solpol sun position calculation:
 * Markus Neteler 4/2001
 *
 **********************
 * MN 2/2001: attempt to update to FP
 * Huidae Cho 3/2001: FP update done
 * 		      but it's somewhat slow with non-CELL maps
 *********************************************************************/

#define MAIN

#include "global.h"
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "gis.h"
#include <string.h>
#include "solpos00.h"

/* to be displayed in r.sunmask */
static char *SOLPOSVERSION = "11 April 2001";

/* uncomment to get debug output */
/* #define DEBUG*/

extern struct posdata pd, *pdat; /* declare a posdata struct and a pointer for
                                    it (if desired, the structure could be
                                    allocated dynamically with malloc) */
struct Cell_head window;

 
/*
#define	RASTER_VALUE_FUNC
*/


union RASTER_PTR
{
    void	*v;
    CELL	*c;
    FCELL	*f;
    DCELL	*d;
};


#ifdef	RASTER_VALUE_FUNC

double raster_value(union RASTER_PTR buf, int data_type, int col);

#else

#define	raster_value(buf, data_type, col)	((double)(data_type == CELL_TYPE ? buf.c[col] : (data_type == FCELL_TYPE ? buf.f[col] : buf.d[col])))

#endif

int main(int argc, char *argv[]) 
{
    char *mapset;
    extern struct Cell_head window;
    union RASTER_PTR elevbuf, tmpbuf, outbuf;
    CELL min, max;
    DCELL dvalue, dvalue2, dmin, dmax;
    struct History  hist;
    RASTER_MAP_TYPE data_type;
    struct Range range;
    struct FPRange fprange;
    double drow, dcol;
    int elev_fd, output_fd, zeros;
    char buf[1024];
    struct
    {
      struct Option *opt1, *opt2, *opt3, *opt4, *north, *east, *year, 
                    *month, *day, *hour, *minutes, *seconds, *timezone;
    } parm;  
    struct Flag *flag1, *flag2, *flag3;
    struct GModule *module;
    char *name, *outname;
    double dazi, dalti;
    double azi, alti;
    double nstep,estep;
    double maxh;
    double east, east1, north, north1;
    int row1, col1;
    char OK;
    double timezone;
    int year, month, day, hour, minutes, seconds;
    long retval;
    int solparms, locparms, use_solpos;
    double sunrise, sunset, current_time;

    G_gisinit (argv[0]);

    module = G_define_module();
    module->description =
            "Calculates cast shadow areas from sun position and DEM. Either "
            "A: exact sun position is specified, or B: date/time to calculate "
            "the sun position by r.sunmask itself.";

    parm.opt1 = G_define_option();
    parm.opt1->key        = "elev" ;
    parm.opt1->type       = TYPE_STRING ;
    parm.opt1->required   = YES ;
    parm.opt1->multiple   = NO ;
    parm.opt1->gisprompt  = "old,cell,raster" ;
    parm.opt1->description= "Name of elevation raster map" ;

    parm.opt2 = G_define_option() ;
    parm.opt2->key        = "output" ;
    parm.opt2->type       = TYPE_STRING ;
    parm.opt2->required   = YES ;
    parm.opt2->multiple   = NO ;
    parm.opt2->gisprompt  = "new,cell,raster" ;
    parm.opt2->description= "Output raster map having shadows" ;

    parm.opt3 = G_define_option() ;
    parm.opt3->key        = "altitude" ;
    parm.opt3->type       = TYPE_DOUBLE ;
    parm.opt3->required   = NO;
    parm.opt3->options    = "0-89.999";
    parm.opt3->description= "A: altitude of the sun above horizon, degrees" ;

    parm.opt4 = G_define_option() ;
    parm.opt4->key        = "azimuth" ;
    parm.opt4->type       = TYPE_DOUBLE ;
    parm.opt4->required   = NO;
    parm.opt4->options    = "0-360";
    parm.opt4->description= "A: azimuth of the sun from the north, degrees" ;

    parm.year = G_define_option();
    parm.year->key = "year";
    parm.year->type = TYPE_INTEGER;
    parm.year->required = NO;
    parm.year->description = "B: year (1950..2050)";

    parm.month = G_define_option();
    parm.month->key = "month";
    parm.month->type = TYPE_INTEGER;
    parm.month->required = NO;
    parm.month->description = "B: month (0..12)";

    parm.day = G_define_option();
    parm.day->key = "day";
    parm.day->type = TYPE_INTEGER;
    parm.day->required = NO;
    parm.day->description = "B: day (0..31)";

    parm.hour= G_define_option();
    parm.hour->key = "hour";
    parm.hour->type = TYPE_INTEGER;
    parm.hour->required = NO;
    parm.hour->description = "B: hour (0..24)";

    parm.minutes = G_define_option();
    parm.minutes->key = "minute";
    parm.minutes->type = TYPE_INTEGER;
    parm.minutes->required = NO;
    parm.minutes->description = "B: minutes (0..60)";

    parm.seconds = G_define_option();
    parm.seconds->key = "second";
    parm.seconds->type = TYPE_INTEGER;
    parm.seconds->required = NO;
    parm.seconds->description = "B: seconds (0..60)";

    parm.timezone = G_define_option();
    parm.timezone->key = "timezone";
    parm.timezone->type = TYPE_INTEGER;
    parm.timezone->required = NO;
    parm.timezone->description = "B: timezone (east positive, offset from GMT, also use to adjust daylight savings)";

    parm.east = G_define_option();
    parm.east->key = "east";
    parm.east->key_desc    = "value";
    parm.east->type = TYPE_STRING;
    parm.east->required = NO;
    parm.east->description = "east coordinate (point of interest, default: map center)";

    parm.north = G_define_option();
    parm.north->key = "north";
    parm.north->key_desc    = "value";
    parm.north->type = TYPE_STRING;
    parm.north->required = NO;
    parm.north->description = "north coordinate (point of interest, default: map center)";

    flag1 = G_define_flag();
    flag1->key         = 'z' ;
    flag1->description = "Zero is a real elevation" ;

    flag2 = G_define_flag();
    flag2->key         = 'v' ;
    flag2->description = "verbose output (also print out sun position etc.)" ;

    flag3 = G_define_flag();
    flag3->key         = 's' ;
    flag3->description = "calculate sun position only and exit" ;

    
    if (G_parser(argc, argv))
      exit(-1);
	
    zeros = flag1->answer;

    G_get_window (&window);

    /* if not given, get east and north: XX*/
    if (!parm.north->answer || !parm.east->answer)
    {
     fprintf (stdout,"Using map center coordinates\n");
     north = (window.north - window.south)/2. + window.south;
     east  = (window.west - window.east)/2. + window.east;
    }
    else /* user defined east, north: */
    {
     sscanf(parm.north->answer, "%lf", &north);
     sscanf(parm.east->answer, "%lf", &east);
     if ( strlen(parm.east->answer) == 0 )
     	G_fatal_error("Empty east coordinate specified!");
     if (strlen(parm.north->answer) == 0 )
     	G_fatal_error("Empty north coordinate specified!");
    }

    /* check which method to use for sun position:
       either user defines directly sun position or it is calculated */
    
    if (parm.opt3->answer && parm.opt4->answer)
       solparms=1; /* opt3 & opt4 complete */
    else
       solparms=0;

    if (parm.year->answer && parm.month->answer && parm.day->answer &&\
        parm.hour->answer && parm.minutes->answer && parm.seconds->answer &&\
        parm.timezone->answer)
       locparms=1; /* complete */
    else
       locparms=0;

    if(solparms && locparms) /* both defined */
        G_fatal_error("Either define sun position or location/date/time parameters.");

    if(!solparms && !locparms) /* nothing defined */
        G_fatal_error("Neither sun position nor east/north, date/time/timezone definition are complete.");

    /* if here, one definition was complete */
    if(locparms)
    {
      fprintf(stderr, "Calculating sun position... (using solpos (V. %s) from NREL)\n", SOLPOSVERSION);
      use_solpos=1;
    }
    else
    {
      fprintf(stderr, "Using user defined sun azimuth, altitude settings (ignoring eventual other values)\n");
      use_solpos=0;
    }

    name = parm.opt1->answer;
    outname= parm.opt2->answer;
    if (!use_solpos)
    {
      sscanf(parm.opt3->answer,"%lf",&dalti);
      sscanf(parm.opt4->answer,"%lf",&dazi);
    }
    else
    {
     sscanf(parm.year->answer, "%i", &year);
     sscanf(parm.month->answer,"%i", &month);
     sscanf(parm.day->answer,  "%i", &day);
     sscanf(parm.hour->answer, "%i", &hour);
     sscanf(parm.minutes->answer, "%i", &minutes);
     sscanf(parm.seconds->answer, "%i", &seconds);
     sscanf(parm.timezone->answer,"%lf", &timezone);
    }

  /* NOTES: G_calc_solar_position ()
   - the algorithm will compensate for leap year.
   - longitude, latitude: decimal degree
   - timezone: DO NOT ADJUST FOR DAYLIGHT SAVINGS TIME.
   - timezone: negative for zones west of Greenwich
   - lat/long: east and north positive
   - the atmospheric refraction is calculated for 1013hPa, 15°C 
   - time: local time from your watch

    Order of parameters:
    long, lat, timezone, year, month, day, hour, minutes, seconds 
   */

  if (use_solpos)
  {
#ifdef DEBUG
      fprintf(stderr, "\nlat:%f  long:%f\n", north, east);
#endif
      retval = G_calc_solar_position (east, north, timezone, year, month, day, hour, minutes, seconds);

      /* print the results */
      if (retval == 0) /* error check */
      {
       if( flag2->answer || (flag3->answer && !flag2->answer))
       {
        fprintf (stderr, " %d.%02d.%02d, daynum %d, time: %02i:%02i:%02i (decimal time: %f)\n",
         pdat->year, pdat->month, pdat->day, pdat->daynum,  
         pdat->hour, pdat->minute, pdat->second, 
         pdat->hour + (pdat->minute * 100.0 / 60.0 + pdat->second * 100.0/3600.0)/100. );
        fprintf(stderr, " long: %f, lat: %f, timezone: %f\n", pdat->longitude, pdat->latitude, pdat->timezone);
        fprintf (stderr, " Solar position: sun azimuth %f,\n   sun angle above horz.(refraction corrected) %f\n",
         pdat->azim, pdat->elevref );
        fprintf (stderr, " Sunrise time (without refraction): %02.0f:%02.0f\n", floor(pdat->sretr/60.), fmod(pdat->sretr, 60.));
        fprintf (stderr, " Sunset time  (without refraction): %02.0f:%02.0f\n", floor(pdat->ssetr/60.), fmod(pdat->ssetr, 60.));
       }
       sunrise=pdat->sretr/60. ; /* decimal minutes */
       sunset =pdat->ssetr/60. ;
       current_time=pdat->hour + (pdat->minute/60.) + (pdat->second/3600.);
     }
     else /* fatal error in G_calc_solar_position() */
        G_fatal_error("Please correct settings.");
  }

  if (use_solpos)
  {  
    dalti=pdat->elevref;
    dazi=pdat->azim;
  } /* otherwise already defined */

  
 /* check sunrise */
  if (use_solpos)
  {
    if ((current_time < sunrise))
    {
        fprintf(stderr, "Time (%02i:%02i:%02i) is before sunrise (%02.0f:%02.0f)!\n", pdat->hour, pdat->minute, pdat->second,\
                         floor(pdat->sretr/60.), fmod(pdat->sretr, 60.));
        G_fatal_error("Please correct time settings.");
    }
    if ((current_time > sunset))
    {
        fprintf(stderr, "Time (%02i:%02i:%02i) is after sunset (%02.0f:%02.0f)!\n", pdat->hour, pdat->minute, pdat->second,\
                         floor(pdat->ssetr/60.), fmod(pdat->ssetr, 60.));
        G_fatal_error("Please correct time settings.");
    }
  }
 
  if (flag3->answer && (use_solpos==1) )  /* we only want the sun position */
  {
        fprintf(stderr, "No map calculation requested. Finished.\n");
    	exit(0);
  }
  else
    if (flag3->answer && (use_solpos==0) )
    {
        /* are you joking ? */
  	fprintf(stderr, "You already know the sun position.\n");
  	exit(0);
    }

  /* Search for output layer in all mapsets ? yes. */
  mapset = G_find_cell2 (name, "") ;

  if((elev_fd = G_open_cell_old (name, mapset)) < 0)
    {
      sprintf (buf,"can't open %s", name);
      G_fatal_error(buf);
    }
    if((output_fd = G_open_cell_new(outname)) < 0)
    {
      sprintf (buf,"can't open %s", outname);
      G_fatal_error(buf);
    }

    data_type = G_raster_map_type(name, mapset);
    elevbuf.v = G_allocate_raster_buf(data_type);
    tmpbuf.v  = G_allocate_raster_buf(data_type);
    outbuf.v  = G_allocate_raster_buf(CELL_TYPE); /* binary map */

    if(data_type == CELL_TYPE)
    {
       if ((G_read_range(name, mapset,&range))<0)
       {
         sprintf (buf,"can't open range file for %s",name);
         G_fatal_error(buf);
       }
       G_get_range_min_max(&range,&min,&max);
       dmin = (double) min;
       dmax = (double) max;
    }
    else
    {
        G_read_fp_range(name, mapset, &fprange);
        G_get_fp_range_min_max(&fprange,&dmin,&dmax);
    }

    azi=2*3.1415926*dazi/360;
    alti=2*3.1415926*dalti/360;
    nstep=cos(azi)*window.ns_res;
    estep=sin(azi)*window.ew_res;
    row1=0;

    fprintf(stderr, "Calculating shadows from DEM...");

    while (row1 < window.rows) 
	  {
            G_percent(row1, window.rows, 2);
	    col1=0;
	    drow=-1;
	    if (G_get_raster_row(elev_fd, elevbuf.v, row1, data_type) < 0)
	      G_fatal_error("can't read row in input elevation map");

	    while (col1<window.cols)
	      {
		dvalue=raster_value(elevbuf, data_type, col1);
/*		outbuf.c[col1]=1;*/
		G_set_null_value(&outbuf.c[col1],1,CELL_TYPE);
		OK=1;
		east=G_col_to_easting(col1+0.5,&window);
		north=G_row_to_northing(row1+0.5,&window);
		east1=east;
		north1=north;
		if (dvalue==0.0 && !zeros) OK=0;
		while (OK==1)

			{
			east+=estep;
			north+=nstep;
			if(north>window.north || north < window.south 
			   || east>window.east || east < window.west)
				OK=0;
			else
				{
				maxh=tan(alti)*
				     sqrt((north1-north)*(north1-north)+
					  (east1-east)*(east1-east));
				if ((maxh) > (dmax-dvalue))
					OK=0;
				else
				  {
				  dcol=G_easting_to_col(east,&window);
				  if(drow!=G_northing_to_row(north,&window))
					{
					drow=G_northing_to_row(north,&window);
	    				G_get_raster_row(elev_fd, tmpbuf.v,(int) drow, data_type);
					}
				  dvalue2=raster_value(tmpbuf, data_type, (int)dcol);
				  if ((dvalue2-dvalue)>(maxh))
					{
					OK=0;
					outbuf.c[col1]=1;
					}
				  }
				}
			}
#ifdef DEBUG
	    fprintf(stderr,"Analysing col %i\n", col1);
#endif
		col1+=1;
	      }
#ifdef DEBUG
	    fprintf(stderr,"Writing result row %i of %i\n", row1, window.rows);
#endif
	    G_put_raster_row(output_fd, outbuf.c, CELL_TYPE);
	    row1+=1;
	  }
    
    G_close_cell(output_fd);
    G_close_cell(elev_fd);

    /* writing history file */
    G_short_history(outname, "raster", &hist);
    sprintf(hist.edhist[0], "%s", *argv); 
    sprintf(hist.datsrc_1,"raster elevation file %s", name);
    /* bug: long lines are truncated */
    sprintf(hist.datsrc_2,"%s", G_recreate_command());
    hist.edlinecnt = 3;
    G_write_history (outname, &hist);

    fprintf(stderr,"Finished.\n");
    exit(0);
}

#ifdef	RASTER_VALUE_FUNC
double raster_value(union RASTER_PTR buf, int data_type, int col)
{
	double	retval;

	switch(data_type){
		case CELL_TYPE:
			retval = (double) buf.c[col];
			break;
		case FCELL_TYPE:
			retval = (double) buf.f[col];
			break;
		case DCELL_TYPE:
			retval = (double) buf.d[col];
			break;
	}

	return retval;
}
#endif

