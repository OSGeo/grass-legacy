#include <unistd.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include "gis.h"
#include "CC.h"

int get_utms(int *, int *, double *, double *, int *);
int zdata(int, double, double);

char sphere[30];

struct info{
  int zone_num;
  int count;
  double east;
  double west;
  double south;
  double north;
};
struct info ZONE_INFO[3];

int main (int argc, char *argv[])
{
  char infile[15];
  int infd;
  
  struct GModule *module;
  struct Option *input, *zone_in, *spheroid;
  
  int i,j;
  int lon[12],lat[12],zone[12];
  int z;
  double east[12],north[12];
  char buf[300];
  int num_zones, ptr, curr_zone, num_pts;
  int ll_flag;
  float percent;
  
  G_gisinit(argv[0]);
  
  /* don't include lat/lon option for now
     latlon = G_define_flag();
     latlon->key='l';
     latlon->description="Use lat/lon coordinates";
     */
  
  module = G_define_module();
  module->description =
	"Finds geographic region information "
	"for U.S. Census Bureau TIGER input data.";

  input = G_define_option();
  input->key="infile";
  input->type=TYPE_STRING;
  input->required=YES;
  input->description="Filename for raw tiger data (type 1)";
  input->multiple=NO;
  
  zone_in = G_define_option();
  zone_in->key="zone";
  zone_in->type=TYPE_INTEGER;
  zone_in->required=NO;
  zone_in->description="UTM zone for this county";
  zone_in->answer="0";
  zone_in->multiple=NO;
  zone_in->options="-60-60";
  
  spheroid = G_define_option();
  spheroid->key="spheroid";
  spheroid->type=TYPE_STRING;
  spheroid->required=NO;
  spheroid->description="Name of spheroid to use";
  spheroid->answer="clark66";
  spheroid->multiple=NO;
  
  if (G_parser(argc,argv))
    exit(1);
  
  curr_zone = ptr = num_zones = num_pts = 0;
  ll_flag=0;
  sprintf(infile,"%s",input->answer);
  
  if (!(infd = open(infile,4))){
    fprintf(stderr,"Can't open input file %s for reading\n",infile);
    exit(-1);
  }
  
  
  /*
    if (latlon->answer)
    ll_flag=1;
    */
  
  if (!ll_flag){
    sscanf(zone_in->answer,"%d",&z);
    strcpy(sphere,spheroid->answer);
  }
  
  if (ll_flag)
    {
      double min_lat, max_lat, min_lon, max_lon;
      double lt, ln;
      
      min_lat=min_lon=1000000.0;
      max_lat=max_lon=-999999.0;
      
      while(1)
	{
	  /* get a whole record (1 line) */
	  /* NOTE: The record extraction routines assume an unknown number
	     of carriage returns and/or linefeeds (^M and/or ^J).  Code will
	     have to change if field terminators change (doubtful). */
	  
	  if (read(infd,buf,1) != 1) break;
	  while ((buf[0] == 13) || (buf[0] == 10)) {
	    if (read(infd,buf,1) != 1) break;
	  }
	  if (read(infd,buf+1,227) != 227) break;
	  buf[228] = 0;
	  
	  for (i=1; i<3; i++) {
	    lat[i] = 0; lon[i] = 0;
	  }
	  /* lat/lon values in the TIGER file are signed and given according to
	     standard FIPS notation: negative latitude represents southern
	     hemisphere and negative longitude represents western hemisphere...
	     below the lat/lon values are read in without the +/- signs, mins
	     and maxes computed, and then region boundaries printed out
	     according to these FIPS notation conventions... */
	  
	  sscanf(buf+190,"-%9d+%8d-%9d+%8d", &lon[1], &lat[1], &lon[2],&lat[2]);
	  
	  for (i=1; i<3; i++)
	    {
	      lt = ((double)lat[i])/1000000.0;
	      ln = ((double)lon[i])/1000000.0;
	      
	      if (lt>max_lat)
		max_lat=lt;
	      if (lt<min_lat)
		min_lat=lt;
	      if (ln>max_lon)
		max_lon=ln;
	      if (ln<min_lon)
		min_lon=ln;
	    }  /* end of loop */
	  
	}  /* end of while  */
      
      fprintf (stdout,"REGION FOR THIS DATA FILE:\n\n");
      fprintf (stdout,"  northern latitude:  %f\n",max_lat);
      fprintf (stdout,"  southern latitude:  %f\n",min_lat);
      fprintf (stdout,"  eastern longitude: -%f\n",min_lon);
      fprintf (stdout,"  western longitude: -%f\n",max_lon);
      
    }  /*  end of if ll_flag section  */
  else
    {
      
      /* if user supplied non-zero zone number, convert to UTMs, but
	 force eastings/northings into this provided zone */
      
      if (z != 0)
	{
	  double r_east, r_west, r_north, r_south;
	  
	  zone[1]=zone[2]=z;
	  
	  /* get a whole record (1 line) */
	  if (read(infd,buf,1) != 1) exit(-1);
	  while ((buf[0] == 13) || (buf[0] == 10)) {
	    if (read(infd,buf,1) != 1) break;
	  }
	  if (read(infd,buf+1,227) != 227) exit(-1);
	  buf[228] = 0;
	  
	  for (i=1; i<3; i++) {
	    lat[i] = 0; lon[i] = 0;
	  }
	  
	  sscanf(buf+190,"-%9d+%8d-%9d+%8d", &lon[1], &lat[1], &lon[2],&lat[2]);
	  get_utms(lon,lat,east,north,zone);
	  
	  /* set initial region values to coordinates of first point */
	  r_east=r_west=east[1];
	  r_south=r_north=north[1];
	  
	  /* from now on, test for max/min */
	  
	  if (east[2]<r_west)
	    r_west=east[2];
	  if (east[2]>r_east)
	    r_east=east[2];
	  if (north[2]<r_south)
	    r_south=north[2];
	  if (north[2]>r_north)
	    r_north=north[2];
	  
	  while(1)
	    {
	      /* get a whole record (1 line) */
	      if (read(infd,buf,1) != 1) break;
	      while ((buf[0] == 13) || (buf[0] == 10)) {
		if (read(infd,buf,1) != 1) break;
	      }
	      if (read(infd,buf+1,227) != 227) break;
	      buf[228] = 0;
	      
	      for (i=1; i<3; i++) {
		lat[i] = 0; lon[i] = 0;
	      }
	      sscanf(buf+190,"-%9d+%8d-%9d+%8d", &lon[1], &lat[1], &lon[2],&lat[2]);
	      if ( lon[1] != 0 ) {
		get_utms(lon,lat,east,north,zone);
		
		for (i=1; i<3; i++)
		  {
		    
		    if (east[i]<r_west)
		      r_west=east[i];
		    if (east[i]>r_east)
		      r_east=east[i];
		    if (north[i]<r_south)
		      r_south=north[i];
		    if (north[i]>r_north)
		      r_north=north[i];
		  }
	      }
	      
	    } /* end of while loop */
	  
	  fprintf (stdout,"REGION FOR THIS DATA FILE:\n\n");
	  fprintf (stdout,"  north border:  %f\n",r_north);
	  fprintf (stdout,"  south border:  %f\n",r_south);
	  fprintf (stdout,"  east border:   %f\n",r_east);
	  fprintf (stdout,"  west border:   %f\n",r_west);
	  fprintf (stdout,"  (zone number:  %d)\n",z);
	  
	} /* end of if z != 0 */
      else
	{
	  
	  while(1)
	    {
	      /* get a whole record (1 line) */
	      if (read(infd,buf,1) != 1) break;
	      while ((buf[0] == 13) || (buf[0] == 10)) {
		if (read(infd,buf,1) != 1) break;
	      }
	      if (read(infd,buf+1,227) != 227) break;
	      buf[228] = 0;
	      
	      for (i=1; i<3; i++) {
		lat[i] = 0; lon[i] = 0;
		zone[i]=z;
	      }
	      
	      sscanf(buf+190," -%9d+%8d -%9d+%8d", &lon[1], &lat[1], &lon[2],&lat[2]);
	      get_utms(lon,lat,east,north,zone);
	      
	      /* record zone info for calculated zone(s) */
	      
	      for (i=1;i<3;i++)
		{
		  num_pts++;
		  if (zone[i] == curr_zone)
		    zdata(ptr,east[i],north[i]);
		  else
		    {
		      curr_zone = zone[i];
		      for (j=0; j<num_zones; j++)
			{
			  if (ZONE_INFO[j].zone_num == zone[i])
			    {
			      ptr = j;
			      zdata(ptr,east[i],north[i]);
			      break;
			    }
			}
		      /* if first piece of zone info, or
			 if didn't find the current zone
			 number, create a new place for
			 current zone info.  program is
			 not created to handle more than
			 3 zones from one county as this
			 is a highly unlikely possibility.
			 */
		      if (j == num_zones || num_zones == 0){
			
			if (num_zones == 3){
			  fprintf(stderr,
				  "Bad data - more than 3 zones found for one county.");
			  exit(-3);
			}
			
			ptr = num_zones;
			num_zones++;
			
			ZONE_INFO[ptr].zone_num=zone[i];
			ZONE_INFO[ptr].count++;
			
			/* info for storing regional
			   spread of data points from
			   this county within this zone
			   set initially to first point
			   */
			
			ZONE_INFO[ptr].west=ZONE_INFO[ptr].east=east[i];
			ZONE_INFO[ptr].south=ZONE_INFO[ptr].north=north[i];
			
		      }
		    }
		}
	    }  /* end of while */
	  
	  
	  fprintf (stdout,"\nNumber of calculated zones is: %d\n",num_zones);
	  for (j=0; j<num_zones; j++){
	    fprintf (stdout,"\nINFO FOR ZONE %d:\n",j+1);
	    fprintf (stdout,"  zone number:    %d\n",ZONE_INFO[j].zone_num);
	    percent = ZONE_INFO[j].count*100.0/num_pts;
	    fprintf (stdout,"  percentage of data points\n  in this zone:       %f\n",percent);
	    fprintf (stdout,"  regional spread of points\n  within this zone:\n");
	    fprintf (stdout,"    north:   %f\n    south:   %f\n",ZONE_INFO[j].north,ZONE_INFO[j].south);
	    fprintf (stdout,"    east:    %f\n    west:    %f\n",ZONE_INFO[j].east,ZONE_INFO[j].west);
	  }
	  
	} /* end of else (z != 0) */
    } /* end of else (ll_flag) */
  exit(0);
  
  
} /* end of main */

int get_utms (int *lon, int *lat, double *east, double *north, int *zone)
{
  static int init_once=0;
  int i;
  double lt, ln;
  
  if (!init_once) {
    if ((CC_u2ll_spheroid(sphere)!=1))
      G_fatal_error(
		    "\nBad spheroid.  See Mgc2ll GRASS Manual Page for choices.");
    init_once = 1;
  }
  
  for (i=1; i<3; i++) {
    east[i] = north[i] = 0.0;
  }
  
  for (i=1; i<3; i++) {
    if (lon[i]==0) break;
    lt = ((double) lat[i])/1000000.0;
    ln = ((double) lon[i])/1000000.0;
    CC_ll2u( lt*3600.0, ln*3600.0, &east[i],&north[i],&zone[i]);
  }

  return 0;
}

int zdata (int ptr, double east, double north)
     /* store data for the current zone number returned in the lat/lon 
	to UTM conversion.  stored data includes a count to tell the 
	percentage of points from the county that are in this zone,
	and the regional spread of points within this zone. */
{
  ZONE_INFO[ptr].count++;
  
  if (east > ZONE_INFO[ptr].east)
    ZONE_INFO[ptr].east = east;
  
  else if (east < ZONE_INFO[ptr].west)
    ZONE_INFO[ptr].west = east;
  
  if (north > ZONE_INFO[ptr].north)
    ZONE_INFO[ptr].north = north;
  
  else if (north < ZONE_INFO[ptr].south)
    ZONE_INFO[ptr].south = north;

  return 0;
}
