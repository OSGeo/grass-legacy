/* improved from v.bubble code 6/2000 MN*/

/* Function: "main.c" for GRASS Program "v.circle".               */
/* "v.circle" creates vector "circle" polygons using points from  */
/* the site_lists files as the centers of the those "polygon"     */
/* circles.                                                       */

/* Exit status of 0 indicates program was successful.     */
/* Exit status of 1 indicates program was not successful. */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <math.h>
#include "Vect.h"
#include "gis.h"
#include "v.circle.h"

int main( int argc, char **argv )
 {
  char *vect_mapset;
  char *site_mapset;
  struct Option *opt_radius;
  struct Option *opt_radius_uom;
  struct Option *opt_area;
  struct Option *opt_area_uom;
  struct Option *opt_input;
  struct Option *opt_output;
  int scan_int;
  double radius, area;
  char input[1024], output[1024];
  char fqn_output[1024];
  char *fqn;
  char radius_uom[128], area_uom[128];
  double n, e, n_circum, e_circum, theta;
  long int deg;
  FILE *fd_site;
  struct Map_info map;
  struct dig_head *local_head;
  struct line_pnts *pnts;
  double x[361], y[361];
  double max_x, max_y;
  double min_x, min_y;
  short int once;
  char today[20];
  char command[1024];
  struct Flag *flag;
  long int count;
  circlesite *bsite;
  int nsites; /*number of sites*/
  int i1,i2,i3;
  struct Categories cats;
  char catbuffer[80];
  FILE *f_att = NULL;
      
/* Initialize the GIS calls */
  G_gisinit(argv[0]) ;

  /* Request a pointer to memory for each flag. */
  flag = G_define_flag();
  flag->key = 's';
  flag->description = "Automatically run \"v.support\" on newly created \
vector file."; 
  flag->answer = 0x00;

/* Request a pointer to memory for each option. */
  opt_radius = G_define_option();
  opt_radius->key = "radius";
  opt_radius->type = TYPE_DOUBLE;
  opt_radius->required = NO;
  opt_radius->description = "Radius of circle(s) with \"site_lists\" point(s) \
as center(s).  If radius selected then area values are not used for \
computations.  If both radius and area selected, then radius has precedence \
over area.";
  opt_radius->answer = "0.0";

/* Request a pointer to memory for each option. */
  opt_radius_uom = G_define_option();
  opt_radius_uom->key = "radius_uom";
  opt_radius_uom->type = TYPE_STRING;
  opt_radius_uom->required = NO;
  opt_radius_uom->description = "Radius unit of measure, ie. (m)meters, \
ft(feet), (mi)miles.";
  opt_radius_uom->answer = "m";

/* Request a pointer to memory for each option. */
  opt_area = G_define_option();
  opt_area->key = "area";
  opt_area->type = TYPE_DOUBLE;
  opt_area->required = NO;
  opt_area->description = "Area of circle(s) with \"site_lists\" point(s) as \
center(s).  If area selected then radius values are not used for computations.";
  opt_area->answer = "0.0";

/* Request a pointer to memory for each option. */
  opt_area_uom = G_define_option();
  opt_area_uom->key = "area_uom";
  opt_area_uom->type = TYPE_STRING;
  opt_area_uom->required = NO;
  opt_area_uom->description = "Area unit of measure, ie. sqm(square meters), \
ac(acres), sqmi(square miles), hec(hectares).";
  opt_area_uom->answer = "sqm";

/* Request a pointer to memory for each option. */
  opt_input = G_define_option();
  opt_input->key = "sitefile";
  opt_input->type = TYPE_STRING;
  opt_input->required = YES;
  opt_input->gisprompt = "old,site_lists,site_lists";
  opt_input->description = "GRASS site_lists file (input).";
  opt_input->answer = "";

/* Request a pointer to memory for each option. */
  opt_output = G_define_option();
  opt_output->key = "output";
  opt_output->type = TYPE_STRING;
  opt_output->required = YES;
  opt_output->gisprompt = "any,dig,vector";
  opt_output->description = "Vector file to be created (output).";
  opt_output->answer = "";

  /* Using GRASS parsing to obtain arguments... */
  if (G_parser(argc, argv) != 0)
   {
    exit(1);
   }

  scan_int=sscanf(opt_radius->answer,"%lf",&radius);
  if (scan_int <= 0)
   {
    char msg[256];      
    sprintf(msg,"%s: \"%s\" is an incorrect value for radius.\n",
            G_program_name(),opt_radius->answer );
    G_fatal_error (msg);
    exit(1);
   }

  scan_int=sscanf(opt_radius_uom->answer,"%s",radius_uom);
  if (scan_int <= 0)
   {
    char msg[256];      
    sprintf(msg,"%s: \"%s\" is an incorrect unit of measure for radius.  \
Please use one these units of measure:  \"m\" (for meters), or \"ft\" \
(for feet), or \"mi\" (for miles)",
            G_program_name(),opt_radius_uom->answer );
    G_fatal_error (msg);
    exit(1);
   }
  if ( (strcmp(radius_uom,"m")!=0)&&(strcmp(radius_uom,"ft")!=0)
        &&(strcmp(radius_uom,"mi")!=0) )
   {
    char msg[256];      
    sprintf(msg,"%s: \"%s\" is an incorrect unit of measure for radius.  \
Please use one these units of measure:  \"m\" (for meters), or \"ft\" \
(for feet), or \"mi\" (for miles)",
            G_program_name(),opt_radius_uom->answer );
    G_fatal_error (msg);
    exit(1);
   }

  scan_int=sscanf(opt_area->answer,"%lf",&area);
  if (scan_int <= 0)
   {
    char msg[256];      
    sprintf(msg,"%s: \"%s\" is an incorrect value for area.\n",
            G_program_name(),opt_area->answer );
    G_fatal_error (msg);
    exit(1);
   }

  scan_int=sscanf(opt_area_uom->answer,"%s",area_uom);
  if (scan_int <= 0)
   {
    char msg[256];      
    sprintf(msg,"%s: \"%s\" is an incorrect unit of measure for area.  \
Please use one these units of measure:  \"sqm\" (for square meters), or \
\"sqmi\" (for square miles),  or \"ac\" (for acres), or \"hec\" (for hectares)",
            G_program_name(),opt_area_uom->answer );
    G_fatal_error (msg);
    exit(1);
   }
  if ( (strcmp(area_uom,"sqm")!=0)&&(strcmp(area_uom,"sqmi")!=0)&&
       (strcmp(area_uom,"ac") !=0)&&(strcmp(area_uom,"hec") !=0)   )
   {
    char msg[256];      
    sprintf(msg,"%s: \"%s\" is an incorrect unit of measure for area.  \
Please use one  these units of measure:  \"sqm\" (for square meters), or \
\"sqmi\" (for square miles),  or \"ac\" (for acres), or \"hec\" (for hectares)",
            G_program_name(),opt_area_uom->answer );
    G_fatal_error (msg);
    exit(1);
   }

/* Make sure that the current projection is UTM or   */
/* Unreferenced XY projection.                       */
  if ((G_projection() != 0)&&(G_projection() != 1) &&(G_projection() != 99))
   {
    char msg[256];      
    sprintf(msg,"%s:  Projection must be either Unreferenced XY (value 0) or \
UTM (value 1).  Change the value \"proj\" in file \"WIND\" to either \
0 or 1 and then re-executed \"%s\".",
    G_program_name(), G_program_name() );
    G_fatal_error (msg);
    exit(1);
   }

  /* Provide warning message if both "radius" and "area" are */
  /* greater than zero.                                      */
  if ( (radius > 0.0)&&(area > 0.0) ) 
   {
    fprintf(stderr,"WARNING:\n\
%s: Both \"radius\" and \"area\" values are valid.  The \"radius\"\n\
value will be used for making the calculations.\n", G_program_name() );
   }

  /* Check "radius" and "area" values. */
  if ( (radius == 0.0)&&(area == 0.0) ) 
   {
    char msg[256];      
    sprintf(msg,"%s: Neither \"radius\" or \"area\" have values.  Please \
provide a correct value for one of them.\n",
            G_program_name() );
    G_fatal_error (msg);
    exit(1);
   }
  else
   {
    if (radius != 0.0)
     {
      /* Make sure that radius is not less than 0.0 . */
      if (radius < 0.0)
       {
        char msg[256];      
        sprintf(msg,"%s: \"%s\" is an incorrect value for radius.\n",
                G_program_name(),opt_radius->answer );
        G_fatal_error (msg);
        exit(1);
       }
      if (strcmp(radius_uom,"m")==0)
       {
        /* "radius" is in "meters". */
        /* area = PI * ((radius)*(radius)) */
        area = (double)M_PI * (pow(radius,(double)2.0)); 
        fprintf(stderr,"INFORMATION:\nRadius = %f meters  \
Area = %f square meters\n",radius,area);
       }
      else
       {
        if (strcmp(radius_uom,"ft")==0)
         {
          fprintf(stderr,"INFORMATION:\nRadius = %f feet  \
Area = %f square feet\n",radius,((double)M_PI*(pow(radius,(double)2.0))) );
          /* Convert "ft" to "m".     */ 
          /* Change "radius" from "feet" to "meters". */
          /*
            1 meter        "radius" feet
          ------------  X 
          3.28083 feet
          */
          radius = radius / (double)3.28083;
          area = (double)M_PI * (pow(radius,(double)2.0)); 
          fprintf(stderr,"Radius = %f meters  \
Area = %f square meters\n",radius,area);
         }
        else
         {
          if (strcmp(radius_uom,"mi")==0)
           {
            fprintf(stderr,"INFORMATION:\nRadius = %f miles  \
Area = %f square miles\n",radius,((double)M_PI*(pow(radius,(double)2.0))) );
            /* Convert "mi" to "m".   */
            /* Change "radius" from "miles" to "meters". */
            /*
              1 meter         5280 feet     "radius" miles
            ------------ X  ------------ X  
            3.28083 feet       1 mile
            */
            radius = (radius * (double)5280.0) / (double)3.2808;
            /* area = PI * ((radius)*(radius)) */
            area = (double)M_PI * (pow(radius,(double)2.0)); 
            fprintf(stderr,"Radius = %f meters  \
Area = %f square meters\n",radius,area);
           }
         }
       }
     }
    else
     {
      /* area != 0.0 */
      /* Make sure that area is not less than 0.0 . */
      if (area < 0.0)
       {
        char msg[256];      
        sprintf(msg,"%s: \"%s\" is an incorrect value for area.\n",
                G_program_name(),opt_area->answer );
        G_fatal_error (msg);
        exit(1);
       }
      if (strcmp(area_uom,"sqm")==0)
       {
        fprintf(stderr,"INFORMATION:\nRadius = %f meters  \
Area = %f square meters\n",pow((area/(double)M_PI),(double)0.5),area );
        /* "area" is in "square meters".       */
        /* area = PI * ((radius)*(radius))     */
        /* radius = square root of (area / PI) */
        radius = area/(double)M_PI;
        radius = pow(radius,(double)0.5); 
       }
      else
       {
        if (strcmp(area_uom,"ac")==0)
         {
          fprintf(stderr,"INFORMATION:\nRadius = %f feet  \
Area = %f acre(s)\n",pow(((area*43560.0)/(double)M_PI),(double)0.5),area );
          /* Convert "ac" to "sqm". */ 
          /* Change "area" from "acres" to "square meters". */
          /*
          43560.0 square feet     1 square meter     "area" acres
          -------------------  X  --------------   X  
              1 acre              10.763845 sq.ft.
          */
          area = ((double)43560.0 * area) / (double)10.763845;  
          /* radius = square root of (area / PI) */
          radius = area/(double)M_PI;
          radius = pow(radius,(double)0.5); 
          fprintf(stderr,"Radius = %f meters  \
Area = %f square meters\n",pow((area/(double)M_PI),(double)0.5),area );
         }
        else
         {
          if (strcmp(area_uom,"sqmi")==0)
           {
            fprintf(stderr,"INFORMATION:\nRadius = %f miles  \
Area = %f square miles\n",pow((area/(double)M_PI),(double)0.5),area );
            /* Convert "sqmi" to "sqm". */
            /* Change "area" from "square miles" to "square meters". */
            /*
  640 acres     43560.0 square feet     1 square meter     "area" sq. miles
 ----------- X  -------------------  X  --------------   X  
  1 sq. mile        1 acre              10.763845 sq.ft.
            */
            area = ((((double)640.0 * area) * (double)43560.0))
                   / (double)10.763845;
            /* radius = square root of (area / PI) */
            radius = area/(double)M_PI;
            radius = pow(radius,(double)0.5); 
            fprintf(stderr,"Radius = %f meters  \
Area = %f square meters\n",pow((area/(double)M_PI),(double)0.5),area );
           }
          else
           {
            if (strcmp(area_uom,"hec")==0)
             {
              fprintf(stderr,"INFORMATION:\nRadius = %f meters  \
Area = %f hectares\n",pow(((area*10000.0)/(double)M_PI),(double)0.5),area );
              /* Convert "hec" to "sqm". */
              /*
                10000.0 square meters    "area" hectares
                --------------------- X 
                    1 hectare          
              */
              area = area * (double)10000.0;
              /* radius = square root of (area / PI) */
              radius = area/(double)M_PI;
              radius = pow(radius,(double)0.5); 
              fprintf(stderr,"Radius = %f meters  \
Area = %f square meters\n",pow((area/(double)M_PI),(double)0.5),area );
             }
           }
         }
       }
     }
   }

  /* input file name */
  strcpy(input,opt_input->answer);

  /* output file name */
  strcpy(output,opt_output->answer);

  /* Obtain the mapset name for the chosen site_lists file "input". */
  if ((site_mapset = G_find_file("site_lists",input,""))==NULL)
   {
    char msg[256];
    sprintf(msg,"%s: Site_lists file: <%s> does not exist.",
    G_program_name(), input);
    G_fatal_error (msg);
    exit(1);
   }

  /* Get "mapset". */
  vect_mapset = G_mapset();

  fqn = fqn_output;
  fqn = G_fully_qualified_name(output,vect_mapset);
   
  /* Open site_lists file. */
  fd_site = G_fopen_sites_old(input,site_mapset);
  if (fd_site == NULL)
   {
    char msg[256];
    sprintf(msg,"%s: Unable to open site_lists file <%s> in mapset \"%s\".",
    G_program_name(),output,site_mapset );
    G_fatal_error (msg);
    exit(1);
   }

  if ( (Vect_open_new(&map,output) < 0) )
   {
    char msg[256];
    sprintf(msg,"%s: Unable to open vector file <%s> in mapset \"%s\".",
    G_program_name(),output,vect_mapset );
    G_fatal_error (msg);
    exit(1);
   }

  /* Provide warning message if projection is Unreferenced XY. */
  if (G_projection() == 0)
   {
    fprintf(stderr,"WARNING:\n\
%s:  The coordinates for the vector output file\n\
<%s> will be in meters.\n",
    G_program_name(), output );
   }

  pnts = Vect_new_line_struct();

  /* Calculate "theta". "theta" is the value in radians of 1 degree of angle. */
  theta = ((double)2.0 * (double)M_PI) / (double)360.0;

  once = 0;
  count = 0;

   if ((nsites = readsites (fd_site,1,1,1, &bsite))==0) {
       G_fatal_error("No sites found.");
   } 
   else {
       fprintf(stderr, "%i sites found\n",nsites);
   }

   /* write atts file */
      f_att = G_fopen_new( "dig_att", output);
      if (f_att == NULL)
          G_fatal_error("Unable to create attribute file.");
         
   /*     Create empty dig_cats file            */
   G_init_cats( (CELL)0,"",&cats);
   if (G_write_vector_cats(output, &cats) != 1)
       G_fatal_error("Writing dig_cats file");

  for (i1=0;i1<nsites;i1++)
   {
    for (deg=0; deg <= 360; deg++)
     {
      e =  radius * cos( (double)(deg) * theta ); 
      n =  radius * sin( (double)(deg) * theta ); 
      x[deg] = bsite[i1].x + e;
      y[deg] = bsite[i1].y + n;
      


      if (once==0)
       {
        max_x = x[deg];
        min_x = x[deg];
        max_y = y[deg];
        min_y = y[deg];
        once = 1;
       }
      if (x[deg] > max_x) max_x = x[deg];
      if (x[deg] < min_x) min_x = x[deg];
      if (y[deg] > max_y) max_y = y[deg];
      if (y[deg] < min_y) min_y = y[deg];
     }
    Vect_copy_xy_to_pnts(pnts,x,y,(int)361);
    Vect_write_line(&map,AREA,pnts);
    count += 1;
   }
 
  /* cycle again through the sites list */
  count = 0;
    
  for (i1=0;i1<nsites;i1++)
   {
      sprintf(catbuffer, "%g", bsite[i1].z); /* use sites z-value as cat */

      /* write att file */
      fprintf( f_att, "A  %-12f  %-12f  %s \n",
      		   bsite[i1].x, bsite[i1].y, catbuffer);
      		   
      /* copy z value from sites as vector cat */
      if (G_set_cat(i1+1, catbuffer, &cats) != 1)
         G_fatal_error("Error setting category in dig_cats");
         
      count += 1;
   }
   
  /* update cats file with new values */
  G_write_vector_cats(output, &cats) != 0;                                

  /* Initialize "dig_head" structure "local_head" with vector info. */
  local_head = (struct dig_head *) G_malloc (sizeof(struct dig_head)); 
  Date(today);
  strcpy(local_head->organization,"");
  strcpy(local_head->date,today);
  strcpy(local_head->your_name,"");
  sprintf(local_head->map_name,
"File created by \"%s\".", G_program_name() );
  strcpy(local_head->source_date,"");
  /* "orig_scale" arbitrarily set to 24000 */
  local_head->orig_scale = (long) 24000;
  sprintf(local_head->line_3,
"Center(s) of circle(s) from \"%s\".",
          input );
  local_head->plani_zone = G_zone();
  local_head->W = min_x;
  local_head->E = max_x;
  local_head->S = min_y;
  local_head->N = max_y;
  /* "digit_thresh" arbitrarily set to 0 */
  local_head->digit_thresh = 0.0;
  /* "map_thresh" arbitrarily set to 0 */
  local_head->map_thresh = 0.0;
  /* Copy contents of "local_head" to "map.head". */
  Vect_copy_head_data(local_head,&map.head);
  /* "Vect_close" will write the header for the vector file. */
  Vect_close(&map);
  fclose(fd_site);
  
  /* Provide warning message if no site points were retrieved. */
  if ( count == 0 )
   {
    fprintf(stderr,"WARNING:\n\
No points were successfully read from \"site_lists\" file <%s>.\n\
Vector file <%s> has no circles in it.\n",  
    input, output);
    exit(1);
   }
  else
   {
    if ( count == 1 )
     {
      /* Provide user with number of circles produced in vector file. */
      fprintf(stderr,
      "%ld circle was produced in vector file <%s>.\n",
      count, output);
     }
    else
     {
      /* Provide user with number of circles produced in vector file. */
      fprintf(stderr,
      "%ld circles were produced in vector file <%s>.\n",
      count, output);
     }
   }

  /* If "-s" flag is passed as argument then run "v.support" on */
  /* newly created vector file (output).                        */
  if (flag->answer == 0x01)
   {
    fprintf(stderr, "Creating support file...");
    sleep(8); /* sleep to avoid timing problems */
    sprintf(command,"%s/bin/v.support map=%s",G_gisbase(), output );
    system(command);
   }
  exit(0);
 }
