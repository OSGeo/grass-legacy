/*======================================================================
			    d.sites.label
======================================================================*/
/* $Id$ */

#include <stdlib.h>
#include <gis.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include "raster.h"
#include "display.h"
#include "local_proto.h"

#define DEFAULT_COLOR "green"
#define DEFAULT_FONT "romanp"
#define DEFAULT_WIDTH "10"
#define DEFAULT_HEIGHT "10"
#define DEFAULT_OFFSET "5"
#define DEFAULT_PLACEMENT "c"
#define DEFAULT_ATTRIBUTE "string"
#define DEFAULT_INDEX "1"

void my_attr_copy(char *theText, Site *theSite, int attr, int index) {
	switch (attr) {
		case SITE_ATTR_CAT:
			if (theSite->cattype == CELL_TYPE)
				snprintf(theText, MAX_SITE_STRING,
						"%d", theSite->ccat);
			else if (theSite->cattype == FCELL_TYPE)
				snprintf(theText, MAX_SITE_STRING,
						"%f", theSite->fcat);
			else if (theSite->cattype == DCELL_TYPE)
				snprintf(theText, MAX_SITE_STRING,
						"%lf", theSite->dcat);
			else
				G_fatal_error("No categories in site file!\n");
			break;
		case SITE_ATTR_STR:
			if (theSite->str_att == NULL)
				G_fatal_error("No string attributes!\n");
			if (theSite->str_alloc <= index)
				G_fatal_error("String index out of range!\n");
			G_strncpy(theText, theSite->str_att[index], 
					MAX_SITE_STRING);
			break;
		case SITE_ATTR_DBL:
			if (theSite->dbl_att == NULL)
				G_fatal_error("No double attributes!\n");
			if (theSite->dbl_alloc <= index)
				G_fatal_error("Double index out of range!\n");
			snprintf(theText, MAX_SITE_STRING,
					"%lf", theSite->dbl_att[index]);
			break;
		default:
			G_fatal_error("Wrong or unknown attribute type!\n");
	}
}

int main(int argc, char *argv[])
{
  struct Option *site_opt, *anchor_opt, *color_opt, *font_opt;
  struct Option *width_opt, *height_opt, *attr_opt, *index_opt;
  char *sitefile;
  char *mapset;
  FILE *site_fd;
  double east, north;
  char desc[MAX_SITE_STRING];
  int x,y, nDims, nStrs, nDbls;
  int color,index, attrib;
  int top,bottom,left,right;
  char window_name[64];
  struct Cell_head region;
  char *anchor;
  int width, height;
  char *font;
  Site *theSite;
  RASTER_MAP_TYPE maptype;

  G_gisinit(argv[0]);

  site_opt = G_define_option();
  site_opt->key = "sitefile";
  site_opt->type = TYPE_STRING;
  site_opt->required = YES;
  site_opt->description = "Site file to display labels for.";

  attr_opt = G_define_option();
  attr_opt->key = "attr";
  attr_opt->type = TYPE_STRING;
  attr_opt->required = NO;
  attr_opt->description = "Type of attribute to use for labels";
  attr_opt->options = "string,cat,double";
  attr_opt->answer  = DEFAULT_ATTRIBUTE;

  index_opt = G_define_option();
  index_opt->key = "index";
  index_opt->type = TYPE_STRING;
  index_opt->required = NO;
  index_opt->description = "Index of attribute. Ignored when attr=cat.";
  index_opt->answer = DEFAULT_INDEX;
  
  anchor_opt = G_define_option();
  anchor_opt->key = "anchor";
  anchor_opt->type = TYPE_STRING;
  anchor_opt->required = NO;
  anchor_opt->description = "Direction beside site to place text";
  anchor_opt->answer = DEFAULT_PLACEMENT;
  anchor_opt->options = "c,n,s,e,w,ne,nw,se,sw";

  color_opt = G_define_option();
  color_opt->key = "color";
  color_opt->type = TYPE_STRING;
  color_opt->required = NO;
  color_opt->description = "Color to display text in";
  color_opt->answer = DEFAULT_COLOR;

  font_opt = G_define_option();
  font_opt->key = "font";
  font_opt->type = TYPE_STRING;
  font_opt->required = NO;
  font_opt->description = "Font to display text in";
  font_opt->answer = DEFAULT_FONT;

  width_opt = G_define_option();
  width_opt->key = "width";
  width_opt->type = TYPE_INTEGER;
  width_opt->required = NO;
  width_opt->description = "Width of letters";
  width_opt->answer = DEFAULT_WIDTH;

  height_opt = G_define_option();
  height_opt->key = "height";
  height_opt->type = TYPE_INTEGER;
  height_opt->required = NO;
  height_opt->description = "Height of letters";
  height_opt->answer = DEFAULT_HEIGHT;


  if(G_parser(argc, argv))
    exit(1);

  sitefile = site_opt->answer;
  mapset = G_find_file("site_lists", sitefile, "");
  
  if(strcmp(attr_opt->answer, "string") == 0) {
	  attrib = SITE_ATTR_STR;
  }
  else if (strcmp(attr_opt->answer, "cat") == 0) {
	  attrib = SITE_ATTR_CAT;
  }
  else if (strcmp(attr_opt->answer, "double") == 0) {
	  attrib = SITE_ATTR_DBL;
  }
  else {
	  G_fatal_error("Unknown attribute type!\n");
  }
  
  index = atoi(index_opt->answer) - 1;
  if (index < 0) {
	  G_fatal_error("Index must be a positive number greater than zero!\n");
  }
  
  width = atoi(width_opt->answer);
  height = atoi(height_opt->answer);
  font = font_opt->answer;
  color = D_translate_color(color_opt->answer);
  if(color == 0) {
    color = 1;
  }

  if(mapset == NULL) {
    fprintf(stderr, "d.sites.label: Could not open sites file %s.\n",
	    sitefile);
    exit(1);
  }

  anchor = anchor_opt->answer;

  site_fd = G_fopen_sites_old(sitefile, mapset);
  if(site_fd == NULL) {
    fprintf(stderr, "find_close_site: Could not open sites file %s\n",
	    sitefile);
    return(2);
  }

  G_get_set_window(&region);

  R_open_driver();
  D_setup(0);
  D_get_cur_wind(window_name);
  D_set_cur_wind(window_name);
  D_get_screen_window(&top, &bottom, &left, &right);
  R_set_window(top,bottom,left,right);

  if ( 0 != G_site_describe(site_fd, &nDims, &maptype, &nStrs, &nDbls)) {
	  G_fatal_error("Unable to get format of site file!\n");
  }
  if (NULL == (theSite = G_site_new_struct(maptype, nDims, nStrs, nDbls))) {
	  G_fatal_error("Unable to allocate site structure!\n");
  }
  while(0 == G_site_get(site_fd, theSite)) {
    east = theSite->east;
    north = theSite->north;
    if(east<region.west || east>region.east ||
       north<region.south || north>region.north)
      ;
    else {
      x = (int)D_u_to_d_col(east);
      y = (int)D_u_to_d_row(north);
      my_attr_copy(desc, theSite, attrib, index);
      draw_site_label(x,y,5,width,height,color,font,desc,anchor);
    }
  }
  R_close_driver();

  return 0;
}
