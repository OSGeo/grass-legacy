/*======================================================================
			    d.sites.label
======================================================================*/
#include <stdlib.h>
#include <gis.h>
#include <stdio.h>
#include <math.h>

#define DEFAULT_COLOR "green"
#define DEFAULT_FONT "romanp"
#define DEFAULT_WIDTH "10"
#define DEFAULT_HEIGHT "10"
#define DEFAULT_OFFSET "5"
#define DEFAULT_PLACEMENT "c"

main(argc, argv)
int argc;
char *argv[];
{
  struct Option *site_opt, *anchor_opt, *color_opt, *font_opt;
  struct Option *width_opt, *height_opt;
  char *sitefile;
  char *mapset;
  FILE *site_fd;
  double east, north;
  char *desc;
  int x,y;
  int color;
  int top,bottom,left,right;
  char window_name[64];
  struct Cell_head region;
  char *anchor;
  int width, height;
  char *font;

  double D_u_to_d_row(double), D_u_to_d_col(double);
  int draw_site_label(int x,
		      int y,
		      int offset,
		      int width,
		      int height,
		      int color,
		      char *font,
		      char *text,
		      char *placement);

  G_gisinit(argv[0]);

  site_opt = G_define_option();
  site_opt->key = "sitefile";
  site_opt->type = TYPE_STRING;
  site_opt->required = YES;
  site_opt->description = "Site file to display labels for.";

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

  while(-1 != G_get_site(site_fd, &east, &north, &desc)) {
    if(east<region.west || east>region.east ||
       north<region.south || north>region.north)
      ;
    else {
      x = (int)D_u_to_d_col(east);
      y = (int)D_u_to_d_row(north);
      draw_site_label(x,y,5,width,height,color,font,desc,anchor);
    }
  }
  R_close_driver();
  exit(0);
}
