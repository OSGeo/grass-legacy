/*
* $Id$
*
****************************************************************************
*
* MODULE:       d.vect.labels
* AUTHOR(S):    Code modified from d.site.labels
*               Stefano Merler <merler@itc.it>
* PURPOSE:      
* COPYRIGHT:    (C) 2001 by the GRASS Development Team
*
*               This program is free software under the GNU General Public
*               License (>=v2). Read the file COPYING that comes with GRASS
*               for details.
*
* METHOD:       The implementation is a bit "trivial": It extracts the
*               cats,strings or coordinates into a sites list and displays
*               them using code from d.site.labels.
*
*****************************************************************************/

#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include "gis.h"
#include "site.h"
#include "Vect.h"
#include "local_proto.h"

/* uncomment for debug output */
/*#define DEBUG*/

#define DEFAULT_ATTRIBUTE "string"
#define D_COLOR_LIST "red,orange,yellow,green,blue,indigo,white,black,brown,magenta,aqua,gray,grey"

int main (int argc, char **argv)
{
  struct Option *vect_opt, *color_opt, *size_opt, *backgr_opt;
  struct Option *border_opt, *font_opt, *col_opt;
  struct Flag *flag_s, *flag_m, *flag_v;
  struct GModule *module;
  char *mapset;
  struct Categories Cats;
  char *cat1;
  struct Map_info Map;
  int level;
  char temp[500];
  int i, column, index;
  char name_tmp_site_file[500];
  char full_path_name_tmp_site_file[500];
  FILE *fpout;
  FILE *infile ;
  int t, b, l, r ;
  struct Cell_head window ;
  char buff[128] ;
  char position[MAX_SITE_STRING];
  char window_name[64] ;
  int mouse;

  G_gisinit(argv[0]) ;
  
  module = G_define_module();
  module->description =
    "Labels vectors using the attribute value or the category value.";
  
  vect_opt = G_define_option() ;
  vect_opt->key        = "map" ;
  vect_opt->type       = TYPE_STRING ;
  vect_opt->required   = YES ;
  vect_opt->gisprompt  = "old,dig,vector" ;
  vect_opt->description= "Name of vector file" ;

  col_opt = G_define_option();
  col_opt->key     = "attr";
  col_opt->type    = TYPE_STRING;
  col_opt->required = NO;
  col_opt->description = "Type of attribute to use for labels";
  col_opt->options = "string,cat,coords";
  col_opt->answer  = DEFAULT_ATTRIBUTE;

  size_opt = G_define_option() ;
  size_opt->key        = "size" ;
  size_opt->type       = TYPE_DOUBLE ;
  size_opt->required   = NO ;
  size_opt->answer     = "10";
  size_opt->description= "Size of text (pixels)" ;

  color_opt = G_define_option() ;
  color_opt->key        = "color" ;
  color_opt->type       = TYPE_STRING ;
  color_opt->required   = NO ;
  color_opt->options    = D_COLOR_LIST;
  color_opt->answer     = "white";
  color_opt->description= "Text color" ;

  backgr_opt = G_define_option() ;
  backgr_opt->key        = "backgr" ;
  backgr_opt->type       = TYPE_STRING ;
  backgr_opt->required   = NO ;
  backgr_opt->answer     = "none" ;
  backgr_opt->options    = "none," D_COLOR_LIST;
  backgr_opt->description= "Background color" ;

  border_opt = G_define_option() ;
  border_opt->key        = "border" ;
  border_opt->type       = TYPE_STRING ;
  border_opt->required   = NO ;
  border_opt->answer     = "none" ;
  border_opt->options    = "none," D_COLOR_LIST;
  border_opt->description= "Border color" ;

  font_opt = G_define_option() ;
  font_opt->key        = "font" ;
  font_opt->type       = TYPE_STRING ;
  font_opt->required   = NO ;
  font_opt->options    = "cyrilc,gothgbt,gothgrt,gothitt,greekc,greekcs,greekp,"
    "greeks,italicc,italiccs,italict,romanc,romancs,romand,"
    "romans,romant,scriptc,scripts";
  font_opt->answer     = "romans" ;
  font_opt->description= "Fontname" ;


  flag_s              = G_define_flag();
  flag_s->key         = 's';
  flag_s->description = "save support sites file";

  flag_m              = G_define_flag();
  flag_m->key         = 'm';
  flag_m->description = "mark labels on the map";

  flag_v              = G_define_flag();
  flag_v->key         = 'v';
  flag_v->description = "Verbose mode";


  if (G_parser(argc, argv))
    exit(1);

  /* Index column and number
  * string: categories text labels
  * cat:    categories numbers
  * coords: categories labels coordinates
  */
  if(strcmp(col_opt->answer, "string") == 0) {
    column = SITE_ATTR_STR;
  }
  else if (strcmp(col_opt->answer, "cat") == 0) {
    column = SITE_ATTR_CAT;
  }
  else if (strcmp(col_opt->answer, "coords") == 0) {
    column = SITE_ATTR_COORD;
  }
  else {
    G_fatal_error("Unknown attribute type!\n");
  }

  /* find input vector file */
  mapset=G_find_vector2(vect_opt->answer,"");
  if (mapset == NULL){
    fprintf (stderr, "warning: %s - vector file not found\n", 
	     vect_opt->answer);
    exit(-1);
  }

  /* open vector file and  fill map info structure*/
  if (flag_v->answer)
  	fprintf(stderr, "Reading vector map...\n");
  level = Vect_open_old (&Map,vect_opt->answer , mapset);
  if (level < 0){
    sprintf(temp, "Vector file [%s] not available", vect_opt->answer);
    G_fatal_error (temp);
  }
  if (level < 2) {
    sprintf(temp, "%s: You must first run v.support on vector file", 
	    vect_opt->answer);
    G_fatal_error (temp);
  }

  /* read cats file */
  if (G_read_vector_cats(vect_opt->answer, mapset, &Cats) < 0)
    Cats.num = -1  ;

  if((Cats.num <= 0) && (column == SITE_ATTR_STR)){
    sprintf(temp, "No categories labels found. Using categories values");
    G_warning (temp);
    column = SITE_ATTR_CAT;
  }

  /* name (and full path name) of tmp sites file */
  sprintf(name_tmp_site_file,"TMP_%s_%s",G_whoami(),
	  G_date());
  G_strchg(name_tmp_site_file,' ','_');
  G_strchg(name_tmp_site_file,':','_');
  sprintf(full_path_name_tmp_site_file,"%s/%s/site_lists/%s",
	  G_location_path(),G_mapset(),name_tmp_site_file);
    
  /* build tmp sites file */
  /* next to come: use sites library!!!!!! */

  if (flag_v->answer)
	fprintf(stderr, "Writing (temporal) sites file for labels...\n");


  switch(column){
  case SITE_ATTR_CAT:  /* this is the cat num value */
    fpout=fopen(full_path_name_tmp_site_file,"w");
    for(i=1;i<=Map.n_atts;i++){
      if (flag_v->answer) G_percent(i,Map.n_atts,2);
      fprintf(fpout,"%f|%f|#%d\n",Map.Att[i].x,Map.Att[i].y,
	      Map.Att[i].cat);
    }
    fclose(fpout);
    break;
  case SITE_ATTR_STR:
#ifdef DEBUG
           fprintf(stderr,"Wir sind hier\n");
           fprintf(stderr,"Attributes number %d\n",Map.n_atts);
           fprintf(stderr,"Cats number %d\n",Cats.ncats);
#endif
    fpout=fopen(full_path_name_tmp_site_file,"w");
    for(i=1;i<=Map.n_atts;i++){
      if (flag_v->answer) G_percent(i,Map.n_atts,2);
      cat1 = G_get_cat(Map.Att[i].cat,&Cats);
#ifdef DEBUG
       fprintf(stderr,"Test0 att: %i: cat %s\n",i, cat1);
#endif
      /* this check is required to avoid crashes since @"" is not allowed */
      if ( (strlen(cat1)>0)  )
      {
#ifdef DEBUG
       fprintf(stderr,"att: %i: cat %s\n",i, cat1);
       fprintf(stderr,"%f|%f|#%d @\"%s\"\n",Map.Att[i].x,Map.Att[i].y,i,cat1);
#endif
       fprintf(fpout,"%f|%f|#%d @\"%s\"\n",Map.Att[i].x,Map.Att[i].y,i,cat1);
      }
    }
    fclose(fpout);
    break;
  case SITE_ATTR_COORD: 
    fpout=fopen(full_path_name_tmp_site_file,"w");
    for(i=1;i<=Map.n_atts;i++){
      if (flag_v->answer) G_percent(i,Map.n_atts,2);
      fprintf(fpout,"%f|%f|#%d\n",Map.Att[i].x,Map.Att[i].y,
	      Map.Att[i].cat);
    }
    fclose(fpout);
    break;
  default:
    break;
  }


  /* Open sites file */
  infile = G_fopen_sites_old (name_tmp_site_file, G_mapset()) ;
  if (infile == NULL)
    {
      sprintf(buff,"Can't open sitesfile [%s]", name_tmp_site_file);
      G_fatal_error(buff) ;
    }

  /* for compatibility with do_labels (orig from d.site.labels) */
     
  index = 0;
  mouse = 0;
  sprintf(position, "center bottom");

  /* begin... */

  if (R_open_driver() != 0)
    G_fatal_error ("No graphics device selected");

  if (D_get_cur_wind(window_name))
    G_fatal_error("No current window") ;

  if (D_set_cur_wind(window_name))
    G_fatal_error("Current window not available") ;

  /* Read in the map window associated with window */
  G_get_window (&window);
  if (D_check_map_window(&window))
    G_fatal_error("Setting map window") ;

  if (G_set_window(&window) == -1) 
    G_fatal_error("Current window not settable") ;

  /* Determine conversion factors */
  t = b = l = r = 0;
  if (D_get_screen_window(&t, &b, &l, &r))
    G_fatal_error("Getting screen window") ;
  if (D_do_conversions(&window, t, b, l, r))
    G_fatal_error("Error in calculating conversions") ;


  /* Go draw the cell file */
  if (flag_v->answer)
	fprintf(stderr, "Drawing labels...\n");
  do_labels(infile,window, position, color_opt->answer, size_opt->answer, 
            backgr_opt->answer, border_opt->answer, 
            font_opt->answer, column, index, mouse,flag_m->answer, name_tmp_site_file);

  D_add_to_list(G_recreate_command()) ;

  fclose(infile) ;

  R_close_driver();


  /* remove tmp sites file */
  if(!flag_s->answer){
    if (flag_v->answer) fprintf(stderr,"Removing temporal sites file\n");
    sprintf(temp, "g.remove sites=%s > /dev/null",name_tmp_site_file);
    system(temp);
  }else{
    fprintf(stderr,"\nSite file %s saved!\n\n",name_tmp_site_file);
  }
 

  return 0;
}

