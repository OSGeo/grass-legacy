#include "gis.h"

#define UNIT_FILE "UNITS"
#define PROJECTION_FILE "PROJ_INFO"

main(argc, argv)
    int argc ;
    char **argv ;
{
    struct Option *spheroid;
    struct Flag *once;
    char s_names[2048];
    char *name;
/*-->*/ int in_stat, cmd_cnt;
	char buff[200], ipath[256], cmnd[200], *value;
        struct Key_Value *in_proj_keys;

    char *CC_spheroid_name();
    int have_spheroid;
    int with_info;
    double a,e;
    int i;

/* Initialize the GIS calls */
    G_gisinit(argv[0]) ;

    once = G_define_flag() ;
    once->key        = '1' ;
    once->description= "one mouse click only";

    spheroid = G_define_option() ;
    spheroid->key        = "spheroid" ;
    spheroid->type       = TYPE_STRING ;
    spheroid->required   = NO ;
    spheroid->description= "Name of a spheroid (for lat/lon coordinate conversion)";
    spheroid->options    = s_names;

    *s_names = 0;
    for (i = 0; name = CC_spheroid_name(i); i++)
    {
	if(i) strcat (s_names, ",");
	strcat (s_names,name);
    }


    if (argc > 1 && G_parser(argc,argv))
	exit(1);

    have_spheroid = 0;
    if (G_projection() != PROJECTION_UTM)
       {
           /* get input projection parameters */
       G__file_name (ipath, "", PROJECTION_FILE, G_mapset());
       if (access(ipath,0) != 0)
           {
           sprintf(cmnd,"Mapset PROJECTION_INFO file not found \n");
           G_fatal_error(cmnd) ;
           }
       in_proj_keys = G_read_key_value_file(ipath,&in_stat);
       if (in_stat != 0)
           {
           sprintf(cmnd,"Current mapset PROJECTION_INFO not found\n");
           G_fatal_error(cmnd) ;
           }
                /* read values and create input command buffer */
       cmnd[0] = '\0';
       value = G_find_key_value("name",in_proj_keys);
       if (strcmp(value,"Lat/Long") != 0)
           {
           for (i=1; i<=in_proj_keys->nitems-1; i++)
              {
              sprintf(buff,"+%s=%s\t",
              in_proj_keys->key[i],in_proj_keys->value[i]);
              strcat(cmnd,buff);
              }
           strcat (cmnd, "+inv\0");
           cmd_cnt = in_proj_keys->nitems + 1;
           }
       set_proj(cmnd,1);
       have_spheroid = 1;
       }
    else
       {
       if(name = spheroid->answer)
          {
	  if (CC_get_spheroid (name, &a, &e) == NULL)
	    {
	         fprintf (stderr,"ERROR: %s=%s: unknown spheroid\n",
		     spheroid->key, spheroid->answer);    
	         exit(-1);
	    }
	  else
	    {
	    CC_u2ll_spheroid_parameters (a,e);
	    CC_u2ll_zone (G_zone());
	    have_spheroid = 1;
	    }
 	  }
       }

    R_open_driver();
    D_setup(0);
    where_am_i(once->answer, have_spheroid) ;
    R_close_driver();

    exit(0);
}
