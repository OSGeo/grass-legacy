/*
 *   v.out.mif
 *
 */

#include "gis.h"
#include "Vect.h"
#define MAIN

struct Cell_head window;
int linecolor;

int main(argc, argv)
int argc ;
char **argv ;
{
	char *mapset ;
	char *color_mapset;
	char buf[128] ;
	char window_name[64];
	int i, stat ;
	int catval = 0;
	int t,b,l,r;
	char map_name[128],filename[128], color_name[128];
	FILE *cf;
	struct Option *opt1,*opt2, *opt3;
	struct line_pnts *Points;
	struct Colors colors;
	struct Categories Mapcats;

	opt1 = G_define_option() ;
	opt1->key        = "input" ;
	opt1->type       = TYPE_STRING ;
	opt1->required   = YES ;
	opt1->multiple   = NO ;
	opt1->description= "Name of existing vector map to be exported" ;

	opt2 = G_define_option() ;
	opt2->key        = "output" ;
	opt2->type       = TYPE_STRING ;
	opt2->required   = NO ;
	opt2->multiple   = NO ;
	opt2->description= "Prefix for MIF/MID file " ;

	opt3 = G_define_option() ;
	opt3->key        = "color" ;
	opt3->type       = TYPE_STRING ;
	opt3->required   = NO ;
	opt3->multiple   = NO ;
	opt3->description= "Name of colortable" ;


	G_gisinit(argv[0]);

	/* Check command line */
	if (G_parser(argc, argv))
		exit(-1);

	strcpy(map_name, opt1->answer);

	if (opt2->answer == NULL) 
		strcpy(filename, opt1->answer);
	else
		strcpy(filename, opt2->answer);

	if (opt3->answer != NULL)
	{
		strcpy(color_name, opt3->answer);
 		color_mapset=G_find_file("colr",color_name,"");
        	sprintf(buf,"Colortable %s not found",color_name);
        	if (color_mapset == NULL) G_fatal_error(buf);
        	if (G_read_colors(color_name, color_mapset, &colors) == -1)
                	G_fatal_error("Colortable not available");
	} else
		colors.version = -9;
	
	/* Make sure map is available */
	mapset = G_find_file2 ("dig", map_name, "") ;
	if (mapset == NULL)
	{
		sprintf(buf,"Vector file [%s] not available", map_name);
		G_fatal_error(buf) ;
		exit(-1);
	}

	if (G_read_vector_cats(map_name, mapset, &Mapcats) < 0)
		Mapcats.num = -1;

	G_get_window(&window) ;
	Points = Vect_new_line_struct ();

	stat = plot (map_name, mapset, Points, filename, &colors, &Mapcats);
	if(stat == 0)
		D_add_to_list(G_recreate_command()) ;

	Vect_destroy_line_struct (Points);

	exit(stat);
}

/* NULL function to bypass debugf() in dig library */
debugf() {}
