#include "gis.h"
#include "display.h"
#include "D.h"
#include "raster.h"

int main(int argc,char *argv[])
{
	struct Option *color;
	char name[256];
	char *err;

	G_gisinit(argv[0]);

	color = G_define_option();
	color->key = "color";
	color->type = TYPE_STRING;
	color->required = NO;
	color->answer = "black";	/* set default color! */
	color->description = "Color to erase with";

	if (argc > 1 && G_parser(argc, argv))
		exit(1);

	R_open_driver();

	err = NULL;
	if (D_get_cur_wind(name))
		err = "No current frame" ;
	else if (D_set_cur_wind(name))
		err = "Current frame not available" ;
	else
		Derase(color->answer) ;

	R_close_driver();
	if (err)
		G_fatal_error(err) ;

	exit(0);
}
