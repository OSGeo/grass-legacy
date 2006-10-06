#include <stdlib.h>
#include <grass/gis.h>
#include <grass/display.h>
#include <grass/raster.h>
#include <grass/glocale.h>
#define MAIN
#include "local_proto.h"

int main (int argc, char **argv)
{
	char frame[64] ;
	struct GModule *module;
	struct
	{
	   struct Option *c1;
	   struct Option *c2;
	   struct Flag *s;
	   struct Flag *m;
	   struct Flag *k;
	} parm;
	int color1, color2, s_flag, m_flag, k_flag;

	/* Initialize the GIS calls */
	G_gisinit(argv[0]) ;

	module = G_define_module();
	module->keywords = _("display");
    module->description =
		_("Measures the lengths and areas of features drawn "
		"by the user in the active display frame on the "
		"graphics monitor.");

	parm.c1 = G_define_option();
	parm.c1->key = "c1";
	parm.c1->description = _("Line color 1");
	parm.c1->type = TYPE_STRING;
	parm.c1->required = NO;
	parm.c1->options=D_color_list();
	parm.c1->answer = DEFAULT_BG_COLOR;

	parm.c2 = G_define_option();
	parm.c2->key = "c2";
	parm.c2->description = _("Line color 2");
	parm.c2->type = TYPE_STRING;
	parm.c2->required = NO;
	parm.c2->options=D_color_list();
	parm.c2->answer = DEFAULT_FG_COLOR;

	parm.s = G_define_flag();
	parm.s->key = 's';
	parm.s->description = _("Suppress clear screen");

	parm.m = G_define_flag();
	parm.m->key = 'm';
	parm.m->description = _("Output in meters only");

	parm.k = G_define_flag();
	parm.k->key = 'k';
	parm.k->description = _("Output in kilometers as well");
	
	if (argc > 1 && G_parser(argc,argv))
	    exit(EXIT_FAILURE);

	if (R_open_driver() != 0)
	    G_fatal_error (_("No graphics device selected"));

	if (D_get_cur_wind(frame))
	    G_fatal_error(_("No current frame"));

	if (D_set_cur_wind(frame))
	    G_fatal_error(_("Current frame not available"));

	color1 = D_translate_color (parm.c1->answer);
	color2 = D_translate_color (parm.c2->answer);
	s_flag = parm.s->answer;
	m_flag = parm.m->answer;
	k_flag = parm.k->answer;

	measurements(color1, color2, s_flag, m_flag, k_flag ) ;

	R_close_driver();

	exit(EXIT_SUCCESS);
}
