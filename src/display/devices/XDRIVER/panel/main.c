#include "gis.h"
main()
{
    char *tmp;
    tmp = G_tempfile();
    if (R_open_driver() != 0)
	    G_fatal_error ("No graphics device selected");
    R_panel_save(tmp,
	    R_screen_top(), R_screen_bot(),
	    R_screen_left(),R_screen_rite());
    R_close_driver();
    unlink (tmp);
}
