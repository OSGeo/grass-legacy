/****************************************************************************
 *
 * MODULE:       g.gui
 *
 * AUTHOR(S):    Martin Landa <landa.martin gmail.com>
 *
 * PURPOSE:      Start GRASS GUI from command line.
 *
 * COPYRIGHT:    (C) 2008 by the GRASS Development Team
 *
 *               This program is free software under the GNU General Public
 *               License (>=v2). Read the file COPYING that comes with GRASS
 *               for details.
 *
 *****************************************************************************/

#include <stdlib.h>
#include <string.h>
#include <grass/gis.h>
#include <grass/glocale.h>

int main(int argc, char *argv[])
{
    struct Option *type;
    struct GModule *module;
    char *gui_type_env;

    G_gisinit(argv[0]);

    module = G_define_module();
    module->keywords = _("general, gui");
    module->description =
	_("Starts GRASS graphical user interface (GUI).");

    type = G_define_option();
    type->key = "type";
    type->type = TYPE_STRING;
    type->required = YES;
    type->description = _("GUI type");
    type->descriptions = _("oldtcltk;Old TCL/TK based GUI - Display Manager (d.m);"
			   "tcltk;TCL/TK based GUI - GIS Manager (gis.m);"
			   "wxpython;wxPython based new generation GUI (wxgui)");
    type->options = "oldtcltk,tcltk,wxpython";
    gui_type_env = G__getenv("GRASS_GUI");
    if (gui_type_env && strcmp(gui_type_env, "text")) {
	type->answer = G_store(gui_type_env);
    }
    else {
	type->answer = "tcltk";
    }

    if (argc > 1 && G_parser(argc, argv))
	exit(EXIT_FAILURE);

    if (gui_type_env) {
	if (strcmp(gui_type_env, type->answer)) {
	    G_message(_("<%s> is now default GUI"), type->answer);
	    G_setenv("GRASS_GUI", type->answer);
	}
    }

    if (strcmp(type->answer, "oldtcltk") == 0) {
	G_system("d.m");
    }
    else if (strcmp(type->answer, "tcltk") == 0) {
	G_system("gis.m");
    }
    else if (strcmp(type->answer, "wxpython") == 0) {
	G_system("wxgui");
    }

    exit(EXIT_SUCCESS);
}
