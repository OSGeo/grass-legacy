/*  
 ****************************************************************************
 *
 * MODULE:       g.message 
 * AUTHOR(S):    Jachym Cepicky - jachym AT les-ejk cz
 * PURPOSE:      Provides a means of reporting the contents of GRASS
 *               projection information files and creating
 *               new projection information files.
 * COPYRIGHT:    (C) 2003-2007 by the GRASS Development Team
 *
 *               This program is free software under the GNU General Public
 *               License (>=v2). Read the file COPYING that comes with GRASS
 *               for details.
 *
 *****************************************************************************/

#include <stdlib.h>
#include <grass/gis.h>
#include <grass/glocale.h>

int main(int argc, char *argv[])
{
    struct Flag *warning, *fatal,*debug;	
    struct Option *message;
    struct GModule *module;
   
    G_gisinit(argv[0]);

    module = G_define_module();
    module->keywords = _("general");
    module->description =
	_("Prints message, warning or fatal error the GRASS way. This module should be used in scripts for messages served to user.");

    warning = G_define_flag();
    warning->key = 'w';
    warning->guisection = "Input";
    warning->description =
      _("Print GRASS warning");

    fatal = G_define_flag();
    fatal->key = 'f';
    fatal->guisection = "Input";
    fatal->description =
      _("Print GRASS fatal error");

    debug = G_define_flag();
    debug->key = 'd';
    debug->guisection = "Input";
    debug->description =
      _("Print GRASS debug message (level 1)");

    message = G_define_option();
    message->key = "message";
    message->type = TYPE_STRING;
    message->key_desc = "string";
    message->required = YES;
    message->guisection = "Input";
    message->description = _("Text of the message to be printed");

    if (G_parser(argc, argv))
	exit(EXIT_FAILURE);


    /* Initialisation & Validation */

    if(fatal->answer)
	G_fatal_error(message->answer);
    else if(warning->answer)
	G_warning(message->answer);
    else if(debug->answer)
	G_debug(1,message->answer);
    else
        G_message(message->answer);
   
    exit(EXIT_SUCCESS);
}
