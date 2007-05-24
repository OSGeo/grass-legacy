/*  
 ****************************************************************************
 *
 * MODULE:       g.message 
 * AUTHOR(S):    Jachym Cepicky - jachym AT les-ejk cz
 * PURPOSE:      Provides a means of reporting the contents of GRASS
 *               projection information files and creating
 *               new projection information files.
 * COPYRIGHT:    (C) 2007 by the GRASS Development Team
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
    struct Flag *warning, *fatal, *debug, *verbose;
    struct Option *message;
    struct GModule *module;
   
    G_gisinit(argv[0]);

    module = G_define_module();
    module->keywords = _("general");
    module->label =
	_("Prints message, warning or fatal error the GRASS way.");
    module->description =
	_("This module should be used in scripts for messages served to user.");

    warning = G_define_flag();
    warning->key = 'w';
    warning->guisection = "Input";
    warning->description =
	_("Print message as GRASS warning");

    fatal = G_define_flag();
    fatal->key = 'e';
    fatal->guisection = "Input";
    fatal->description =
	_("Print message as GRASS fatal error");

    debug = G_define_flag();
    debug->key = 'd';
    debug->guisection = "Input";
    debug->description =
	_("Print message as GRASS debug message (level 1)");

    verbose = G_define_flag();
    verbose->key = 'v';
    verbose->guisection = "Input";
    verbose->description =
	_("Print message only if in verbose mode");

    message = G_define_option();
    message->key = "message";
    message->type = TYPE_STRING;
    message->key_desc = "string";
    message->required = YES;
    message->guisection = "Input";
    message->description = _("Text of the message to be printed");

    if (G_parser(argc, argv))
	exit(EXIT_FAILURE);


    if(fatal->answer + warning->answer + debug->answer + verbose->answer > 1)
	G_fatal_error(_("Select only one message level."));

    /* Initialisation & Validation */

    if(fatal->answer)
	G_fatal_error(message->answer);
    else if(warning->answer)
	G_warning(message->answer);
    else if(debug->answer)
	G_debug(1,message->answer);
    else if(verbose->answer) {
	if( G_verbose() > G_verbose_std() )
	    G_message(message->answer);
    }
    else
        G_message(message->answer);
   
    exit(EXIT_SUCCESS);
}
