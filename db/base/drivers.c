#include <stdlib.h>
#include "codes.h"
#include "dbmi.h"
#include "codes.h"
#include "gis.h"
#include "glocale.h"

void parse_command_line();

struct {
	int f;
} parms;


int
main(int argc, char *argv[])
{
    dbDbmscap *list, *p;

    parse_command_line (argc, argv);

    list = db_read_dbmscap();
    if (list == NULL) {
      G_message ( _("Error trying to read dbmscap file\n"));
      exit(ERROR);
    }

    for (p = list; p; p = p->next) {
      G_message ( _("%s"), p->driverName);
      if (parms.f) G_message ( _(":%s"), p->comment);
      G_message ("\n");
    }
    exit(OK);
}

void
parse_command_line (int argc, char *argv[])
{
    struct Flag *full, *print;
    struct GModule *module;

    /* Initialize the GIS calls */
    G_gisinit(argv[0]) ;

    full = G_define_flag();
    full->key = 'f';
    full->description = _("Full output");

    print = G_define_flag();
    print->key               = 'p';
    print->description       = _("print tables and exit");    

    /* Set description */
    module              = G_define_module();
    module->description = _("List all database drivers.");

    if (G_parser(argc, argv))
	exit(ERROR);

    parms.f = full->answer;
}
