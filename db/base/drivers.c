#include "codes.h"
#include "dbmi.h"
#include "gis.h"

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
      fprintf (stderr, "Error trying to read dbmscap file\n");
      exit(ERROR);
    }

    for (p = list; p; p = p->next) {
      fprintf (stdout,"%s", p->driverName);
      if (parms.f) fprintf (stdout,":%s", p->comment);
      fprintf (stdout,"\n");
    }
    exit(OK);
}

void
parse_command_line (int argc, char *argv[])
{
    struct Flag *full;

    full = G_define_flag();
    full->key = 'f';
    full->description = "Full output";

    G_disable_interactive();

    if (argc > 1) {
      if(G_parser(argc, argv)) exit(ERROR);
    }

    parms.f = full->answer;
}
