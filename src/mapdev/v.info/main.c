#include "gis.h"
#include "Vect.h"
#include <string.h>
#include <stdlib.h>

#define printline(x) fprintf (stdout, " | %-74.74s |\n", x)
#define divider(x) \
	fprintf (stdout, " %c", x); \
	for (i = 0; i < 76; i++ ) \
		fprintf ( stdout, "-" ); \
	fprintf (stdout, "%c\n", x)


int main (
int argc,
char *argv[])
{
  struct GModule *module;
  struct Option *input;
  struct Map_info map;
  struct Categories cats;
  struct dig_head v_head;
  char *mapset, *name, line[200], temp[50], err_msg[200];
  int cats_ok, i;

  G_gisinit (argv[0]);

  module = G_define_module();
  module->description =
	"Outputs basic information about a user-specified vector map layer.";

  input = G_define_option();
  input->key = "input";
  input->type = TYPE_STRING;
  input->required = YES;
  input->description = "Name of existing vector file";
  input->gisprompt = "old,dig,vector";

  if (G_parser(argc,argv))
    exit(1);

  name = input->answer;

  mapset = G_find_file ("dig", name, "");
  if (mapset == NULL) {
    sprintf (err_msg, "Could not find vector file [%s]", name);
    G_fatal_error (err_msg);
    }
    
  if (0 > Vect_open_old (&map, name, mapset)) {
    sprintf (err_msg, "Could not open vector file [%s]\n", name);
    G_fatal_error (err_msg);
    }

  cats_ok = G_read_vector_cats (name, mapset, &cats) >= 0;
  v_head = map.head;

  divider ('+');

  sprintf (line, "Layer:    %-29.29s  Date: %s", name, v_head.date );
  printline (line);
  sprintf (line, "Mapset:   %-29.29s  Organization: %s", mapset, v_head.organization );
  printline (line);
  sprintf (line, "Location: %-29.29s  Name of creator: %s", G_location (), v_head.your_name);
  printline (line);
  sprintf (line, "DataBase: %s", G_gisdbase ());
  printline (line);
  if (cats_ok)
    sprintf (line, "Title:    %s ( %s) ", cats.title, name);
  else
    sprintf (line, "Title:    %s ", name);

  printline (line);

  divider ('|');
  printline ("");

  sprintf (line, "  Type of Map:  %s                   ", "Vector");

  strcat (line, "Number of Categories: ");

  if (cats_ok)
  {
    sprintf (temp, "%-9ld", (long)cats.num);
    strcat (line, temp);
  }
  else
    strcat (line, "??");

  printline (line);
  printline ("");
  sprintf (line, "  Projection: %s (zone %d)", G_database_projection_name(), G_zone());
  printline (line);

  sprintf (line, "           N: %10.0f    S: %10.0f", v_head.N, v_head.S); 
  printline (line);
  sprintf (line, "           E: %10.0f    W: %10.0f", v_head.E, v_head.W);
  printline (line);

  printline ("");

  sprintf (line, "  Source date:     %s", v_head.source_date);
  printline (line);
  sprintf (line, "  Original scale 1:%ld", v_head.orig_scale);
  printline (line);
  printline ("");
  sprintf (line, "  Comments:");
  printline (line);
  sprintf (line, "    %s", v_head.line_3);
  printline (line);
  printline ("");
  divider ('+');
  fprintf (stdout, "\n");
 
  Vect_close (&map);

  return (0);
}
