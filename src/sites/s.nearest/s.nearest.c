#include <math.h>
#include "gis.h"

main (argc,argv) 
int argc;
char *argv[];
{
	char
		 *Mapset1,
		 *Mapset2,
		 *name1,
		 *name2,
		 *name3,
		 *desc1,
		 *desc2,
		 text[80],
		 nearest[80],
		 err_msg[200];
	FILE
		 *Sites1,
		 *Sites2,
		 *Sites3;
	double
		 east1,
		 north1,
		 east2,
		 north2,
		 xdist,
		 ydist,
		 dist,
		 min;
	struct Option 
 			 *input1,
			 *input2,
			 *output;

	G_gisinit (argv[0]);

	input1 = G_define_option();
	input1 -> key = "s1";
	input1 -> type = TYPE_STRING;
	input1 -> required = YES;
	input1 -> description = "Name of first existing sites file";
	input1 -> gisprompt = "old,site_lists,sites";

	input2 = G_define_option();
	input2 -> key = "s2";
	input2 -> type = TYPE_STRING;
	input2 -> required = YES;
	input2 -> description = "Name of second existing sites file";
	input2 -> gisprompt = "old,site_lists,sites";

	output = G_define_option();
	output -> key = "s3";
	output -> type = TYPE_INTEGER;
	output -> required = YES;
	output -> description = "Output sites file";

	output -> gisprompt = "new,site_lists,sites";

	if (G_parser(argc,argv))
		exit (1);

	name1 = input1 -> answer;
	Mapset1 = G_find_file ("site_lists", name1, "");
	if (Mapset1 == NULL) {
		sprintf (err_msg, "Sites file [%s] not found\n", name1);
		G_fatal_error (err_msg);
		}

	name2 = input2 -> answer;
	Mapset2 = G_find_file ("site_lists", name2, "");
	if (Mapset2 == NULL) {
		sprintf (err_msg, "Sites file [%s] not found\n", name2);
		G_fatal_error (err_msg);
		}

	Sites1 = G_fopen_sites_old (name1, Mapset1);
	if (Sites1 == NULL) {
		sprintf (err_msg, "Could not open sites file [%s]\n", name1);
		G_fatal_error (err_msg);
		}

	Sites2 = G_fopen_sites_old (name2, Mapset2);
	if (Sites2 == NULL) {
		sprintf (err_msg, "Could not open sites file [%s]\n", name2);
		G_fatal_error (err_msg);
		}

	name3 = output -> answer;
	Sites3 = G_fopen_sites_new (name3, Mapset1);
	if (Sites3 == NULL) {
		sprintf (err_msg, "Could not open sites file [%s]\n", name3);
		G_fatal_error (err_msg);
		}

	fprintf (Sites3, "name| %s\n", name3);
	fprintf (Sites3, "desc| minimum distance from a site in %s to a site in %s\n", name1, name2);

	while (G_get_site (Sites1, &east1, &north1, &desc1) > 0 )
	{

		sprintf (text, "%s", desc1);
		sprintf (nearest, "-");
		min = 1.0e+20;
		while (G_get_site (Sites2, &east2, &north2, &desc2) > 0 )
		{

			xdist = east1 - east2;
			ydist = north1 - north2;

			dist = sqrt ((xdist * xdist) + (ydist * ydist));
			if ( dist < min && dist != 0 ) {
				min = dist;
				sprintf (nearest, "%s", desc2);
			}
		}
	fprintf (Sites3, " %f | %f | %s | %10.2f | %s\n", east1, north1, text, min, nearest);
	rewind (Sites2);

	}
	return (0);

}
