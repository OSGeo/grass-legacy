/* Cell-file line thinning */

/* Mike Baba */
/* DBA Systems */
/* Fairfax, Va */
/* Jan 1990 */

/* Jean Ezell */
/* US Army Corps of Engineers */
/* Construction Engineering Research Lab */
/* Modelling and Simulation Team */
/* Champaign, IL  61820 */
/* January - February 1988 */

#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <grass/gis.h>
#include "local_proto.h"
#include <grass/glocale.h>

char *error_prefix;

int main(int argc, char *argv[])
{
    char *input, *output;
    struct GModule *module;
    struct Option *opt1, *opt2, *opt3;
    int iterations;

    G_gisinit(argv[0]);

    module = G_define_module();
    module->keywords = _("raster");
    module->description =
	_("Thins non-zero cells that denote linear "
	  "features in a raster map layer.");

    opt1 = G_define_standard_option(G_OPT_R_INPUT);

    opt2 = G_define_standard_option(G_OPT_R_OUTPUT);

    opt3 = G_define_option();
    opt3->key = "iterations";
    opt3->type = TYPE_INTEGER;
    opt3->required = NO;
    opt3->answer   = "200";
    opt3->description = _("Maximal number of iterations");

    if (G_parser(argc, argv))
	exit(EXIT_FAILURE);

    input = opt1->answer;
    output = opt2->answer;
    iterations = atoi(opt3->answer);

    open_file(input);
    thin_lines(iterations);
    close_file(output);

    exit(EXIT_SUCCESS);
}
