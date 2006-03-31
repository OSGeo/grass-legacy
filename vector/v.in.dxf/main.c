
/*******************************************************************************
 *
 * MODULE:       v.in.dxf
 *
 * AUTHOR(S):    Original written by Chuck Ehlschlaeger, 6/89
 * 		 Revised by Dave Gerdes, 12/89
 * 		 US Army Construction Engineering Research Lab
 *
 * 		 Contribution:
 * 		 Benjamin Horner-Johnson <ben@earth.nwu.edu>
 * 		 Michel Wurtz <michel.wurtz@eledetection.fr>
 * 		 Jacques Bouchard <bouchard@onera.fr>
 * 		 J Moorman
 *
 * 		 Rewrite for GRASS 6.0:
 * 		 Huidae Cho <grass4u@gmail.com>
 *
 * PURPOSE:      Import DXF file
 *
 * COPYRIGHT:    CORRECT ME!
 *
 ******************************************************************************/

#define _MAIN_C_
#include <stdlib.h>
#include <string.h>
#include "global.h"

int main(int argc, char *argv[])
{
    struct dxf_file *dxf;
    struct Map_info *Map;
    char *output_name;

    struct GModule *module;
    struct
    {
	struct Flag *extent;
	struct Flag *table;
    } flag;
    struct
    {
	struct Option *input;
	struct Option *output;
    } opt;

    G_gisinit(argv[0]);

    module = G_define_module();
    module->description =
	_("Converts files in DXF format to GRASS vector file format.");

    flag.extent = G_define_flag();
    flag.extent->key = 'e';
    flag.extent->description = _("Ignore the map extent of DXF file");

    flag.table = G_define_flag();
    flag.table->key = 't';
    flag.table->description = _("Do not create attribute tables");

    opt.input = G_define_option();
    opt.input->key = "input";
    opt.input->type = TYPE_STRING;
    opt.input->required = YES;
    opt.input->multiple = NO;
    opt.input->gisprompt = "file,file,file";
    opt.input->description = _("DXF input file");

    opt.output = G_define_option();
    opt.output->key = "output";
    opt.output->type = TYPE_STRING;
    opt.output->required = NO;
    opt.output->multiple = NO;
    opt.output->description = _("Name of output vector map");

    if (G_parser(argc, argv))
	exit(-1);

    debug_init();

    flag_extent = flag.extent->answer;
    flag_table = flag.table->answer;

    fprintf(stderr, _("\nCONVERSION OF %s TO VECTOR FILE:  "),
	    opt.input->answer);

    /* open DXF file */
    if (!(dxf = dxf_open(opt.input->answer)))
	G_fatal_error(_("%s: Cannot open dxf file"), opt.input->answer);

    /* make vector file name SQL compliant */
    if (opt.output->answer)
	output_name = G_store(opt.output->answer);
    else {
	char *p, *p2;

	if ((p = G_rindex(dxf->name, '/')))
	    p++;
	else
	    p = dxf->name;
	output_name = G_store(p);
	if ((p2 = G_rindex(p, '.')))
	    output_name[p2 - p] = 0;
    }
    {
	char *p;

	for (p = output_name; *p; p++)
	    if (*p == '.')
		*p = '_';
    }

    if (Vect_legal_filename(output_name) < 0)
	G_fatal_error(_("Use output= option to change vector map name"));

    /* create vector file */
    Map = (struct Map_info *)G_malloc(sizeof(struct Map_info));
    if (Vect_open_new(Map, output_name, 1) < 0)
	G_fatal_error(_("%s: Cannot open new vector file"), output_name);

    Vect_set_map_name(Map, output_name);
    G_free(output_name);

    Vect_hist_command(Map);

    /* import */
    dxf_to_vect(dxf, Map);

    dxf_close(dxf);
    Vect_close(Map);

    exit(0);
}
