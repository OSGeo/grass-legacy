#include "glob.h"

void
parse_command_line(argc, argv, parms)
    char *argv[];
    struct parms *parms;
{
    struct Option *input, *output, *barriers, *fs, *attributes;
    struct Flag *quiet, *region;
    struct GModule *module;

    module = G_define_module();
    module->description = 
      "Aggregate point data into clusters of "
      "like data using a voronoi tesselation. ";

    input              = G_define_option();
    input->key         = "input";
    input->type        = TYPE_STRING;
    input->required    = YES;
    input->description = "input point file";

    region             = G_define_flag();
    region->key        = 'r';
    region->description = "only process points in the current region";

    output              = G_define_option();
    output->key         = "output";
    output->type        = TYPE_STRING;
    output->required    = YES;
    output->description = "output point file";

    fs                = G_define_option();
    fs->key           = "fs";
    fs->key_desc      = "char";
    fs->type          = TYPE_STRING;
    fs->required      = NO;
    fs->description   = "input field separator";

    attributes                = G_define_option();
    attributes->key           = "attributes";
    attributes->key_desc      = "field#";
    attributes->type          = TYPE_INTEGER;
    attributes->multiple      = YES;
    attributes->required      = NO;
    attributes->description   = "attributes to compare";

    barriers                = G_define_option();
    barriers->key           = "barriers";
    barriers->key_desc      = "vectorfile";
    barriers->type          = TYPE_STRING;
    barriers->required      = NO;
    barriers->multiple      = YES;
    barriers->description   = "vector 'barrier' file";
    barriers->gisprompt     = "old,dig,vector";

    quiet             = G_define_flag();
    quiet->key        = 'q';
    quiet->description = "run quietly";


    G_disable_interactive();
    if(G_parser(argc, argv))
        exit(1);

    parms->input   = input->answer;
    parms->output  = output->answer;

    if(barriers->answer)
	parms->barriers = barriers->answers;
    else
	parms->barriers = NULL;

    if(attributes->answer)
	parms->fields = attributes->answers;
    else
	parms->fields = NULL;

    parms->fs = fs->answer;
    if (parms->fs)
    {
	if (*parms->fs == 0 || *parms->fs == ' ')
	    parms->fs = NULL;
    }

    parms->quiet   = quiet->answer;
    parms->region  = region->answer;
}
