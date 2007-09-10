/* ****************************************************************************
 *
 * MODULE:       v.label.sa
 * AUTHOR(S):    Wolf Bergenheim
 * PURPOSE:      Create paint labels, but use a Simulated Annealing
 *               algorithm to avoid overlaping labels.
 *               This file contains the command line parsing and main function.
 *               The paint label file writing funtion (print_label()) is also
 *               part of this file.
 * COPYRIGHT:    (C) 2007 by the GRASS Development Team
 *
 *               This program is free software under the GNU General Public
 *               License (>=v2). Read the file COPYING that comes with GRASS
 *               for details.
 *
 *****************************************************************************/

#include <time.h>
#include "labels.h"
#define DEFAULT_CHARSET "UTF-8"

/**
 * This function defines the parameters and calls the command-line parser
 * @param argc Count of command line arguments
 * @param argv The command line arguments.
 * @param p The parameters structure.
 */
static int parse_args(int argc, char *argv[], struct params *p);

/**
 * The main function controls the program flow.
 */
int main(int argc, char *argv[])
{
    struct params p;
    label_t *labels;
    int n_labels, i;
    struct GModule *module;
    FILE *labelf;

    srand((unsigned int)time(NULL));

    G_gisinit(argv[0]);
    module = G_define_module();
    module->keywords = _("vector, paint labels");
    module->description = _("Create optimally placed labels for vector map(s)");

//    fprintf(stderr, "Parsing options and flags\n");
    /* parse options and flags */
    if (parse_args(argc, argv, &p))
	exit(EXIT_FAILURE);
    /* initialize labels (get text from database, and get features) */
    labels = labels_init(&p, &n_labels);
    /* start algorithm */
    /*   1. candidate position generation */
    label_candidates(labels, n_labels);
    /*   2. position evaluation */
    label_candidate_overlap(labels, n_labels);
    /*   3. position selection */
    simulate_annealing(labels, n_labels, &p);
    /* write lables to file */
    fprintf(stderr, "Writing labels to file: ...");
    labelf = G_fopen_new("paint/labels", p.labels->answer);
    for (i = 0; i < n_labels; i++) {
	if (labels[i].n_candidates > 0) {
	    print_label(labelf, &labels[i], &p);
	}
	G_percent(i, (n_labels - 1), 1);
    }
    fclose(labelf);
/*
    {
	char *f;
	f = G_tempfile();
	labelf = fopen(f, "w");
	printf("Writing all labels to file %s", f);
	for (i = 0; i < n_labels; i++) {
		if (labels[i].n_candidates > 0) {
			print_labels(labelf, &labels[i], &p);
		}
		G_percent(i, (n_labels - 1), 1);
    	}
	free(f);
    	fclose(labelf);
    }
*/
    return EXIT_SUCCESS;
}

static int parse_args(int argc, char *argv[], struct params *p)
{
    p->map = G_define_standard_option(G_OPT_V_MAP);

    p->type = G_define_standard_option(G_OPT_V_TYPE);
    p->type->options = "point,line,area";
    p->type->answer = "point,line,area";

    p->layer = G_define_standard_option(G_OPT_V_FIELD);

    p->column = G_define_option();
    p->column->key = "column";
    p->column->type = TYPE_STRING;
    p->column->required = YES;
    p->column->description =
	_("Name of attribute column to be used for labels");

    p->labels = G_define_option();
    p->labels->key = "labels";
    p->labels->description = _("Name for new paint-label file");
    p->labels->type = TYPE_STRING;
    p->labels->required = YES;
    p->labels->key_desc = "name";

    p->font = G_define_option();
    p->font->key = "font";
    p->font->type = TYPE_STRING;
    p->font->required = YES;
    p->font->description = _("Path to TrueType font (including file name)");
    p->font->guisection = _("Font");
    p->font->gisprompt = "old_file,file,font";

    p->size = G_define_option();
    p->size->key = "size";
    p->size->description = _("Label size (in map-units)");
    p->size->type = TYPE_DOUBLE;
    p->size->answer = "100";
    p->size->guisection = _("Font");

    p->isize = G_define_option();
    p->isize->key = "isize";
    p->isize->description = _("Icon size of point features (in map-units)");
    p->isize->type = TYPE_DOUBLE;
    p->isize->answer = "10";

    p->charset = G_define_option();
    p->charset->key = "charset";
    p->charset->type = TYPE_STRING;
    p->charset->required = NO;
    p->charset->answer = DEFAULT_CHARSET;
    p->charset->description =
	"Character encoding (default: " DEFAULT_CHARSET ")";

    return G_parser(argc, argv);
}

void print_label(FILE * labelf, label_t * label, struct params *p)
{
    int cc;
    cc = label->current_candidate;
    double size;
    size = atof(p->size->answer);

    fprintf(labelf, "east: %lf\n", label->candidates[cc].point.x);
    fprintf(labelf, "north: %lf\n", label->candidates[cc].point.y);
    fprintf(labelf, "xoffset: %lf\n", -0.0 * (size));
    fprintf(labelf, "yoffset: %lf\n", -0.0 * (size));
    fprintf(labelf, "ref: %s\n", "none none");

    fprintf(labelf, "font: %s\n", p->font->answer);
    fprintf(labelf, "color: %s\n", "black");

    fprintf(labelf, "size: %lf\n", size);

    fprintf(labelf, "width: %d\n", 1);
    fprintf(labelf, "hcolor: %s\n", "none");
    fprintf(labelf, "hwidth: %d\n", 0);
    fprintf(labelf, "background: %s\n", "none");
    fprintf(labelf, "border: %s\n", "none");
    fprintf(labelf, "opaque: %s\n", "yes");
    fprintf(labelf, "rotate: %f\n",
	    label->candidates[cc].rotation * 180.0 / M_PI);
    fprintf(labelf, "text:%s\n\n", label->text);

    return;
}

