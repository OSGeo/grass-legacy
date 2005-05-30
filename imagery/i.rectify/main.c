/* CMD version based on INTER version from Bob Covill 2001 */

#define GLOBAL
#include <stdlib.h>
#include <string.h>
#include "global.h"
#include "crs.h"

#define NFILES 15

void err_exit(char *, char *);

int main (int argc, char *argv[])
{
    char group[40], extension[40] ;
    char result[NFILES][15];
    int order;      /* ADDED WITH CRS MODIFICATIONS */
    int n, i, m, k;
    int got_file = 0;

    struct Option *grp, *val, *ifile, *ext;
    struct Flag *c, *a;
    struct GModule *module;

    struct Cell_head cellhd;

    setbuf (stdout, NULL);
    setbuf (stderr, NULL);

    G_gisinit (argv[0]);

/* Get Args */
  module = G_define_module();
  module->description =
	"Rectifies an image by computing a coordinate "
	"transformation for each pixel in the image based on the "
	"control points";

  grp = G_define_option();
  grp->key             = "group";
  grp->type            =  TYPE_STRING;
  grp->required        =  YES;
  grp->gisprompt        = "old,group,group";
  grp->description     = "Name of imagery group";

  ifile = G_define_option();
  ifile->key             = "input";
  ifile->type            =  TYPE_STRING;
  ifile->required        =  NO;
  ifile->multiple        = YES;
  ifile->gisprompt       = "old,cell,raster";
  ifile->description     = "Name of input raster map(s)";

  ext = G_define_option();
  ext->key             = "extension";
  ext->type            =  TYPE_STRING;
  ext->required        =  YES;
  ext->multiple         = NO;
  ext->description     = "Output file extension (inputfile(s) + extension)";

  val = G_define_option();
  val->key             = "order";
  val->type            =  TYPE_INTEGER;
  val->required        =  YES;
  val->description     = "Rectification polynom order (1-3)";

  c = G_define_flag();
  c->key              = 'c';
  c->description      = "Use curr. region settings in target location (def.=calculate smallest area)";

  a = G_define_flag();
  a->key              = 'a';
  a->description      = "Rectify all images in group";



  if (G_parser (argc, argv))
   exit (-1);

 G_strip(grp->answer);
 strcpy(group, grp->answer);
 strcpy(extension, ext->answer);
 order = atoi(val->answer);

 if(!ifile->answers)
    a->answer=1;   /* force all */
 
/* Find out how files on command line */
 if (!a->answer) {
   for (m = 0; ifile->answers[m]; m++) {
      k = m;
   }
  k++;
 }

if (order < 1 || order > MAXORDER)
  G_fatal_error("\nInvalid order (%d) please enter 1 to %d.\n",order, MAXORDER);

/* determine the number of files in this group */
    if (I_get_group_ref (group, &ref) <= 0 )
        G_fatal_error("Group %s does not exist.", grp->answer);

    if (ref.nfiles <= 0)
    {
	fprintf (stderr, "No files in this group - %s!\n", grp->answer);
	exit(0);
    }

    for (i=0; i < NFILES; i++)
	result[i][0] = 0;

    ref_list = (int *) G_malloc (ref.nfiles * sizeof(int));
    new_name = (char **) G_malloc (ref.nfiles * sizeof(char *));

    if (a->answer) {
      for (n = 0; n < ref.nfiles; n++) {
      ref_list[n] = -1;
      }
   } 
   else {
      for (m = 0; m < k; m++) {
	got_file = 0;
	for (n = 0; n < ref.nfiles; n++) {
	  if (strcmp(ifile->answers[m],ref.file[n].name) == 0) {
		got_file = 1;
		ref_list[n] = -1;
		break;
	  }
	}
	if (got_file == 0) 
	    err_exit(ifile->answers[m], group);
   }
}

/* read the control points for the group */
    get_control_points (group,order);

/* get the target */
    get_target(group);


if (c->answer) {
  /* Use current Region */
  G_get_window(&target_window);
} else {
  /* Calculate smallest region */
  if (a->answer) {
    if (G_get_cellhd (ref.file[0].name, ref.file[0].mapset, &cellhd) < 0)
        exit(-1);
    } else {
        if (G_get_cellhd (ifile->answers[0], ref.file[0].mapset, &cellhd) < 0)
            exit(-1);
    }
  georef_window (&cellhd, &target_window, order);
}

fprintf(stderr, "Using Region: N=%f S=%f, E=%f W=%f\n", target_window.north, target_window.south, target_window.east, target_window.west);

    exec_rectify (order, extension);
    exit(0);
}

void err_exit(char *file, char *grp)
{
 int n;

 fprintf(stderr, "Input file %s does not exist in group %s.\n Try:\n", file, grp);
 for (n = 0; n < ref.nfiles; n++) {
   fprintf(stderr, "%s ", ref.file[n].name);
   fprintf(stderr, "\n");
 }
 G_fatal_error ("Exit!");
}
