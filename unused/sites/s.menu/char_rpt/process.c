#include <stdlib.h>
#include "gis.h"
#include "report.h"
#include "local_proto.h"

#define POINT_NUMBER points->field[1]
#define POINT_EAST   points->field[2]
#define POINT_NORTH  points->field[3]
#define POINT_DESC   points->field[4]

int process (
    FILE *out,
    REPORT *points,REPORT *ref,
    int *cats_list)		/* work array for sorting cats */
{
    int point_num;
    int layer_num;
    int i;
    char *N, *E;
    double atof();

    point_num = atoi (POINT_NUMBER);

    fprintf (out, ".blocktitle\n");
    fprintf (out, "Site: ");
    fprintf (out, "%-44.44s ", points->nfields > 4 ? POINT_DESC : " ");
    N = "(N)";  E = "(E)";
    if(G_projection()==PROJECTION_LL)
      {
         N = "";  E = "";
       }
    fprintf (out, "  %s%s %s%s\n", POINT_EAST, E, POINT_NORTH, N);
    fprintf (out, "Size: %d cell%s\n", points->matrix.size,
	    points->matrix.size == 1 ? "" : "s");
    fprintf (out,".end\n");

    fprintf (out,".block\n");

    for (layer_num = 1; layer_num <= ref->nlayers; layer_num++)
    {
	fprintf (out, "\n  layer: ");
	if (report_find_layer (ref, layer_num))
	    fprintf (out, "%-20s %s\n", ref->field[2], ref->field[3]);
	else
	    fprintf (out, " ** unknown **\n");
	fprintf (out, "\n");

	if (report_find_data (ref, layer_num, point_num))
	{
	    for (i = 0; i < ref->matrix.size; i++)
		cats_list[i] = atoi(ref->field[i+3]);
	    sort_int (cats_list, ref->matrix.size, 1);
	    do_layer (out, ref, layer_num, cats_list);
	}
	else
	    fprintf (out, "    ** no data **\n");
    }
    fprintf (out, ".end\n");

    return 0;
}
