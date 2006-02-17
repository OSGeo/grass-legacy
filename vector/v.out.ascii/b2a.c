#include <grass/Vect.h>
#include <grass/gis.h>
#include "local_proto.h"

int bin_to_asc(FILE * ascii,
	       FILE * att, struct Map_info *Map, int ver, int format, int dp)
{
    int type, ctype, i, cat, proj;
    double *xptr, *yptr, *zptr, x, y;
    static struct line_pnts *Points;
    struct line_cats *Cats;
    char *xstring = NULL, *ystring = NULL, *zstring = NULL;

    Points = Vect_new_line_struct();	/* init line_pnts struct */
    Cats = Vect_new_cats_struct();

    proj = Vect_get_proj(Map);

    /* by default, read_next_line will NOT read Dead lines */
    /* but we can override that (in Level I only) by specifying */
    /* the type  -1, which means match all line types */

    Vect_rewind(Map);

    while (1) {
	if (-1 == (type = Vect_read_next_line(Map, Points, Cats)))
	    return (-1);

	if (type == -2)		/* EOF */
	    return (0);

	if (format == FORMAT_POINT && !(type & GV_POINTS))
	    continue;

	if (ver < 5) {
	    Vect_cat_get(Cats, 1, &cat);
	}

	switch (type) {
	case GV_BOUNDARY:
	    if (ver == 5)
		ctype = 'B';
	    else
		ctype = 'A';
	    break;
	case GV_CENTROID:
	    if (ver < 5) {
		if (att != NULL) {
		    if (cat > 0) {
			G_asprintf(&xstring, "%.*f", dp, Points->x[0]);
			G_trim_decimal(xstring);
			G_asprintf(&ystring, "%.*f", dp, Points->y[0]);
			G_trim_decimal(ystring);
			fprintf(att, "A %s %s %d\n", xstring, ystring, cat);
		    }
		}
		continue;
	    }
	    ctype = 'C';
	    break;
	case GV_LINE:
	    ctype = 'L';
	    break;
	case GV_POINT:
	    ctype = 'P';
	    break;
	case GV_FACE:
	    ctype = 'F';
	    break;
	case GV_KERNEL:
	    ctype = 'K';
	    break;
	default:
	    ctype = 'X';
	    fprintf(stderr, "got type %d\n", (int)type);
	    break;
	}

	if (format == FORMAT_POINT) {
	    /*fprintf(ascii, "%c", ctype); */

	    G_asprintf(&xstring, "%.*f", dp, Points->x[0]);
	    G_trim_decimal(xstring);
	    G_asprintf(&ystring, "%.*f", dp, Points->y[0]);
	    G_trim_decimal(ystring);

	    if (Map->head.with_z && ver == 5) {
		G_asprintf(&zstring, "%.*f", dp, Points->z[0]);
		G_trim_decimal(zstring);
		fprintf(ascii, "%s|%s|%s", xstring, ystring, zstring);
	    }
	    else {
		fprintf(ascii, "%s|%s", xstring, ystring);
	    }
	    if (Cats->n_cats > 0)
		fprintf(ascii, "|%d", Cats->cat[0]);

	    fprintf(ascii, "\n");
	}
	else {
	    /* FORMAT_STANDARD */
	    if (ver == 5 && Cats->n_cats > 0)
		fprintf(ascii, "%c  %d %d\n", ctype, Points->n_points,
			Cats->n_cats);
	    else
		fprintf(ascii, "%c  %d\n", ctype, Points->n_points);

	    xptr = Points->x;
	    yptr = Points->y;
	    zptr = Points->z;

	    while (Points->n_points--) {

		G_asprintf(&xstring, "%.*f", dp, *xptr++);
		G_trim_decimal(xstring);
		G_asprintf(&ystring, "%.*f", dp, *yptr++);
		G_trim_decimal(ystring);

		if (ver == 5) {
		    if (Map->head.with_z) {
			G_asprintf(&zstring, "%.*f", dp, *zptr++);
			G_trim_decimal(zstring);
			fprintf(ascii, " %-12s %-12s %-12s\n", xstring, ystring,
				zstring);
		    }
		    else {
			fprintf(ascii, " %-12s %-12s\n", xstring, ystring);
		    }
		}		/*Version 4 */
		else {
		    fprintf(ascii, " %-12s %-12s\n", ystring, xstring);
		}
	    }

	    if (ver == 5) {
		for (i = 0; i < Cats->n_cats; i++) {
		    fprintf(ascii, " %-5d %-10d\n", Cats->field[i],
			    Cats->cat[i]);
		}
	    }
	    else {
		if (cat > 0) {
		    if (type == GV_POINT) {
			G_asprintf(&xstring, "%.*f", dp, Points->x[0]);
			G_trim_decimal(xstring);
			G_asprintf(&ystring, "%.*f", dp, Points->y[0]);
			G_trim_decimal(ystring);
			fprintf(att, "P %s %s %d\n", xstring, ystring, cat);
		    }
		    else {
			x = (Points->x[1] + Points->x[0]) / 2;
			y = (Points->y[1] + Points->y[0]) / 2;

			G_asprintf(&xstring, "%.*f", dp, x);
			G_trim_decimal(xstring);
			G_asprintf(&ystring, "%.*f", dp, y);
			G_trim_decimal(ystring);
			fprintf(att, "L %s %s %d\n", xstring, ystring, cat);
		    }
		}
	    }
	}
    }

    /* not reached */
}
