#include <string.h>
#include <stdlib.h>
#include "gis.h"
#include "report.h"
#include "local_proto.h"

/* mode.h */
long mode(REPORT *);

FILE *out;

int main (int argc, char *argv[])
{
    REPORT *rpt;
    int layer;
    int site;
    int ncats;
    char sfile[1024];
    char buff[50];

    if (argc < 2)
	    exit(0);

    G_gisinit (argv[0]);
    ask_prefix();

    rpt = report_open (argv[1]);
    open_outfile(sfile);
    announce (sfile);
    announce (" ... ");

    message("","SITES to S");

/* header info */
    assign ("location");
    begin_char_vector();
    list(rpt->location);
    list(rpt->fullname);
    end_vector();

    assign ("mapset");
    begin_char_vector();
    list(rpt->mapset);
    end_vector();

    assign ("sitelist");
    begin_char_vector();
    list(rpt->site_list_name);
    list(rpt->site_list_desc);
    end_vector();

    assign("wind.n");
    fprintf(out,"%s\n", format_north(rpt->north, buff, -1));

    assign("wind.s");
    fprintf(out,"%s\n", format_north(rpt->south, buff, -1));

    assign("wind.w");
    fprintf(out,"%s\n", format_east(rpt->west, buff, -1));

    assign("wind.e");
    fprintf(out,"%s\n", format_east(rpt->east, buff, -1));

    assign("wind.ns.res");
    fprintf(out,"%s\n", format_res(rpt->ns_res, buff, -1));

    assign("wind.ew.res");
    fprintf(out,"%s\n", format_res(rpt->ew_res, buff, -1));


/* find the maximum number of categories */

    ncats = -1;
    for (layer = 1; layer <= rpt->nlayers; layer++)
    {
	int cat;
	for (cat = ncats+1; report_find_cat(rpt, layer, cat); cat++)
	    ncats = cat;
    }
    ncats++;

/* category stats - in one matrix */

    assign("cat.histo");
    begin_numeric_matrix();

    for (layer = 1; layer <= rpt->nlayers; layer++)
    {
	int cat;
	for (cat = 0; report_find_cat(rpt, layer, cat); cat++)
	    list(rpt->field[3]);

	while (cat++ < ncats)
	    list("NA");
    }
    end_matrix(ncats,rpt->nlayers,1);

/* category stats - in separate vectors */

    for (layer = 1; layer <= rpt->nlayers; layer++)
    {
	int cat;
	char name[20];

	sprintf(name, "cat.%d.histo", layer);
	assign (name);
	begin_numeric_vector();
	for (cat = 0; report_find_cat(rpt, layer, cat); cat++)
		list(rpt->field[3]);
	end_vector();
    }

/* category labels in one matrix */

    assign("cat.name");
    begin_char_matrix();

    for (layer = 1; layer <= rpt->nlayers; layer++)
    {
	int cat;

	for (cat = 0; report_find_cat(rpt, layer, cat); cat++)
	    list(rpt->field[4]);

	while (cat++ < ncats)
	    list("");
    }
    end_matrix(ncats,rpt->nlayers,1);

/* category labels - in separate vectors */

    for (layer = 1; layer <= rpt->nlayers; layer++)
    {
	int cat;
	char name[20];

	sprintf(name, "cat.%d.name", layer);
	assign(name);
	begin_char_vector();
	for (cat = 0; report_find_cat(rpt, layer, cat); cat++)
	    list(rpt->field[4]);
	end_vector();
    }

/* layer names */

    assign("nlayers");
    fprintf(out,"%d\n", rpt->nlayers);

    assign("layer.name");
    begin_char_matrix();

    for (layer = 1; layer <= rpt->nlayers; layer++)
    {
	if(report_find_layer(rpt, layer))
	{
	    list(rpt->field[2]);
	    list(rpt->field[3]);
	}
	else
	{
	    list("");
	    list("");
	}
    }
    end_matrix (2,0,1);

/* sites */

    assign("nsites");
    fprintf(out,"%d\n", rpt->npoints);

    assign ("site.e");
    begin_numeric_vector();
    for (site = 1; site <= rpt->npoints; site++)
    {
	if (report_find_point (rpt, site))
	    list (rpt->field[2]);
	else
	    list ("NA");
    }
    end_vector();

    assign ("site.n");
    begin_numeric_vector();
    for (site = 1; site <= rpt->npoints; site++)
    {
	if (report_find_point (rpt, site))
	    list (rpt->field[3]);
	else
	    list ("NA");
    }
    end_vector();

    assign ("site.name");
    begin_char_vector();
    for (site = 1; site <= rpt->npoints; site++)
    {
	if (report_find_point (rpt, site))
	    list (rpt->field[4]);
	else
	    list ("NA");
    }
    end_vector();

/* data! */
    assign("site.data");
    begin_numeric_array();

    for (layer = 1; layer <= rpt->nlayers; layer++)
	for (site = 1; site <= rpt->npoints; site++)
	{
	    int nfields, i;
	    int cat;
	    char temp[20];

	    if (report_find_data (rpt, layer, site))
		nfields = rpt->nfields - 3;
	    else
		nfields = 0;

	    if (nfields > rpt->matrix.size)
		nfields = rpt->matrix.size;

	    for (i = 0; i < nfields; i++)
	    {
		cat = atoi(rpt->field[i+3]) + 1;
		sprintf(temp, "%d", cat);
		list (temp);
	    }
	    while (i++ < rpt->matrix.size)
		list("NA");
	}

    end_array (rpt->matrix.size, rpt->npoints, rpt->nlayers);

/* data modes */
    assign("site.mode");
    begin_numeric_matrix();

    for (site = 1; site <= rpt->npoints; site++)
	for (layer = 1; layer <= rpt->nlayers; layer++)
	{
	    char temp[20];

	    if (report_find_data (rpt, layer, site))
		sprintf(temp,"%ld", mode (rpt) + 1);
	    else
		strcpy(temp,"NA");

	    list (temp);
	}

    end_matrix (rpt->nlayers, rpt->npoints, 1);

/* done */
    message("","COMPLETE");
    fclose (out);
    fprintf (stdout,"complete\n\n");
    fprintf (stdout,"To install into an S database, you will need to issue\n");
    fprintf (stdout,"the following command from within S:\n\n");
    fprintf (stdout,"      source(\"%s\")\n\n", sfile);
    exit(1);	/* force hit RETURN */
}
