#define MAIN
#include <string.h>
#include <stdio.h>
#include "filter.h"
#include "glob.h"

int main (int argc, char *argv[])
{
    FILTER *filter;
    int nfilters;
    int repeat;
    char *in_name, *in_mapset;
    char *filt_name;
    char *out_name;
    char title[1024];
    char temp[300];
    int i;
	struct GModule *module;
    struct Flag *flag1 ;
    struct Flag *flag2 ;
    struct Option *opt1 ;
    struct Option *opt2 ;
    struct Option *opt3 ;
    struct Option *opt4 ;
    struct Option *opt5 ;

	module = G_define_module();
	module->description =
		"Raster file matrix filter.";

    /* Define the different options */

    opt1 = G_define_option() ;
    opt1->key        = "input";
    opt1->type       = TYPE_STRING;
    opt1->multiple   = NO;
    opt1->required   = YES;
    opt1->gisprompt  = "old,cell,raster" ;
    opt1->description= "Name of the input raster file" ;

    opt2 = G_define_option() ;
    opt2->key        = "output";
    opt2->type       = TYPE_STRING;
    opt2->multiple   = NO;
    opt2->required   = YES;
    opt2->gisprompt  = "new,cell,raster" ;
    opt2->description= "Name of the output raster file" ;

    opt3 = G_define_option() ;
    opt3->key        = "filter";
    opt3->type       = TYPE_STRING;
    opt3->multiple   = NO;
    opt3->required   = YES;
    opt3->description= "Name of the filter file" ;

    opt4 = G_define_option() ;
    opt4->key        = "repeat";
    opt4->type       = TYPE_INTEGER;
    opt4->multiple   = NO;
    opt4->required   = NO;
    opt4->answer     = "1";
    opt4->description= "Number of times to repeat the filter" ;

    opt5 = G_define_option() ;
    opt5->key        = "title";
    opt5->key_desc   = "\"phrase\"";
    opt5->type       = TYPE_STRING;
    opt5->required   = NO;
    opt5->description= "Output raster file title" ;

    /* Define the different flags */

    flag1 = G_define_flag() ;
    flag1->key         = 'q' ;
    flag1->description = "Quiet" ;

    /* this isn't implemented at all 
    flag3 = G_define_flag() ;
    flag3->key         = 'p' ;
    flag3->description = "Preserved edge" ;
    */

    flag2 = G_define_flag() ;
    flag2->key         = 'z' ;
    flag2->description = "Apply filter only to zero data values" ;

    G_gisinit (argv[0]);

    if (G_parser(argc, argv))
        exit(-1);

    silent = flag1->answer;
    /*
    preserve_edges = flag3->answer;
    */
    zero_only = flag2->answer;

    sscanf (opt4->answer, "%d", &repeat);
    out_name = opt2->answer;
    filt_name = opt3->answer;

    in_name = opt1->answer;

    in_mapset = G_find_cell2 (in_name,"");
    if (in_mapset == NULL)
    {
        fprintf (stderr, "%s: raster file not found", in_name);
        exit(1);
    }

    nrows = G_window_rows();
    ncols = G_window_cols();
    buflen = ncols * sizeof (CELL);

    /* get the filter */
    filter = get_filter (filt_name, &nfilters, temp);

    /* make sure filter matrix won't extend outside the cell file */
    for (i=0; i < nfilters; i++)
    {
        if (filter[i].size > ncols || filter[i].size > nrows)
        {
            fprintf (stderr,
		"%s: raster file too small for the size of the filter",
		G_program_name());
	    exit(1);
        }
    }


    /* make a title for result */
    if (opt5->answer)
	strcpy (title, opt5->answer);
    else
    {
        if (*temp == 0)
            strcpy (temp, "unknown filter");
        sprintf (title, "%s filtered using %s", in_name, temp);
    }

    perform_filter (in_name, in_mapset, out_name, filter, nfilters, repeat);

    G_put_cell_title (out_name, title);
    exit(0);
}
