#include <time.h>
#define GLOBAL
#include "global.h"
#include "local_proto.h"

static int interrupted = 0;

int 
main (int argc, char *argv[])
{
    int count;
    int n;
    int row,nrows;
    int col,ncols;
    CELL *x;
    struct Cell_head window;
    FILE *fd;

	struct GModule *module;
    struct {
	struct Option *group_name, *subgroup_name, *out_sig, *seed_sig,
		      *class, *sample_interval, *iterations, *separation, 
                      *convergence, *min_size, *report_file;
    } parm;

    struct {
	struct Flag *q;
    } flag;

    G_gisinit(argv[0]);

	module = G_define_module();
	module->description =
		"An imagery function that generates spectral signatures for "
		"land cover types in an image using a clustering algorithm. "
		"The resulting signature file is used as input for i.maxlik, "
		"to generate an unsupervised image classification.";

    G_get_window (&window);
    nrows = G_window_rows();
    ncols = G_window_cols();


    I_cluster_clear (&C);

    parm.group_name = G_define_option();
    parm.group_name->key = "group";
    parm.group_name->type = TYPE_STRING;
    parm.group_name->required = YES;
    parm.group_name->description = "Group of imagery files to be clustered";

    parm.subgroup_name = G_define_option();
    parm.subgroup_name->key = "subgroup";
    parm.subgroup_name->type = TYPE_STRING;
    parm.subgroup_name->required = YES;
    parm.subgroup_name->description = "Subgroup name in the above group";

    parm.out_sig = G_define_option();
    parm.out_sig->key = "sigfile";
    parm.out_sig->type = TYPE_STRING;
    parm.out_sig->required = YES;
    parm.out_sig->description = "File contains result signatures";

    parm.class = G_define_option();
    parm.class->key = "classes";
    parm.class->type = TYPE_INTEGER;
    parm.class->options = "1-255";
    parm.class->required = YES;
    parm.class->description = "Initial number of classes";

    parm.seed_sig = G_define_option();
    parm.seed_sig->key = "seed";
    parm.seed_sig->type = TYPE_STRING;
    parm.seed_sig->required = NO;
    parm.seed_sig->description = "File contains initial signatures";

    parm.sample_interval = G_define_option();
    parm.sample_interval->key = "sample";
    parm.sample_interval->key_desc = "row_interval,col_interval";
    parm.sample_interval->type = TYPE_INTEGER;
    parm.sample_interval->required = NO;
    parm.sample_interval->description = 
	"Sampling intervals (by row and col); default: ~10,000 pixels";

    parm.iterations = G_define_option();
    parm.iterations->key = "iterations";
    parm.iterations->type = TYPE_INTEGER;
    parm.iterations->required = NO;
    parm.iterations->description = "Maximum number of iterations";
    parm.iterations->answer = "30";

    parm.convergence = G_define_option();
    parm.convergence->key = "convergence";
    parm.convergence->type = TYPE_DOUBLE;
    parm.convergence->required = NO;
    parm.convergence->options = "0-100";
    parm.convergence->description = "Percent convergence";
    parm.convergence->answer = "98.0";

    parm.separation = G_define_option();
    parm.separation->key = "separation";
    parm.separation->type = TYPE_DOUBLE;
    parm.separation->required = NO;
    parm.separation->description = "Cluster separation";
    parm.separation->answer = "0.0";

    parm.min_size = G_define_option();
    parm.min_size->key = "min_size";
    parm.min_size->type = TYPE_INTEGER;
    parm.min_size->required = NO;
    parm.min_size->description = "Minimum number of pixels in a class";
    parm.min_size->answer = "17";

    parm.report_file = G_define_option();
    parm.report_file->key = "reportfile";
    parm.report_file->type = TYPE_STRING;
    parm.report_file->required = NO;
    parm.report_file->description = "Name of an output file to contain final report";

    flag.q = G_define_flag();
    flag.q->key = 'q';
    flag.q->description = "quiet";

    if (G_parser(argc,argv))
      exit(-1);

    group = parm.group_name->answer; /* a required parameter */

    subgroup = parm.subgroup_name->answer; /* required */

    outsigfile = parm.out_sig->answer;
    if (G_legal_filename(outsigfile) < 0) {
      fprintf (stderr, "\nERROR: <%s> -- illegal result signature file name\n", outsigfile);
      G_usage();
      exit(1);
    }

    if (sscanf(parm.class->answer, "%d", &maxclass) != 1 || maxclass < 1 
	|| maxclass>255) {
      fprintf(stderr,"\nERROR: <%s> -- illegal number of initial classes\n", parm.class->answer);
      G_usage();
      exit(1);
    }      

    insigfile = parm.seed_sig->answer;

    if (parm.sample_interval->answer) {
      if(sscanf(parm.sample_interval->answer,"%d,%d",&sample_rows,&sample_cols)!=2
	|| sample_rows<1 || sample_cols<1 
        || sample_rows>nrows || sample_cols>ncols)
      {
        fprintf(stderr,"\nERROR: <%s> -- illegal value(s) of sample intervals\n",
		parm.sample_interval->answer);
        G_usage();
        exit(1);
      }
    }
    else {
      sample_rows = nrows/100;
      if (sample_rows < 1)
          sample_rows = 1;
      sample_cols = ncols/100;
      if (sample_cols < 1)
          sample_cols = 1;
    }

    if(sscanf(parm.iterations->answer,"%d", &iters)!=1 || iters<1 )
    {
      fprintf(stderr,"\nERROR: <%s> -- illegal value of iterations\n", parm.iterations->answer);
      G_usage();
      exit(1);
    }

    if(sscanf(parm.convergence->answer,"%lf", &conv)!=1 || conv<0.0 || conv>100.0)
    {
      fprintf(stderr,"\nERROR: <%s> -- illegal value of convergence\n", 
	parm.convergence->answer);
      G_usage();
      exit(1);
    }

    if(sscanf(parm.separation->answer,"%lf", &sep)!=1 || sep < 0.0 )
    {
      fprintf(stderr,"\nERROR: <%s> -- illegal value of separation\n",
	parm.separation->answer);
      G_usage();
      exit(1);
    }

    if(sscanf(parm.min_size->answer,"%d", &mcs)!=1 || mcs < 2 )
    {
      fprintf(stderr,"\nERROR: <%s> -- illegal value of min_size\n",
	parm.min_size->answer);
      G_usage();
      exit(1);
    }

    verbose = !flag.q->answer;

    if ((reportfile=parm.report_file->answer) == NULL)
      report = fopen("/dev/null", "w");
    else 
      report = fopen (reportfile, "w");
    if (report == NULL)
    {
        fprintf (stderr, "can't creat reportfile: ");
	perror (reportfile);
	exit(1);
    }

    open_files();


    fprintf (report, "#################### CLUSTER (%s) ####################\n\n", G_date());
    fprintf (report, "Location: %s\n", G_location());
    fprintf (report, "Mapset:   %s\n", G_mapset());
    fprintf (report, "Group:    %s\n", group);
    fprintf (report, "Subgroup: %s\n", subgroup);
    for (n=0; n < ref.nfiles; n++)
    {
	fprintf (report, " %s\n", G_fully_qualified_name(ref.file[n].name, ref.file[n].mapset));
    }
    fprintf (report,"Result signature file: %s\n", outsigfile);
    fprintf (report, "\n");
    fprintf (report, "Region\n");
    fprintf (report, "  North: %12.2f  East: %12.2f\n",
	    window.north, window.east);
    fprintf (report, "  South: %12.2f  West: %12.2f\n",
	    window.south, window.west);
    fprintf (report, "  Res:   %12.2f  Res:  %12.2f\n",
	    window.ns_res, window.ew_res);
    fprintf (report, "  Rows:  %12d  Cols: %12d  Cells: %d\n",
	    nrows, ncols, nrows*ncols);
    fprintf (report, "Mask: %s\n", G_mask_info());
    fprintf (report, "\n");
    fprintf(report, "Cluster parameters\n");
    fprintf(report, " Number of initial classes:    %d",maxclass);
    if (insigfile)
	fprintf (report, " [from signature file %s]",insigfile);
    fprintf (report, "\n");
    fprintf(report, " Minimum class size:           %d\n",mcs);
    fprintf(report, " Minimum class separation:     %f\n",sep);
    fprintf(report, " Percent convergence:          %f\n",conv);
    fprintf(report, " Maximum number of iterations: %d\n",iters);
    fprintf(report, "\n");
    fprintf(report, " Row sampling interval:        %d\n", sample_rows);
    fprintf(report, " Col sampling interval:        %d\n", sample_cols);
    fprintf(report, "\n");
    fflush (report);

    x = (CELL *) G_malloc (ref.nfiles * sizeof(CELL));

    I_cluster_begin (&C,ref.nfiles);

    count = 0;
    if (verbose) fprintf (stderr, "Reading image ... ");
    for (row = sample_rows-1; row < nrows; row += sample_rows)
    {
	if (verbose)
	    G_percent (row, nrows, 2);
	for (n=0; n < ref.nfiles; n++)
	    if (G_get_c_raster_row (cellfd[n], cell[n], row) < 0)
		exit(1);
	for (col = sample_cols-1; col < ncols; col += sample_cols)
	{
	    count++;
	    for (n=0; n < ref.nfiles; n++)
		x[n] = cell[n][col];
	    if(I_cluster_point (&C, x) < 0)
		G_fatal_error ("Out of Memory. Please run again and choose a smaller sample size");
	}
    }
    if (verbose)
	G_percent (nrows, nrows, 2);
    fprintf (report, "Sample size: %d points\n", C.npoints);
    fprintf (report, "\n");
    if (count < 2)
	G_fatal_error ("Not enough sample points. Please run again and choose a larger sample size");

    if (C.npoints < 2)
	G_fatal_error ("Not enough non-zero sample data points. Check your current region (and mask)");

    for (n=0; n < ref.nfiles; n++)
    {
	G_free (cell[n]);
	G_close_cell (cellfd[n]);
    }
    G_free(x);

    start_time = time(NULL);
    I_cluster_exec(&C,maxclass,iters,conv,sep,mcs,checkpoint,&interrupted);

    fprintf (report,"\n########## final results #############\n");
    fprintf (report,"%d classes (convergence=%.1f%%)\n",
	I_cluster_nclasses(&C,mcs), (double)C.percent_stable);
    print_separability(report,&C);
    print_class_means(report,&C);

    if((fd = I_fopen_signature_file_new (group, subgroup, outsigfile)) != NULL)
    {
	I_write_signatures (fd, &C.S);
	fclose (fd);
    }
    else
    {
	fprintf (stderr,
	"Could not write signature file <%s> for group <%s>, subsgroup <%s>",
		outsigfile, group, subgroup);
	exit(1);
    }

    fprintf (report, "\n\n#################### CLASSES ####################\n");
    fprintf (report, "\n%d classes, %.2f%% points stable\n",
        I_cluster_nclasses(&C,1), (double) C.percent_stable);
    fprintf (report, "\n######## CLUSTER END (%s) ########\n", G_date());
    fclose (report);
    exit(0);
}
