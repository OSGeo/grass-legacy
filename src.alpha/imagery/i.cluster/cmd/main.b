#define GLOBAL
#include "global.h"

static int interrupted = 0;
static char *me;
static char *error = 0;

main(argc,argv) char *argv[];
{
    int count;
    int n;
    int row,nrows;
    int col,ncols;
    CELL *x;
    struct Cell_head window;
    int checkpoint();
    long time();
    FILE *fd;
    char filename[80], dirname[100]; 
    FILE *jfd;

    struct {
	struct Option *group_name, *subgroup_name, *out_sig, *seed_sig,
		      *class, *sample_interval, *iterations, *separation, 
                      *convergence, *min_size, *report_file;
    } parm;

    struct
        {
            struct Flag *q, *o;
    } flag;

    G_gisinit(me = argv[0]);
    if (G_maskfd() >= 0)
    {
	printf ("\nWARNING: you have your mask set.\n");
	if (!G_yes("Do you want to continue? ", -1)) exit(0);
    }
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
    /* parm.class->options = "1-255"; */
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
	"Sample pixels (by row and col); defaults: processing pixels < 10,000";

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
    /*parm.convergence->options = "1.0-100.0";*/
    parm.convergence->description = "Percent convergence";
    parm.convergence->answer = "98.0";

    parm.separation = G_define_option();
    parm.separation->key = "separation";
    parm.separation->type = TYPE_DOUBLE;
    parm.separation->required = NO;
    parm.separation->description = "Separation of classes";
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
    parm.report_file->description = "name of an output file to contain report";

    flag.q = G_define_flag();
    flag.q->key = 'q';
    flag.q->description = "quiet";

    flag.o = G_define_flag();
    flag.o->key = 'o';
    flag.o->description = "overwrite old signature and report file";

    if (G_parser(argc,argv))
      exit(-1);

jfd = fopen ("junk", "w");

    group = parm.group_name->answer; /* a required parameter */
fprintf (jfd, "group name: <%s>\n", group);

    subgroup = parm.subgroup_name->answer; /* required */
fprintf (jfd, "subgroup name: <%s>\n", subgroup);

    outsigfile = parm.out_sig->answer;
    if (G_legal_filename(outsigfile) < 0) {
      fprintf (stderr, "\nERROR: <%s> -- illegal result signature file name\n", outsigfile);
      G_usage();
      exit(1);
    }
fprintf (jfd, "outsigfile name: <%s>\n", outsigfile);

    if (sscanf(parm.class->answer, "%d", &maxclass) != 1 || maxclass < 1 
	|| maxclass>255) {
      fprintf(stderr,"\nERROR: <%d> -- illegal number of initial classes\n", maxclass);
      G_usage();
      exit(1);
    }      
fprintf (jfd, "initial class: <%d>\n", maxclass);

    insigfile = parm.seed_sig->answer;
fprintf (jfd, "insigfile name: <%s>\n", insigfile);

    if (parm.sample_interval->answer) {
      if(sscanf(parm.sample_interval->answer,"%d,%d",&sample_rows,&sample_cols)!=2
	|| sample_rows<1 || sample_cols<1 
        || sample_rows>nrows || sample_cols>ncols)
      {
        fprintf(stderr,"\nERROR: <%d,%d> -- illegal value(s) of sample intervals\n",sample_rows,sample_cols);
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
fprintf (jfd, "values of sample interval: row - <%d>; col - <%d>\n", sample_rows, sample_cols);

    if(sscanf(parm.iterations->answer,"%d", &iters)!=1 || iters<1 )
    {
      fprintf(stderr,"\nERROR: <%d> -- illegal value of iterations\n", iters);
      G_usage();
      exit(1);
    }
fprintf (jfd, "iteration number: <%d>\n", iters);

    if(sscanf(parm.convergence->answer,"%lf", &conv)!=1 || conv<0.0 || conv>100.0)
    {
      fprintf(stderr,"\nERROR: <%lf> -- illegal value of convergence\n", conv);
      G_usage();
      exit(1);
    }
fprintf (jfd, "convergence percent: <%lf>\n", conv);

    if(sscanf(parm.separation->answer,"%lf", &sep)!=1 || sep < 0.0 )
    {
      fprintf(stderr,"\nERROR: <%lf> -- illegal value of separation\n", sep);
      G_usage();
      exit(1);
    }
fprintf (jfd, "separation percent: <%lf>\n", sep);

    if(sscanf(parm.min_size->answer,"%d", &mcs)!=1 || mcs < 2 )
    {
      fprintf(stderr,"\nERROR: <%d> -- illegal value of min_size\n", mcs);
      G_usage();
      exit(1);
    }
fprintf (jfd, "min_size of classes: <%d>\n", mcs);

    verbose = !flag.q->answer;
fprintf (jfd, "verbose (!quiet) signature: <%d>\n", verbose);

    overwrite = flag.o->answer;
fprintf (jfd, "overwrite old sig file: <%d>\n", overwrite);

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
fprintf (jfd, "report file name: <%s>\n", reportfile);

    open_file(argv[0]);

fprintf (jfd, "initial class after checking seed file: <%d>\n", maxclass);
fclose (jfd);

    fprintf (report, "#################### CLUSTER (%s) ####################\n\n", G_date());
    fprintf (report, "Location: %s\n", G_location());
    fprintf (report, "Mapset:   %s\n", G_mapset());
    fprintf (report, "Group:    %s\n", group);
    fprintf (report, "Subgroup: %s\n", subgroup);
    for (n=0; n < ref.nfiles; n++)
    {
	fprintf (report, " %s@%s\n", ref.file[n].name, ref.file[n].mapset);
    }
    fprintf (report,"Result signature file: %s\n", outsigfile);
    fprintf (report, "\n");
    fprintf (report, "Region\n");
    fprintf (report, "  North: %12.2lf  East: %12.2lf\n",
	    window.north, window.east);
    fprintf (report, "  South: %12.2lf  West: %12.2lf\n",
	    window.south, window.west);
    fprintf (report, "  Res:   %12.2lf  Res:  %12.2lf\n",
	    window.ns_res, window.ew_res);
    fprintf (report, "  Rows:  %12d  Cols: %12d  Cells: %d\n",
	    nrows, ncols, nrows*ncols);
    fprintf (report, "Mask: %s\n", G_mask_info());
    fprintf (report, "\n");
    fprintf(report, "Cluster parameters\n");
    fprintf(report, " Number of initial classes:    %d",maxclass);
    if (*insigfile)
	fprintf (report, " [from signature file %s]",insigfile);
    fprintf (report, "\n");
    fprintf(report, " Minimum class size:           %d\n",mcs);
    fprintf(report, " Minimum class separation:     %lf\n",sep);
    fprintf(report, " Percent convergence:          %lf\n",conv);
    fprintf(report, " Maximum number of iterations: %d\n",iters);
    fprintf(report, "\n");
    fprintf(report, " Row sampling interval:        %d\n", sample_rows);
    fprintf(report, " Col sampling interval:        %d\n", sample_cols);
    fprintf(report, "\n");
    fflush (report);

    x = (CELL *) G_malloc (ref.nfiles * sizeof(CELL));

    I_cluster_begin (&C,ref.nfiles);

    count = 0;
    for (row = sample_rows-1; row < nrows; row += sample_rows)
    {
	for (n=0; n < ref.nfiles; n++)
	    if (G_get_map_row (cellfd[n], cell[n], row) < 0)
		exit(1);
	for (col = sample_cols-1; col < ncols; col += sample_cols)
	{
	    count++;
	    for (n=0; n < ref.nfiles; n++)
		x[n] = cell[n][col];
	    if(I_cluster_point (&C, x) < 0)
	    {
		error = "Out of Memory. Please run again and choose a smaller sample size";

		done();
		exit(1);
	    }
	}
    }
    fprintf (report, "Sample size: %d points\n", C.npoints);
    fprintf (report, "\n");
    if (count < 2)
    {
	error = "Not enough sample points. Please run again and choose a larger sample size";
	done();
	exit(1);
    }

    if (C.npoints < 2)
    {
	error = "Not enough non-zero sample data points. Check your current region (and mask)";
	done();
	exit(1);
    }

    for (n=0; n < ref.nfiles; n++)
    {
	free (cell[n]);
	G_close_cell (cellfd[n]);
    }
    free(x);

    start_time = time(NULL);
    I_cluster_exec(&C,maxclass,iters,conv,sep,mcs,checkpoint,&interrupted);

    fprintf (report,"\n########## final results #############\n");
    fprintf (report,"%d classes (convergence=%.1lf%%)\n",
	I_cluster_nclasses(&C,mcs), (double)C.percent_stable);
    print_separability(report,&C);
    print_class_means(report,&C);

    if((fd = I_fopen_signature_file_new (group, subgroup, outsigfile)) != NULL)
    {
	I_write_signatures (fd, &C.S);
	fclose (fd);
    }
    else
	error = "could not write signature file";
    done();
    exit(0);
}
done()
{
    fprintf (report, "\n\n#################### CLASSES ####################\n");
    fprintf (report, "\n%d classes, %.2lf%% points stable\n",
        I_cluster_nclasses(&C,1), (double) C.percent_stable);
    fprintf (report, "\n######## CLUSTER END (%s) ########\n", G_date());
    fclose (report);
}
