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
    FILE *fd;

    G_gisinit(me = argv[0]);
    if (G_maskfd() >= 0)
    {
	printf ("\nWARNING: you have your mask set.\n");
	if (!G_yes("Do you want to continue? ", -1)) exit(0);
    }
    G_get_window (&window);
    nrows = G_window_rows();
    ncols = G_window_cols();

    maxclass = 15;
    conv = 98.0;
    sep = .5;
    iters = 30;
    mcs = 17;	/* minimum class size */

    group[0] = 0;
    outsigfile[0] = 0;
    insigfile[0] = 0;
    I_cluster_clear (&C);

    ask_files (argv[0]);
    ask_parms();
#ifndef NOFORK
    if (G_fork())
    {
	printf("You will be notified by mail when %s is complete\n",
	    argv[0]);
	exit(0);
    }

    freopen ("/dev/null", "w", stderr);

    reportfile = G_tempfile();
    report = fopen (reportfile,"w");
    if (report == NULL)
    {
	G_fatal_error ("unable to create any temp files");
	exit(1);
    }
#else

    /*
    freopen ("report.out", "w", stderr);
    */
    report = stderr;
    setbuf (report, NULL);

#endif

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

    I_cluster_exec(&C,maxclass,iters,conv,sep,mcs,checkpoint,&interrupted);

    fprintf (report,"\n########## final results #############\n");
    fprintf (report,"%d classes (convergence=%.1lf%%)\n",
	I_cluster_nclasses(&C,mcs), (double)C.percent_stable);
    print_separability(report,&C);
    print_class_means(report,&C);

    if(fd = I_fopen_signature_file_new (group, subgroup, outsigfile))
    {
	I_write_signatures (fd, &C.S);
	fclose (fd);
	fprintf (report, "\nEND (%s)\n", G_date());
    }
    else
	error = "could not write signature file";
    done ();
    exit(0);
}
done()
{
    FILE *popen(), *mail;
    char buf[1024];

#ifndef NOFORK
    fclose (report);
    report = fopen (reportfile, "r");
    sprintf (buf, "mail '%s'", G_whoami());
    mail = popen (buf,"w");
#else
    mail = stderr;
#endif
    if (mail)
    {
	fprintf (mail, "Subject: %s\n", me);
	fprintf (mail, "%s complete\n\n", me);
	if (error)
	{
	    fprintf (mail, "** ERROR **\n");
	    fprintf (mail, "%s\n\n",error);
	}
	else
	    fprintf (mail, "%d classes, %.2lf%% points stable\n",
		I_cluster_nclasses(&C,1), (double) C.percent_stable);
#ifndef NOFORK
	if (report)
	    while (fgets(buf,sizeof buf,report))
		fprintf (mail, "%s", buf);
	pclose (mail);
	unlink (reportfile);
	G_done_msg ("Check your mail");
#endif
    }
}
