#include "global.h"
checkpoint(X,n)
    struct Cluster *X;
{
    long time(), elapsed_time, cur_time;
    int c, band;

    switch(n)
    {
    case 1:
	print_band_means(report,X);
	if (insigfile)
	{
	    fprintf (report, "using seed means (%d files)\n", ref.nfiles);
	    for (c = 0 ; c < in_sig.nsigs; c++)
		for (band = 0; band < ref.nfiles; band++)
		    X->mean[band][c] = in_sig.sig[c].mean[band];
	}
	print_seed_means(report,X);
	break;
    case 2:
	print_class_means(report,X);
	print_distribution(report,X);
	break;
    case 3:
	fprintf (report, "\n");
	fprintf (report, "######## iteration %d ###########\n",
		X->iteration);
	fprintf (report, "%d classes, %.2lf%% points stable\n",
	    I_cluster_nclasses(X,1), (double) X->percent_stable);
    /*
	I_cluster_sum2 (X);
	print_class_means(report,X);
    */
	print_distribution(report,X);
	if (verbose) {
	     cur_time = time(NULL);
	     elapsed_time = cur_time - start_time;
	     fprintf (stderr, "Iteration %d: %%Convergence %.2lf ",
		X->iteration, (double) X->percent_stable);
	     fprintf (stderr, "(");
	     print_time (elapsed_time);
	     fprintf (stderr, " elapsed, ");
	     print_time (iters*elapsed_time/(X->iteration+1) - elapsed_time);
	     fprintf (stderr, " left)\n");
	     fflush (stderr);
	}
	break;
    case 4:
    /*
	fprintf (report, "\nmerging class %d into %d\n",
		X->merge2+1, X->merge1+1);
    */
	break;
    }
    fflush (report);
    return 1;
}
