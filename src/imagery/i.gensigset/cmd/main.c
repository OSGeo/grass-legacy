#include "imagery.h"
#include "parms.h"
#include "files.h"
main(argc,argv) char *argv[];
{
    struct parms parms; /* command line parms */
    struct files files; /* file descriptors, io, buffers */
    struct SigSet S;
    int i;
    int junk;

    G_gisinit (argv[0]);
    parse (argc,argv, &parms);
    openfiles (&parms, &files);
    read_training_labels (&parms, &files);

    get_training_classes (&parms, &files, &S);
    read_data (&files, &S);
    for (i = 0; i< S.nclasses; i++)
    {
	printf ("Clustering class %d, with %d pixels\n", 
		i+1, S.ClassSig[i].ClassData.npixels); 
	subcluster (&S, i, &junk);
	printf (" Solution: Number of subclasses is %d\n", S.ClassSig[i].nsubclasses);
    }
    write_sigfile (&parms, &S);
    exit(0);
}
