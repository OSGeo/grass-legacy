#include "imagery.h"
#include "parms.h"
#include "files.h"
main(argc,argv) char *argv[];
{
    struct parms parms; /* command line parms */
    struct files files; /* file descriptors, io, buffers */
    struct Signature S;

    G_gisinit (argv[0]);
    parse (argc,argv, &parms);
    openfiles (&parms, &files);
    read_training_labels (&parms, &files);

    get_training_classes (&files, &S);
    compute_means (&files, &S);
    compute_covariances (&files, &S);
    check_signatures (&S);
    write_sigfile (&parms, &S);
    exit(0);
}
