#include "imagery.h"
#include "parms.h"
#include "files.h"
main(argc,argv) char *argv[];
{
    struct parms parms; /* command line parms */
    struct files files; /* file descriptors, io, buffers */
    struct SigSet S;

    G_gisinit (argv[0]);
    parse (argc,argv, &parms);
    openfiles (&parms, &files);
    read_signatures (&parms, &S);
    create_output_labels (&S, &files);

    segment (&S, &parms, &files);

    closefiles(&parms, &files);
    exit(0);
}
