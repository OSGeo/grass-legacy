#include "imagery.h"
#include "signature.h"
#include "parms.h"
#include "files.h"
#include "local_proto.h"

int main (int argc, char *argv[])
{
    struct parms parms; /* command line parms */
    struct files files; /* file descriptors, io, buffers */
    struct Signature S;
	struct GModule *module;

    G_gisinit (argv[0]);

	module = G_define_module();
	module->description =
		"Generates statistics for i.maxlik "
		"from raster map layer.";

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
