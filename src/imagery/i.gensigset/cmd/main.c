#include "imagery.h"
#include "files.h"
#include "parms.h"
#include "local_proto.h"

int main (int argc, char *argv[])
{
    struct parms parms; /* command line parms */
    struct files files; /* file descriptors, io, buffers */
    struct SigSet S;
    int i;
    int junk;
	struct GModule *module;

    G_gisinit (argv[0]);

	module = G_define_module();
	module->description =
		"Generate statistics for i.smap from raster map layer.";

    parse (argc,argv, &parms);
    openfiles (&parms, &files);
    read_training_labels (&parms, &files);

    get_training_classes (&parms, &files, &S);
    read_data (&files, &S);

    for (i = 0; i< S.nclasses; i++)
    {
	fprintf (stderr, "Clustering class %d, with %d pixels\n", 
		i+1, S.ClassSig[i].ClassData.npixels); 
	subcluster (&S, i, &junk, parms.maxsubclasses);
	fprintf (stderr, " Solution: Number of subclasses is %d\n", S.ClassSig[i].nsubclasses);
    }
    write_sigfile (&parms, &S);
    exit(0);
}
