#include "imagery.h"
#include "parms.h"
write_sigfile (parms, S)
    struct parms *parms;
    struct Signature *S;
{
    FILE *fd;

    fd = I_fopen_signature_file_new (parms->group, parms->subgroup, parms->sigfile);
    if (fd == NULL)
    {
	fprintf (stderr, "ERROR: unable to create signature file [%s] ", parms->sigfile);
	fprintf (stderr, "for subgroup [%s] in group [%s]\n",
		parms->subgroup, parms->group);
	exit(1);
    }
    fprintf (stderr, "Writing signature file [%s] ...", parms->sigfile);
    fflush (stderr);
    I_write_signatures (fd, S);
    fprintf (stderr, "\n");
}
