#include <stdlib.h>
#include "imagery.h"
#include "bouman.h"

int read_signatures (struct parms *parms, struct SigSet *S)
{
    FILE *fd;
    struct Ref Ref;

    if (!I_get_subgroup_ref (parms->group, parms->subgroup, &Ref))
    {
        fprintf (stderr,
             "ERROR: unable to read REF file for subgroup [%s] in group [%s]\n",
                parms->subgroup, parms->group);
        exit(1);
    }
    if (Ref.nfiles <= 0)
    {
        fprintf (stderr, "ERROR: subgroup [%s] in group [%s] contains no files\n",
                parms->subgroup, parms->group);
        exit(1);
    }

    fd = I_fopen_sigset_file_old (parms->group, parms->subgroup, parms->sigfile);
    if (fd == NULL)
    {
	fprintf (stderr, "ERROR: signature file [%s] missing or not readable\n",
		parms->sigfile);
	exit(1);
    }
    if(I_ReadSigSet (fd, S) < 0 || Ref.nfiles != S->nbands)
    {
	fprintf (stderr, "ERROR: signature file [%s] invalid\n",
		parms->sigfile);
	exit(1);
    }
    fclose (fd);

    return 0;
}
