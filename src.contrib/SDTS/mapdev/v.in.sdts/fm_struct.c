#include <stdio.h>
#include <string.h>
#include "sdts_in.h"

extern char Error_msg[];

struct sdts_fm * alloc_sdts_fm ();
char *G_malloc();
char *G_realloc();

init_Sdts_info (S_info)
   struct Sdts_info *S_info;
{
    S_info->n_files = 0;
    S_info->n_manifolds = 0;
	S_info->S_catd = NULL;
	S_info->S_mfold = NULL;
}


struct Sdts_catd *
alloc_Sdts_catd_rec (S_info)
    struct Sdts_info *S_info;
{
    struct Sdts_catd *ptr;
    int num;

    ptr = S_info->S_catd;

    num = S_info->n_files;

    if (S_info->n_files == 0) {
        if ((ptr = (struct Sdts_catd *) G_malloc (sizeof (struct Sdts_catd)))==0)
            G_fatal_error ("Can't allocate memory for CATD struct, first time. Exiting.");
	    G_zero (ptr, sizeof (struct Sdts_catd));
    }
    else {
        if ((ptr = (struct Sdts_catd *) G_realloc ((char *) ptr, (num + 1) * sizeof (struct Sdts_catd)))==0) {
            sprintf (Error_msg, "Can't allocate memody for CATD struct, %dth time. Exiting.\n", num +1);
            G_fatal_error (Error_msg);
         }
	     G_zero (ptr + num,  sizeof (struct Sdts_catd));
    }

    if (ptr)
        S_info->n_files++;

    return (ptr);
}

struct Sdts_manifold *
alloc_Sdts_manifold_struct (S_info)
    struct Sdts_info *S_info;
{
    struct Sdts_manifold *ptr;
    int num;

    ptr = S_info->S_mfold;

    num = S_info->n_manifolds;

    if (S_info->n_manifolds == 0) {
        if ((ptr = (struct Sdts_manifold *) G_malloc (sizeof (struct Sdts_manifold)))==0)
            fprintf (stderr,"Can't allocate memory for Manifold struct, first time. Exiting.");
        else
	  G_zero (ptr, sizeof (struct Sdts_manifold));
    }
    else {
        if ((ptr = (struct Sdts_manifold *) G_realloc ((char *) ptr, (num + 1) * sizeof (struct Sdts_manifold)))==0) {
            sprintf (Error_msg, "Can't allocate memory for Manifold struct, %dth time. Exiting.\n", num +1);
            fprintf (stderr, "%s", Error_msg);
         }
        else
	  G_zero (ptr,  (num + 1) * sizeof (struct Sdts_manifold));
    }

    if (ptr)
        S_info->n_manifolds++;

    return (ptr);
}
