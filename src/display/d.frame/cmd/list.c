#include "gis.h"
#include "raster.h"
#include <stdio.h>
#include <stdlib.h>

/* List the maps currently displayed in GRASS monitor MN + Huidae Cho 8/2001 */

int 
main (void)
{
    char **pads;
    char **items;
    char **list;
    int npads;
    int nitems;
    int count;
    int p;
    int i, j;
    int n ;
    int stat ;
    char **rast, **vect, **site;
    int nrasts, nvects, nsites, nlists;
    char *cmd;
    
    rast = vect = site = NULL;
    nrasts = nvects = nsites = 0;

    if (R_open_driver() != 0)
	G_fatal_error ("No graphics device selected");


    R_pad_list(&pads, &npads);

    stat = R_pad_select("");
    if(stat)
	G_fatal_error ("Failed to process the screen pad");

    for(p=0; p<npads; p++)
    {
	R_pad_select(pads[p]);
/*	fprintf(stdout, "frame: %s\n", pads[p]);*/

        if(D_get_cell_list (&rast, &nrasts) < 0)
            rast = NULL;
        else
        {
            rast = (char **)G_realloc(rast, (nrasts+1)*sizeof(char *));
            rast[nrasts] = NULL;
        }

        if(D_get_dig_list (&vect, &nvects) < 0)
            vect = NULL;
        else
        {
            vect = (char **)G_realloc(vect, (nvects+1)*sizeof(char *));
            vect[nvects] = NULL;
        }

        if(D_get_site_list (&site, &nsites) < 0)
            site = NULL;
        else
        {
            site = (char **)G_realloc(site, (nsites+1)*sizeof(char *));
            site[nsites] = NULL;
        }

        stat = D_get_list(&list, &nlists);

        cmd = NULL;
        if (stat < 0 || !nlists)
        {
	    fprintf(stderr, "ERROR: can not get \"list\" items\n");
        }
        else
        {
	    cmd = (char *)G_malloc(strlen(list[0])+1);
	    strcpy(cmd, list[0]);
	    for(i=1; i<nlists; i++)
	    {
	        cmd = (char *)G_realloc(cmd, strlen(cmd)+strlen(list[i])+2);
	        strcat(cmd, "\n");
	        strcat(cmd, list[i]);
	    }
/*	    fprintf(stderr, "cmd lines:\n%s\n",cmd); */
        }
    
    /*print a comma separated list: */
        fprintf(stdout, "rast:");
        for(i=0; i<nrasts; i++)
        {
            if (i>0) fprintf(stdout, ",");
            fprintf(stdout, "%s", rast[i]);
        }

        fprintf(stdout, "\nvect:");
        for(i=0; i<nvects; i++)
        {
            if (i>0) fprintf(stdout, ",");
            fprintf(stdout, "%s", vect[i]);
        }

        fprintf(stdout, "\nsite:");
        for(i=0; i<nsites; i++)
        {
            if (i>0) fprintf(stdout, ",");
            fprintf(stdout, "%s", site[i]);
        }
        
        fprintf(stdout,"\n");

        if (rast)
           R_pad_freelist(rast, nrasts);
           
        if (vect)
           R_pad_freelist(vect, nvects);
                      
        if (site)
           R_pad_freelist(site, nsites);

        if (list)
           R_pad_freelist(list, nlists);

	fprintf(stdout, "\n");
    }

    if (pads)
	R_pad_freelist(pads, npads);

    R_close_driver();

    exit(0);
}

