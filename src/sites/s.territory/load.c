/* Adapted from SG3d
*/
#include <stdlib.h>
#include <string.h>
#include "gis.h"
#include "site.h"
#include "local_proto.h"

#define SITE_BLOCK 512


/*
 ************************************************************
 * loads & caches sites with optional string compression
 * If wind != NULL, will only select those in wind region
 ************************************************************
*/
Site ** gu_load_cached_sites (
    FILE *sfd,
    int comp_str,   /* flag to compress string attributes */
    int ignore_str,   /* flag to ignore (don't cache) string attributes */
    int quiet,
    struct Cell_head *wind,
    int *nsites
)
{
int ndim, nstr, ndec;
RASTER_MAP_TYPE rtype;
int s_alloc=0, snum=0, outside=0, tot_mem=0;
Site **newsites;
int in_region;

    in_region = (wind != NULL);
    rtype = -1;
    G_site_describe (sfd, &ndim, &rtype, &nstr, &ndec);
    if(!quiet)
	fprintf(stderr,"NDIM=%d, RTYPE = %d, NSTR=%d, NDEC=%d\n", 
		ndim, rtype, nstr, ndec );

    /* use these for allocation */

    if((newsites = (Site **)malloc(SITE_BLOCK*sizeof(Site *)))
	     == NULL){
	fprintf(stderr,"site malloc failed-not enough memory");
	return(NULL);
    }
    s_alloc = SITE_BLOCK;

    newsites[snum] = G_site_new_struct (rtype, ndim, nstr, ndec);

    while(G_site_get (sfd, newsites[snum]) >= 0){
	
	if( !in_region || G_site_in_region (newsites[snum], wind) ){

	    if(nstr & (comp_str || ignore_str))
		compress_cached_site(newsites[snum], ignore_str);
	    tot_mem += site_mem(newsites[snum], ignore_str);
	    snum++;

	    if (snum == s_alloc){   /* need more memory */
		
		if((newsites = (Site **)realloc(newsites,
			(s_alloc + SITE_BLOCK)*sizeof(Site *))) ==NULL){
		    fprintf(stderr,"site malloc failed-not enough memory");
		    return(NULL);
		}    
		s_alloc += SITE_BLOCK;
	    }
	    newsites[snum] = G_site_new_struct (rtype, ndim, nstr, ndec);
	    if(!quiet)
		if(!(snum%100)) fprintf(stderr,"%6d\b\b\b\b\b\b", snum);
	}
	else{
	    outside++;
	}
    }
    G_site_free_struct (newsites[snum]);

    if(!quiet){
	fprintf(stderr,"Total sites cached: %d\n", snum);
	fprintf(stderr,"Minimum sites memory used: %.3f Kb\n", tot_mem/1000.);
	fprintf(stderr,"Total sites ignored: %d\n", outside);
    }

    *nsites = snum;

    return (newsites);
}

int site_mem (Site *s, int nostrings)
{
int i, tot;

    tot = sizeof(Site);
    tot += s->dim_alloc * sizeof(double);
    tot += s->dbl_alloc * sizeof(double);
    tot += s->str_alloc * sizeof(char *);

    if(nostrings) return (tot);
    
    for (i=0; i<s->str_alloc; i++){
	tot += strlen(s->str_att[i]);
	tot++; 
    }
    return(tot);

}

int compress_cached_site (Site *s, int zerostrings)
{
char *tofree;
int i;

    for (i=0; i<s->str_alloc; i++){
	tofree=s->str_att[i];
	if(zerostrings) 
	    s->str_att[i]=NULL;
	else 
	    s->str_att[i]=G_store(tofree);
	free(tofree);
    }

    return 0;
}

int gu_free_cached_sites (Site **tofree, int nsites)
{

    while (nsites > 0){
	G_site_free_struct (tofree[--nsites]);
    }
    free(tofree);

    return 0;
}


