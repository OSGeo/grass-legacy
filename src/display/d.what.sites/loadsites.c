#include <string.h>
#include <stdlib.h>
#include "local_proto.h"

#define SITE_BLOCK 512


int load_sites (int n, struct Cell_head *wind, int verbose)
{
FILE *fp;
char *site_map;
int ndim, nstr, ndec;
RASTER_MAP_TYPE rtype;
int s_alloc=0, snum=0, outside=0, tot_mem=0;
char *str;

    /* open sites */
    str = strchr(site[n], '@');
    if (NULL == (site_map = G_find_file2 ("site_lists", site[n],
				    (str ? str+1 : "")))){
	    fprintf (stderr,  "Could not find file '%s'\n", site[n]);
	    return(1);
    }

    fp = G_fopen_sites_old (site[n], site_map);
    if (fp == NULL){
	    fprintf (stderr, "can't open sites file [%s]\n", site[n]);
	    return(1);
    }
    fprintf(stderr,"loading %s...", site[n]);


    /* load sites */
    Wind = wind;
    rtype = -1;
    G_site_describe (fp, &ndim, &rtype, &nstr, &ndec);
    /* use these for allocation */

    if((CurSites[n] = (Site **)malloc(SITE_BLOCK*sizeof(Site *)))
	     == NULL){
	fprintf(stderr,"site malloc failed-not enough memory");
	return(1);
    }
    s_alloc = SITE_BLOCK;

    CurSites[n][snum] = G_site_new_struct (rtype, ndim, nstr, ndec);
     fprintf(stderr,"NDIM=%d, RTYPE = %d, NSTR=%d, NDEC=%d\n",
		     ndim, rtype, nstr, ndec );

    while(G_site_get (fp, CurSites[n][snum]) >= 0){
	
	if( G_site_in_region (CurSites[n][snum], wind) ){

	    if(nstr) compress_cached_site(CurSites[n][snum]);
	    tot_mem += site_mem(CurSites[n][snum]);
	    snum++;
	    if (snum == s_alloc){   /* need more memory */
		
		if((CurSites[n] = (Site **)G_realloc(CurSites[n],
			(s_alloc + SITE_BLOCK)*sizeof(Site *))) ==NULL){
		    fprintf(stderr,"site malloc failed-not enough memory");
		    return(1);
		}    
		s_alloc += SITE_BLOCK;
	    }
	    CurSites[n][snum] = G_site_new_struct (rtype, ndim, nstr, ndec);
	    if(!(snum%100) && verbose) fprintf(stderr,"%6d\b\b\b\b\b\b", snum);
	}
	else{
	    outside++;
	}
    }

    fclose(fp);

    G_site_free_struct (CurSites[n][snum]);
    Snum[n] = snum;
    
    if(verbose){
	fprintf(stderr,"Total sites cached: %d\n", snum);
	fprintf(stderr,"Minimum sites memory used: %.3f Kb\n", tot_mem/1000.);
	fprintf(stderr,"Total sites outside region: %d\n", outside);
    }

    return 0;
}

int site_mem (Site *s)
{
int i, tot;

    tot = sizeof(Site);
    tot += s->dim_alloc * sizeof(double);
    tot += s->dbl_alloc * sizeof(double);
    tot += s->str_alloc * sizeof(char *);

    for (i=0; i<s->str_alloc; i++){
	tot += strlen(s->str_att[i]);
	tot++; 
    }

    return(tot);

}

int compress_cached_site (Site *s)
{
char *tofree;
int i;

    for (i=0; i<s->str_alloc; i++){
	tofree=s->str_att[i];
	s->str_att[i]=G_store(tofree);
	G_free(tofree);
    }

    return 0;
}

int free_cached_sites (void)
{

    while (nsites){
	nsites--;
        while (Snum[nsites]){
            G_site_free_struct (CurSites[nsites][--Snum[nsites]]);
        }
	G_free(CurSites[nsites]);
	CurSites[nsites] = NULL;
    }
    G_free(CurSites);
    CurSites = NULL;

    return 0;
}


Site *
closest_site (int n, double pick_e, double pick_n)
{
int i;
double dsq, mdsq, de, dn;
int idx = -1;

/* pick_e already "adjusted? */
    for (i = 0 ; i< Snum[n]; i++){
	de = pick_e - G_adjust_easting (CurSites[n][i]->east, Wind);
	dn = pick_n - CurSites[n][i]->north;
	dsq = de * de + dn * dn; 
	if( idx < 0 || dsq < mdsq){
	    mdsq = dsq;
	    idx = i;
	}
    }

    return(CurSites[n][idx]);

}
