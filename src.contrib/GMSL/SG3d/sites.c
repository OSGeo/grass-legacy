/*
** Fri Sep 22 09:50:24 CDT 1995
** Changed to use new sites format.
** Also uses cache of sites in region now instead of rereading 
**   file on each display.
** Turn off by commenting out #define CACHE_SITES
*/

/*
** Written winter 1991 - 1992 by Bill Brown
** US Army Construction Engineering Research Lab
*/

/*
** Copyright USA CERL 1992. All rights reserved.
*/


#include "gis.h"
#include "externs.h"
#include "stdlib.h"
#include "site.h"

extern void update_sitefields();
double fabs();


#define ST_FIELD(fields, sf) ((Sadd[sf] + fields[Sfieldno[sf]])*Smult[sf])

double G_adjust_easting();
double atof();

int viewcell_interp();
void draw_x();

#define CACHE_SITES
#define SITE_BLOCK 512
Site **CurSites;
int NSites;

static Site *Sstat_min, *Sstat_max;
static char Sname[128];
static Site_head Shd;

init_sitemods()
{
int i;

    for(i=0; i<NUM_ST_ATTS; i++){
	Sadd[i] = 0.;
	Smult[i] = 1.;
    }

}

load_cached_sites(sfd)
FILE *sfd;
{
int ndim, nstr, ndec;
RASTER_MAP_TYPE rtype;
int s_alloc=0, snum=0, outside=0, tot_mem=0;

    rtype = -1;
    G_site_describe (sfd, &ndim, &rtype, &nstr, &ndec);
fprintf(stderr,"NDIM=%d, RTYPE = %d, NSTR=%d, NDEC=%d\n", ndim, rtype, nstr, ndec );
    /* use these for allocation */

    if((CurSites = (Site **)malloc(SITE_BLOCK*sizeof(Site *)))
	     == NULL){
	fprintf(stderr,"site malloc failed-not enough memory");
	return(NULL);
    }
    s_alloc = SITE_BLOCK;

    CurSites[snum] = G_site_new_struct (rtype, ndim, nstr, ndec);

    while(G_site_get (sfd, CurSites[snum]) != -1){
	
	if( G_site_in_region (CurSites[snum], &wind) ){

	    if(nstr) compress_cached_site(CurSites[snum]);
	    addto_stats(CurSites[snum], !snum);
	    tot_mem += site_mem(CurSites[snum]);
	    snum++;
	    if (snum == s_alloc){   /* need more memory */
		
		if((CurSites = (Site **)realloc(CurSites,
			(s_alloc + SITE_BLOCK)*sizeof(Site *))) ==NULL){
		    fprintf(stderr,"site malloc failed-not enough memory");
		    return(NULL);
		}    
		s_alloc += SITE_BLOCK;
	    }
	    CurSites[snum] = G_site_new_struct (rtype, ndim, nstr, ndec);
if(!(snum%100)) fprintf(stderr,"%6d\b\b\b\b\b\b", snum);
	}
	else{
	    outside++;
	}
    }
    G_site_free_struct (CurSites[snum]);
    NSites = snum;

    fprintf(stderr,"Total sites cached: %d\n", NSites);
    fprintf(stderr,"Minimum sites memory used: %.3f Kb\n", tot_mem/1000.);
    fprintf(stderr,"Total sites outside region: %d\n", outside);

    if(snum > CacheSites){  /* currently CacheSites # is not enforced */
	fprintf(stderr,"To disable sites caching:\n");
	fprintf(stderr,"\tsetenv SG3D_SITE_CACHE 0\n");
	fprintf(stderr,"\tand restart SG3d\n");
    }

}

site_mem(s)
Site *s;
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

compress_cached_site(s)
Site *s;
{
char *tofree;
int i;

    for (i=0; i<s->str_alloc; i++){
	tofree=s->str_att[i];
	s->str_att[i]=G_store(tofree);
	free(tofree);
    }

}

addto_stats(s, reset)
Site *s;
int reset;
{
int i;

    if(reset){

	Sstat_max->ccat = Sstat_min->ccat = s->ccat;
	Sstat_max->fcat = Sstat_min->fcat = s->fcat;
	Sstat_max->dcat = Sstat_min->dcat = s->dcat;

	for (i=0; i<s->dbl_alloc; i++)
	    Sstat_max->dbl_att[i] = Sstat_min->dbl_att[i] = s->dbl_att[i];

	for (i=0; i<s->dim_alloc; i++)
	    Sstat_max->dim[i] = Sstat_min->dim[i] = s->dim[i];

	Sstat_max->east = Sstat_min->east = s->east;
	Sstat_max->north = Sstat_min->north = s->north;

	return(0);
    }
    
    switch(s->cattype){
	case  CELL_TYPE:
	    if(s->ccat < Sstat_min->ccat)
		Sstat_min->ccat = s->ccat;
	    else if(s->ccat > Sstat_max->ccat)
		Sstat_max->ccat = s->ccat;
	    break;
	case FCELL_TYPE:
	    if(s->fcat < Sstat_min->fcat)
		Sstat_min->fcat = s->fcat;
	    else if(s->fcat > Sstat_max->fcat)
		Sstat_max->fcat = s->fcat;
	    break;
	case DCELL_TYPE:
	    if(s->dcat < Sstat_min->dcat)
		Sstat_min->dcat = s->dcat;
	    else if(s->dcat > Sstat_max->dcat)
		Sstat_max->dcat = s->dcat;
	    break;
    }
    for (i=0; i<s->dbl_alloc; i++){
	if(s->dbl_att[i] < Sstat_min->dbl_att[i])
	    Sstat_min->dbl_att[i] = s->dbl_att[i];
	else if(s->dbl_att[i] > Sstat_max->dbl_att[i])
	    Sstat_max->dbl_att[i] = s->dbl_att[i];
    }
    for (i=0; i<s->dim_alloc; i++){
	if(s->dim[i] < Sstat_min->dim[i])
	    Sstat_min->dim[i] = s->dim[i];
	else if(s->dim[i] > Sstat_max->dim[i])
	    Sstat_max->dim[i] = s->dim[i];
    }

    if(s->east < Sstat_min->east)
	Sstat_min->east = s->east;
    else if(s->east > Sstat_max->east)
	Sstat_max->east = s->east;

    if(s->north < Sstat_min->north)
	Sstat_min->north = s->north;
    else if(s->north > Sstat_max->north)
	Sstat_max->north = s->north;

    return(0);

}

#define TRY_LABELS

void
sites_describe()
{
char rtypestr[32];
int i;
int got_labels=0;
#ifdef TRY_LABELS
char dimlabel[MAX_ST_ATTS][40];
char catlabel[40];
char strlabel[MAX_ST_ATTS][40];
char declabel[MAX_ST_ATTS][40];
int ndiml, nstrl, ndecl;
#endif


    if(!Site_file){
	fprintf(stderr, "\n\t - - - NO SITES FILE LOADED - - -\n\n");
	return;
    }
    if(!CacheSites){
	fprintf(stderr, "\n\t - - - NO SITES FILE CACHED - - -\n");
	fprintf(stderr, 
		"\tTo turn on sites caching, setenv SG3D_SITE_CACHE\n\n");
	return;
    }

#ifdef TRY_LABELS
    ndiml = nstrl = ndecl = 0;
    if(Shd.form && Shd.labels){
    char *pf, *pl;
    int err=0, ret;

        pf=Shd.form;
        pl=Shd.labels;
	i = 0;
	while(pf[i]){
	    switch (pf[i]) {
		case '|':
		    ret = sscanf(pl, "%[^|#@%]s", dimlabel[ndiml]);
/*
fprintf(stderr,"%d %s\n", ndiml, dimlabel[ndiml]);
*/
		    ndiml += ret;
		    pl = strchr(pl, pf[i]) + 1;
		    break;
		case '#':
		    ret = sscanf(pl, "#%[^|#@%]s", catlabel);
/*
fprintf(stderr,"%s\n", catlabel);
*/
		    if(pf[i+1] && pl[1])  pl = strchr(pl+1, pf[i+1]) ;
		    break;
		case '@':
		    ret = sscanf(pl, "@%[^|#@%]s", strlabel[nstrl]);
/*
fprintf(stderr,"%d %s\n", nstrl, strlabel[nstrl]);
*/
		    nstrl += ret;
		    if(pf[i+1] && pl[1])  pl = strchr(pl+1, pf[i+1]) ;
		    break;
		case '%':
		    ret = sscanf(pl, "%%%[^|#@%]s", declabel[ndecl]);
/*
fprintf(stderr,"%d %s\n", ndecl, declabel[ndecl]);
*/
		    ndecl += ret;
		    if(pf[i+1] && pl[1])  pl = strchr(pl+1, pf[i+1]) ;
		    break;
	    }
	    if(!pl){
		break;
	    }
	    i++;
	}

	if((ndiml == Sstat_min->dim_alloc + 2) && 
	   (nstrl == Snumstrings) && 
	   (ndecl == Sstat_min->dbl_alloc)) got_labels = 1;
/*
fprintf(stderr,"%d:    %d %d %d\n", got_labels, Sstat_min->dim_alloc + 2,
 Snumstrings,Sstat_min->dbl_alloc);
*/
    }
#endif

    switch(Sstat_min->cattype){
	case  CELL_TYPE:
	    strcpy(rtypestr,"CELL_TYPE");
	    break;
	case FCELL_TYPE:
	    strcpy(rtypestr,"FCELL_TYPE");
	    break;
	case DCELL_TYPE:
	    strcpy(rtypestr,"DCELL_TYPE");
	    break;
	default:
	    strcpy(rtypestr,"NO CATEGORY");
	    break;
    }

    fprintf(stderr, 
	"----------------------------------------------------------\n\n");
    fprintf(stderr, "SITES FILENAME: %s\n", Sname);
    fprintf(stderr, "--------------\n");

    fprintf(stderr, "\n");

    /* for now just regurgitate header - hopefully will be able to 
       match labels to fields at some point - not yet supported by site lib*/

    fprintf(stderr, "Header Information:\n");
    fprintf(stderr, "------------------\n");
    if(Shd.name)
	fprintf(stderr, "\tname          %s\n", Shd.name);
    if(Shd.desc)
	fprintf(stderr, "\tdescription   %s\n", Shd.desc);
    if(Shd.form)
	fprintf(stderr, "\tformat        %s\n", Shd.form);
    if(Shd.labels)
	fprintf(stderr, "\tlabels        %s\n", Shd.labels);
    if(Shd.stime)
	fprintf(stderr, "\ttime          %s\n", Shd.stime);

    fprintf(stderr, "\n");

    fprintf(stderr, "Number of DIMENSIONS:         %d\n", 
		   Sstat_min->dim_alloc + 2);
    fprintf(stderr, "--------------------\n");
    fprintf(stderr, "\t      \t - - MIN - -\t - - MAX - -\n");
    fprintf(stderr, "\tdim  1\t%12f\t%12f\t%s\n", 
		     Sstat_min->east, Sstat_max->east,
		     got_labels? dimlabel[0]: "Easting");
    fprintf(stderr, "\tdim  2\t%12f\t%12f\t%s\n", 
		     Sstat_min->north, Sstat_max->north,
		     got_labels? dimlabel[1]: "Northing");
    for(i=0; i<Sstat_min->dim_alloc; i++){
	fprintf(stderr, "\tdim%3d\t%12f\t%12f\t%s\n", i+3,
		     Sstat_min->dim[i], Sstat_max->dim[i],
		     got_labels? dimlabel[i+2]: "");
    }

    fprintf(stderr, "\n");

    fprintf(stderr, "Type of CATEGORY information: %s\n", rtypestr);
    fprintf(stderr, "----------------------------\n");
    if(Sstat_min->cattype != -1)
	fprintf(stderr, "\t      \t - - MIN - -\t - - MAX - -\n");
    if(Sstat_min->cattype == CELL_TYPE)
	fprintf(stderr, "\t      \t%12d\t%12d\t%s\n", 
		     Sstat_min->ccat, Sstat_max->ccat, got_labels? catlabel:"");
    else if(Sstat_min->cattype == FCELL_TYPE)
	fprintf(stderr, "\t      \t%12f\t%12f\t%s\n", 
		     Sstat_min->fcat, Sstat_max->fcat, got_labels? catlabel:"");
    else if(Sstat_min->cattype == DCELL_TYPE)
	fprintf(stderr, "\t      \t%12f\t%12f\t%s\n", 
		     Sstat_min->dcat, Sstat_max->dcat, got_labels? catlabel:"");

    fprintf(stderr, "\n");

    fprintf(stderr, "Number of DOUBLE attributes:  %d\n", Sstat_min->dbl_alloc);
    fprintf(stderr, "---------------------------\n");
    if(Sstat_min->dbl_alloc)
	fprintf(stderr, "\t      \t - - MIN - -\t - - MAX - -\n");
    for(i=0; i<Sstat_min->dbl_alloc; i++){
	fprintf(stderr, "\tdbl%3d\t%12f\t%12f\t%s\n", i+1,
		     Sstat_min->dbl_att[i], Sstat_max->dbl_att[i],
		     got_labels? declabel[i]: "");
    }

    fprintf(stderr, "\n");

    fprintf(stderr, "Number of STRING attributes:  %d\n", Snumstrings);
    fprintf(stderr, "---------------------------\n");
    for(i=0; i<Snumstrings; i++){
	fprintf(stderr, "\tstr%3d\t%12s\t%12s\t%s\n", i+1, "", "",
		     got_labels? strlabel[i]: "");
    }

    /* add label IDs for each later when supported by sites lib */

    fprintf(stderr, "\n");
    fprintf(stderr, 
	   "NOTE: min/max values are only for sites in current region!\n");

    fprintf(stderr, 
	"----------------------------------------------------------\n\n");

}

free_cached_sites()
{

    while (NSites){
	G_site_free_struct (CurSites[--NSites]);
    }
    free(CurSites);
    CurSites = NULL;

}

void
new_sites()
{
    do_new_sites(NULL,NULL);
    update_sites_panel();
}

/* TODO: provide for aborting caching at env set mem level */

/* TODO: Implement draworder by doing a sort?
(don't allow if sites not cached) */

/* TODO: Implement cancel site drawing */

update_sites_panel()
{

/* TODO: check label fields, too (similar to update_sitefields) */

    if(Site_file){
	Asites->selectable = 1;
	pnl_fixact (Asites);

	Asfsize_c->selectable = Shas_cat;
	pnl_fixact (Asfsize_c);
	Asfsize2_c->selectable = Shas_cat;
	pnl_fixact (Asfsize2_c);
	Asfcolor_c->selectable = Shas_cat;
	pnl_fixact (Asfcolor_c);
	Asfz_c->selectable = Shas_cat;
	pnl_fixact (Asfz_c);
	Asfdorder_c->selectable = Shas_cat;
	pnl_fixact (Asfdorder_c);
	if(Shas_z){
	    Asfz->val = 1;
	    sprintf(PNL_ACCESS(Typein, Asfz_txt, str), "%c", 'Z');
	}

	update_sitefields(Asfsize);
	update_sitefields(Asfsize2);
	update_sitefields(Asfcolor);
	update_sitefields(Asfz);
	update_sitefields(Asfdorder);

	Asitelabcats->selectable = Shas_cat;
	Asitelabcats->val = Shas_cat? Asitelabcats->val: 0;
	pnl_fixact(Asitelabcats);

	Asitelabstrings->selectable = Snumstrings;
	Asitelabstrings->val = Snumstrings? Asitelabstrings->val: 0;
	pnl_fixact(Asitelabstrings);
    }

}


int
do_new_sites(name,mapset)
char *name, *mapset;
{
char *site_map;
int ndim, rtype; 
static int first=1;
   
    if(Site_file){
	G_site_free_struct (Sstat_min);
	G_site_free_struct (Sstat_max);

	if(CacheSites)
	    free_cached_sites();
	else
	    fclose(Sitefd);

	Site_file = 0;
    }
/*
    get_set_sitecache();
*/

    if(mapset != NULL && name != NULL) {
	site_map = mapset;
	strcpy(Sname, name);
    }
    else{
	site_map = G_ask_sites_old("enter name of sites file", Sname);
	    if (site_map == NULL){
		fprintf (stderr,  "Could not find file '%s'", Sname);
		return(0);
	    }
    }

    Sitefd = G_sites_open_old (Sname, site_map);
    if (Sitefd == NULL){
	fprintf (stderr, "can't open sites file [%s]", Sname);
	return(0);
    }
    G_site_get_head(Sitefd, &Shd);

    if(first){
	first=0;
	init_sitemods();
    }

    G_site_describe (Sitefd, &ndim, &rtype, &Snumstrings, &Snumvals);
    Shas_cat = (rtype != -1);
    Shas_z = (ndim > 2);

    Sstat_min = G_site_new_struct (rtype, ndim, 0, Snumvals);
    Sstat_max = G_site_new_struct (rtype, ndim, 0, Snumvals);
/*
fprintf(stderr,"Shas_cat = %d\nShas_z = %d rtype = %d\n",Shas_cat,Shas_z,rtype);
*/

    if(CacheSites){
	load_cached_sites(Sitefd);
	fclose(Sitefd);
    }

    Site_file = 1;

    fprintf(stderr,"%s loaded.\n", Sname);

    return(1);
    
}

void
new_sitecol()
{
char scolname[80];
char *sitecol_map;

    
    if (NULL == (sitecol_map = G_ask_old 
	("Enter name of color map to use for sites field", 
	scolname, "colr", " "))){
	fprintf (stderr,  "Could not find file '%s'", scolname);
    }
    if(sitecol_map){
	G_suppress_warnings(1);
	G_read_colors (scolname, sitecol_map, &Scolor);
	G_suppress_warnings(0);
    }

}


draw_site(s, fsite, fields)
Site *s;
float fsite[3], *fields;
{
int color;
CELL cat;
int r, g, b;
float siz, siz2, stretch, fcat;
    
    color = Blend | Dcolor[SITE_COLOR];
    stretch = 1.0;

    siz = (Asfsize->val || Asfsize_c->val? XYscale * ST_FIELD(fields, SF_SIZE): 
	    Asitesiz->val * LONGDIM/30.);

    /* siz2 defaults to double siz1 */
    siz2 = (Asfsize2->val || Asfsize2_c->val? 
	   XYscale * ST_FIELD(fields, SF_SIZE2): Asitesiz->val * LONGDIM/15.);
    cat = 0;

    if(Asfcolor->val || Asfcolor_c->val){
	fcat = (FCELL)ST_FIELD(fields, SF_COLOR);
/*
fprintf(stderr,"fcat = %f, field [%d] = %f\n", fcat,
Sfieldno[SF_COLOR],fields[Sfieldno[SF_COLOR]]);
*/
	/* look up color */
	if(G_get_f_raster_color (&fcat, &r, &g, &b, &Scolor));
	    color = Blend | r & 0xff | ((g & 0xff) << 8) | ((b & 0xff) << 16);
    }
    else if(AsfcolorRGB->val){
    double *dblvals;
    int nvals, r, g, b;

	site_to_selected_vals(s, PNL_ACCESS(Typein, AsfRGBfields, str), 
			&dblvals, &nvals);
	if(nvals == 3){
	    r = 256 * dblvals[0];
	    g = 256 * dblvals[1];
	    b = 256 * dblvals[2];
	    RGB_TO_INT(r,g,b,color);
	    color |= Blend;
	}
	else if(nvals == 1){
	    r = g = b = 256 * dblvals[0];
	    RGB_TO_INT(r,g,b,color);
	    color |= Blend;
	}
	else{
	    fprintf(stderr,"WARNING: site fields for color not defined\n");
	}
    }


    if(Aglyph1site->val){
	fcat =(Asfsize2->val || Asfsize2_c->val)?ST_FIELD(fields, SF_SIZE2): 1.;
/*
	stretch = 2.0 * Z_exag * fabs(fcat) / siz;
*/
	stretch = siz2/siz;
	if(stretch < 1.0) stretch = 1.0;  /* optional */
	    if(fcat>0){
		draw_dome(fsite,color,siz,stretch,1); /* top semisphere */
		draw_dome(fsite,color,siz,1.0,0); /* optional bottom */
	    }
	    else if(fcat<0){
		draw_dome(fsite,color,siz,1.0,1); /* optional top */
		draw_dome(fsite,color,siz,stretch,0); /* bottom semisphere */
	    }
	    else{
		draw_sphere(fsite,color,siz,stretch);
	    }
    }
	
    if(Aglyph2site->val){   /* inverse of glyph1 */
	fcat =(Asfsize2->val || Asfsize2_c->val)?ST_FIELD(fields, SF_SIZE2): 1.;
/*
	stretch = 2.0 * Z_exag * abs(fcat) / siz;
*/
	stretch = siz2/siz;
	if(stretch < 1.0) stretch = 1.0;  /* optional */
	    if(fcat>0){
		draw_dome(fsite,color,siz,1.0,1); /* optional top */
		draw_dome(fsite,color,siz,stretch,0); /* bottom semisphere */
	    }
	    else if(fcat<0){
		draw_dome(fsite,color,siz,stretch,1); /* top semisphere */
		draw_dome(fsite,color,siz,1.0,0); /* optional bottom */
	    }
	    else{
		draw_sphere(fsite,color,siz,stretch);
	    }
    }
	
    else if(Acylsite->val)
	    draw_cylinder(fsite,color,siz,siz2);
    else if(Aspheresite->val)
	    draw_sphere(fsite,color,siz,stretch);
    else if(Aoctosite->val)
	draw_octo (fsite, color, siz);
    else if(Aconetree->val)
	draw_conetree (fsite, siz);
    else if(Arndtree->val)
	draw_roundtree (fsite, siz);
    else if(Axsite->val)
	draw_x (fsite, color, siz);

}

void 
do_site_display()
{
char *desc;
double newsite[3];
float z_up, fsite[3], sitez, fields[MAX_ST_ATTS];
double e_ing, n_ing;
int cnt=0, cnt2=0;
int ndim, nstr, ndec, snum;
RASTER_MAP_TYPE rtype;
Site *tmpsite;

    if(!Site_file)
	return ;

    z_up = X_Res / 2.0;

    if(!Axsite->val)
	lmcolor(LMC_AD);

    if(getdisplaymode()){
        frontbuffer (1);
        backbuffer (0);
    }

    set_transparency();


    if(!CacheSites){
	rewind(Sitefd);
	G_site_describe (Sitefd, &ndim, &rtype, &nstr, &ndec);
	tmpsite = G_site_new_struct (rtype, ndim, nstr, ndec);
    }

    for(snum=0; (CacheSites? snum < NSites: (G_site_get (Sitefd, tmpsite) >= 0))
		&& (!check_cancel(Asites)); snum++){

	if(CacheSites)
	    tmpsite=CurSites[snum];

/*
	descript_to_vals(desc, fields);
*/
	site_to_vals(tmpsite, fields);

        e_ing = G_adjust_easting (tmpsite->east, &wind);
	n_ing = tmpsite->north;

	newsite[X] = (e_ing - wind.west) * XYscale;
	newsite[Y] = (n_ing - wind.south) * XYscale;

	if(Asfz->val || Asfz_c->val){
	    sitez = (ST_FIELD(fields, SF_Z) - (double)Zoff) * Z_exag + z_up;
	    if(1 == viewcell_interp(newsite)){
		fsite[X] = (float)newsite[X];
		fsite[Y] = (float)newsite[Y];
		if(Aconsite->val){
		    fsite[Z] = 
			    (float)(newsite[Z] - (double)Zoff) * Z_exag + z_up;
		    bgnline();
		    cpack(Dcolor[SITE_COLOR]);
		    v3f(fsite);
		    fsite[Z] = sitez; 
		    v3f(fsite);
		    endline();
		}
		else
		    fsite[Z] = sitez;

		draw_site(tmpsite, fsite, fields);

		cnt++;
	    }
	}

	else{
	    if(1 == viewcell_interp(newsite)){
		fsite[X] = (float)newsite[X];
		fsite[Y] = (float)newsite[Y];
		fsite[Z] = (float)(newsite[Z] - (double)Zoff) * Z_exag + z_up;

		draw_site(tmpsite, fsite, fields);

		cnt++;
	    }
	}

	cnt2++;
    }
    Asites->val = 0;
    Asites->dirtycnt = 2;
    pnl_fixact(Asites);

    if(!CacheSites)
	G_site_free_struct (tmpsite);

    unset_transparency();

    if(getdisplaymode()){
        frontbuffer (0);
        backbuffer (1);
    }

    if(!Axsite->val)
	lmcolor(LMC_COLOR);

    fprintf(stderr,"%d sites displayed out of %d sites read.\n", cnt, cnt2);

}     

/***********************************************************/
/* reads up to 6 floating point fields from description, 
   assigns to vals & returns number read
   (MAX_ST_ATTS currently = 6) */
/* NO LONGER USED with 4.2 Sites API*/
/***********************************************************/
int
descript_to_vals(desc, vals)
char *desc;
float *vals;
{
int numvals;

    vals[0] = vals[1] = vals[2] = vals[3] = vals[4] = vals[5] = 0.0;
    if(numvals = sscanf(desc,"%f%f%f%f%f%f", 
	    vals, vals+1, vals+2, vals+3, vals+4, vals+5)) return(numvals);
    return(numvals = sscanf(desc,"#%f%f%f%f%f%f", 
	    vals, vals+1, vals+2, vals+3, vals+4, vals+5));

}

/***********************************************************/
/* Loads category info, then dimensions, then doubles
*/

site_to_vals(s, vals)
Site *s;
float *vals;
{
static char str[80];
int i,j, ret;

    
    vals[S_X] = s->east;
    vals[S_Y] = s->north;

    switch(s->cattype){
	case  CELL_TYPE:
	    vals[S_CAT] = s->ccat;
	    break;
	case FCELL_TYPE:
	    vals[S_CAT] = s->fcat;
	    break;
	case DCELL_TYPE:
	    vals[S_CAT] = s->dcat;
	    break;
	default:
	    break;
    }
/*
    i = S_RESV + 1;
*/
/* new */
    i = S_RESV;

    for (j=0; j< s->dim_alloc && i < MAX_ST_FIELDS; j++, i++)
	vals[i] = s->dim[j];

/* new */
    i = S_RESV + (s->dim_alloc? s->dim_alloc: 1);

    for (j=0; j< s->dbl_alloc && i < MAX_ST_FIELDS; j++, i++)
	vals[i] = s->dbl_att[j];

    ret = i;
    while (i<MAX_ST_FIELDS) vals[i++] = 0.0;

    return(ret);
}


void 
do_site_labels()
{
char *desc, buf[MAX_SITE_LEN], buf1[MAX_SITE_LEN];
double newsite[3];
float z_up, fsite[3], fields[MAX_ST_ATTS];
double e_ing, n_ing;
int ndim, nstr, ndec;
RASTER_MAP_TYPE rtype;
Site *tmpsite;
int snum;
int rfield, prec, i, lastf, dir;
int err_d, err_c, err_v, err_s;
char *s_str, *f_str;
double *dblvals;
int nvals;

    if(Site_file){
	
	err_v = err_c = err_d = err_s = 0;
	set_font();
	z_up = X_Res / 2.0;

	if(1 != sscanf(PNL_ACCESS(Typein, Asitelabdec, str), "%d", &prec))
	    prec = 2;
        
	ndec = Snumvals;
	if(!CacheSites){
	    rewind(Sitefd);
	    G_site_describe (Sitefd, &ndim, &rtype, &nstr, &ndec);
	    tmpsite = G_site_new_struct (rtype, ndim, nstr, ndec);
	}

	if(Asitelabpie->val){
	    set_piechart_colors(ndec, (int)(Apiegrey->val));
	}

	for(snum=0; (CacheSites? snum < NSites: (G_site_get (Sitefd, tmpsite) 
		    >= 0)) && (!check_cancel(Asites)); snum++){

	    if(CacheSites)
		tmpsite=CurSites[snum];


	    e_ing = G_adjust_easting (tmpsite->east, &wind);
	    n_ing = tmpsite->north;
	    newsite[X] = (e_ing - wind.west) * XYscale;
	    newsite[Y] = (n_ing - wind.south) * XYscale;
	    
	    strcpy(buf,"");
	    if(Asitelabcats->val){ /* category field only */
		switch(tmpsite->cattype){
		     case CELL_TYPE:
			sprintf(buf1,"%d", tmpsite->ccat);
			break;
		     case FCELL_TYPE:
			sprintf(buf1,"%.*f", prec, tmpsite->fcat);
			break;
		     case DCELL_TYPE:
			sprintf(buf1,"%.*lf", prec, tmpsite->dcat);
			break;
		     default:
			err_c = 1;
			break;
		}
		strcpy(buf, buf1);
	    }
	    if(Asitelabstrings->val){ /* strings */
		f_str=PNL_ACCESS(Typein, Asitestrfields, str);
		lastf = 0;
		while(f_str && f_str[0]){
		    rfield = 0;
		    while(f_str[0] >= '0' && f_str[0] <= '9'){
			rfield *= 10;
			rfield += f_str[0] - '0';
			f_str++;
		    }
		    if(rfield > tmpsite->str_alloc){
			err_s=1;
			while (rfield > tmpsite->str_alloc) rfield--;
		    }
		    if(rfield){
			if(lastf){
			    dir = rfield - lastf > 0? 1 : -1;
			    for (i=lastf+dir; i != rfield; i+=dir){
				sprintf(buf1,"%s", tmpsite->str_att[i-1]);
				if(buf[0])  
				    strcat(buf, " ");
				strcat(buf, buf1);
			    }
			    lastf = 0;
			}
			sprintf(buf1,"%s", tmpsite->str_att[rfield-1]);
			if(buf[0])  
			    strcat(buf, " ");
			strcat(buf, buf1);
		    }
		    lastf = f_str[0] == '-' ? rfield: 0;
		    f_str++;
		}
	    }
	    if(Asitelabvals->val){ /* decimal values */
		f_str=PNL_ACCESS(Typein, Asitevalfields, str);
		lastf = 0;
		while(f_str && f_str[0]){
		    rfield = 0;
		    while(f_str[0] >= '0' && f_str[0] <= '9'){
			rfield *= 10;
			rfield += f_str[0] - '0';
			f_str++;
		    }
		    if(rfield > tmpsite->dbl_alloc){
			err_v=1;
			while (rfield > tmpsite->dbl_alloc) rfield--;
		    }
		    if(rfield){
			if(lastf){
			    dir = rfield - lastf > 0? 1 : -1;
			    for (i=lastf+dir; i != rfield; i+=dir){
				sprintf(buf1,"%.*lf", prec, 
						   tmpsite->dbl_att[i-1]);
				if(buf[0])  
				    strcat(buf, " ");
				strcat(buf, buf1);
			    }
			    lastf = 0;
			}
			sprintf(buf1,"%.*lf", prec, tmpsite->dbl_att[rfield-1]);
			if(buf[0])  
			    strcat(buf, " ");
			strcat(buf, buf1);
		    }
		    switch(f_str[0]){
			case 'X':
			case 'x':
			case 'e':
			case 'E':
			case 'w':
			case 'W':
			    sprintf(buf1,"%.*lf", prec, 
					tmpsite->east);
			    if(buf[0])  
				strcat(buf, " ");
			    strcat(buf, buf1);
			    lastf = 0; 
			    break;
			case 'Y':
			case 'y':
			case 'n':
			case 'N':
			case 's':
			case 'S':
			    sprintf(buf1,"%.*lf", prec, 
					tmpsite->north);
			    if(buf[0])  
				strcat(buf, " ");
			    strcat(buf, buf1);
			    lastf = 0; 
			    break;
			case 'Z':
			case 'z':
			    if(tmpsite->dim_alloc){
				sprintf(buf1,"%.*lf", prec, 
					    tmpsite->dim[0]);
				if(buf[0])  
				    strcat(buf, " ");
				strcat(buf, buf1);
			    }
			    else 
				err_d = 1;
			    lastf = 0; 
			    break;
			case '-':
			    lastf = rfield; 
			    break;
		    }
		    f_str++;
		}
	    }
	    if(!(Asitelabvals->val || 
		 Asitelabstrings->val || Asitelabcats->val)){ 
		sprintf(buf,"%c", ' ');
	    }


	    if(Asfz->val || Asfz_c->val){
		if(1 == viewcell_interp(newsite)){
		    fsite[X] = (float)newsite[X];
		    fsite[Y] = (float)newsite[Y];
		    fsite[Z] = (ST_FIELD(fields, SF_Z) - (double)Zoff) 
				* Z_exag + z_up;
		    
		    if(Asitelabpie->val){
			site_to_selected_vals(tmpsite, 
				PNL_ACCESS(Typein, Asitepiefields, str), 
				&dblvals, &nvals);
			do_piechart_displaycenter(fsite, dblvals, 
						  nvals, Asitesiz->val);
		    }
		    do_label_displaycenter(fsite, buf, (int)Asitelabbox->val);
		}
	    }

	    else{

		if(1 == viewcell_interp(newsite)){
		    fsite[X] = (float)newsite[X];
		    fsite[Y] = (float)newsite[Y];
		    fsite[Z] = (float)(newsite[Z] - (double)Zoff)*Z_exag + z_up;
		    
		    if(Asitelabpie->val){
			site_to_selected_vals(tmpsite, 
				PNL_ACCESS(Typein, Asitepiefields, str), 
				&dblvals, &nvals);
			do_piechart_displaycenter(fsite, dblvals, 
						  nvals, Asitesiz->val);
		    }
		    do_label_displaycenter(fsite, buf, (int)Asitelabbox->val);
		}
	    }
	}

	if(err_c)
	    fprintf(stderr, 
		    "WARNING: No category in site file\n");
	if(err_s)
	    fprintf(stderr, 
		    "WARNING: Specified string field not in site file\n");
	if(err_v)
	    fprintf(stderr, 
		    "WARNING: Specified value field not in site file\n");
	if(err_d)
	    fprintf(stderr, 
		    "WARNING: Specified dimension not in site file\n");

    if(!CacheSites)
	G_site_free_struct (tmpsite);
    }

}


site_to_selected_vals(s, f_str, dblvals, nvals)
Site *s;
char *f_str;
double **dblvals;
int *nvals;
{
int lastf, rfield, err_v, dir, i, num, tdim;
char tstr[40];
static double dbls[MAX_ST_ATTS];

	err_v = lastf = num = 0;
	while(f_str && f_str[0]){
	    rfield = 0;
	    while(f_str[0] >= '0' && f_str[0] <= '9'){
		rfield *= 10;
		rfield += f_str[0] - '0';
		f_str++;
	    }
	    if(rfield > s->dbl_alloc || rfield > MAX_ST_ATTS){
		err_v=1;
		while (rfield > s->dbl_alloc) rfield--;
	    }
	    if(rfield){
		if(lastf){
		    dir = rfield - lastf > 0? 1 : -1;
		    for (i=lastf+dir; i != rfield; i+=dir){
			dbls[num] = s->dbl_att[i-1];
			num++;
		    }
		    lastf = 0;
		}
		dbls[num] = s->dbl_att[rfield-1];
		num++;
	    }
	    switch(f_str[0]){
		case '-':
		    lastf = rfield; 
		    break;
	    }
	    f_str++;
	}
	*dblvals = dbls;
	*nvals = num;

	return(err_v? -1: 1);

}


site_to_selected_dims(s, f_str, dbldims, nvals)
Site *s;
char *f_str;
double **dbldims;
int *nvals;
{
int lastf, rfield, err_d, dir, i, num, tdim;
char tstr[40];
static double dbls[MAX_ST_DIMS];

	err_d = lastf = num = 0;
	while(f_str && f_str[0]){
	    rfield = 0;
	    while(f_str[0] >= '0' && f_str[0] <= '9'){
		rfield *= 10;
		rfield += f_str[0] - '0';
		f_str++;
	    }
	    if(rfield > s->dim_alloc || rfield > MAX_ST_DIMS){
		err_d=1;
		while (rfield > s->dim_alloc) rfield--;
	    }
	    if(rfield){
		if(lastf){
		    dir = rfield - lastf > 0? 1 : -1;
		    for (i=lastf+dir; i != rfield; i+=dir){
			dbls[num] = s->dim[i-1];
			num++;
		    }
		    lastf = 0;
		}
		dbls[num] = s->dim[rfield-1];
		num++;
	    }
	    switch(f_str[0]){
		case '-':
		    lastf = rfield; 
		    break;
	    }
	    f_str++;
	}
	*dbldims = dbls;
	*nvals = num;

	return(err_d? -1: 1);

}
