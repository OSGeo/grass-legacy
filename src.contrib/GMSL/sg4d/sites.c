

/*
** Written winter 1991 - 1992 by Bill Brown
** US Army Construction Engineering Research Lab
*/

/*
** Copyright USA CERL 1992. All rights reserved.
*/

/*
#include "gis.h"
*/
#include "externs.h"
#include "stdlib.h"

double G_adjust_easting();
double atof();

int viewcell_interp();
void draw_x();


void
new_sites()
{
char sname[30];
char *site_map;
    
    site_map = G_ask_sites_old("enter name of sites file", sname);
	if (site_map == NULL){
	    fprintf (stderr,  "Could not find file '%s'", sname);
	}
	else{

	    if(Site_file) fclose(Sitefd);
	    Sitefd = G_fopen_sites_old (sname, site_map);
	    if (Sitefd == NULL){
		fprintf (stderr, "can't open sites file [%s]", sname);
		Site_file = 0;
	    }
	    else{
		Site_file = 1;
		Asites->selectable = 1;
		pnl_fixact (Asites);
		fprintf(stderr,"%s loaded.\n", sname);
	    }
        } 
    
}


draw_site(fsite, desc)
float fsite[3];
char *desc;
{
long color;
CELL cat;
int r, g, b;
float siz, stretch;
    
    color = STcolor;
    stretch = 1.0;
    siz = Asitesiz->val * LONGDIM/30.;
    cat = 0;

    if (desc){
	if(Site_cat_isZ){
	    sscanf(desc,"#%*f%d", &cat);
	}
	else if (desc[0] == '#')
	    cat = atoi(&desc[1]);
	else
	    cat = atoi(desc);
    }

    if(Map_Sitecolor){
	if(Amapsite->val){
	    if(desc){
		/* look up color */
		if(G_get_color (cat, &r, &g, &b, &Scolor));
		    color = r & 0xff | ((g & 0xff) << 8) | ((b & 0xff) << 16);
	    }
	}
    }

#ifdef DO_EGG
/* now part of glyphsite */
/* set stretch */
stretch = Z_exag * abs(cat) / siz;

if(stretch < 1.0) stretch = 1.0;  
/* not really necessary, but since we draw the other half of the
   sphere when going positive & negative, seems like the sphere should
   never be flattened, only stretched - otherwise, maybe should only
   draw a cone (or conehead, rather), with a flat end instead of the
   domed end. */

#endif
    

    if(Aglyph1site->val){
	stretch = 2.0 * Z_exag * abs(cat) / siz;
	if(stretch < 1.0) stretch = 1.0;  /* optional */
	    if(cat>0){
		draw_dome(fsite,color,siz,stretch,1); /* top semisphere */
		draw_dome(fsite,color,siz,1.0,0); /* optional bottom */
	    }
	    else if(cat<0){
		draw_dome(fsite,color,siz,1.0,1); /* optional top */
		draw_dome(fsite,color,siz,stretch,0); /* bottom semisphere */
	    }
	    else{
		draw_sphere(fsite,color,siz,stretch);
	    }
    }
	
    if(Aglyph2site->val){   /* inverse of glyph1 */
	stretch = 2.0 * Z_exag * abs(cat) / siz;
	if(stretch < 1.0) stretch = 1.0;  /* optional */
	    if(cat>0){
		draw_dome(fsite,color,siz,1.0,1); /* optional top */
		draw_dome(fsite,color,siz,stretch,0); /* bottom semisphere */
	    }
	    else if(cat<0){
		draw_dome(fsite,color,siz,stretch,1); /* top semisphere */
		draw_dome(fsite,color,siz,1.0,0); /* optional bottom */
	    }
	    else{
		draw_sphere(fsite,color,siz,stretch);
	    }
    }
	
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
float z_up, fsite[3], sitez;
double e_ing, n_ing;
int cnt=0, cnt2=0;

    rewind(Sitefd);
    z_up = X_Res / 2.0;

    if(!Axsite->val)
	lmcolor(LMC_AD);

    if(getdisplaymode()){
        frontbuffer (1);
        backbuffer (0);
    }

    set_transparency();

    while(G_get_site (Sitefd, &e_ing, &n_ing, &desc) > 0){

        e_ing = G_adjust_easting (e_ing, &wind);
	newsite[X] = (e_ing - wind.west) * XYscale;
	newsite[Y] = (n_ing - wind.south) * XYscale;
	
	if(Asitelevel->val){ /* level */
	    sitez = Z_Max_real;  /* default */
	    if(PNL_ACCESS(Typein, Asitez, str)[0]){
		sscanf(PNL_ACCESS(Typein, Asitez, str),"%f",&sitez);
	    }
	    sitez = (sitez - (double)Zoff) * Z_exag + z_up;
	}
	else if(Site_cat_isZ){

		if (desc){
		if (desc[0] != '#'){
		    fprintf(stderr,"Can't use site category as Z val.\n");
		    Site_cat_isZ = 0;
		}
		else
		    sitez = (atof(&desc[1]) - (double)Zoff) * Z_exag + z_up;
	    }
	}

	if(Site_cat_isZ && !Asitelevel->val){
	    if(viewcell_interp(newsite)){
		fsite[X] = (float)newsite[X];
		fsite[Y] = (float)newsite[Y];
		fsite[Z] = sitez; 
		draw_site(fsite, desc);
		if(Aconsite->val){
		/*
		    linewidth ((short)V_Width);
		*/
		    linewidth (1);
		    bgnline();
		    /* would also have to change lmcolor
		    cpack(STcolor);
		    */
		    v3f(fsite);
		    fsite[Z] = 
			    (float)(newsite[Z] - (double)Zoff) * Z_exag + z_up;
		    v3f(fsite);
		    endline();
		}

		cnt++;
	    }
	}

	else{
	    if(viewcell_interp(newsite)){
		fsite[X] = (float)newsite[X];
		fsite[Y] = (float)newsite[Y];
		if(Asitelevel->val)
		    fsite[Z] = sitez;
		else
		    fsite[Z] = 
			(float)(newsite[Z] - (double)Zoff) * Z_exag + z_up;

		draw_site(fsite, desc);

		cnt++;
	    }
	}

	cnt2++;
    }

    unset_transparency();

    if(getdisplaymode()){
        frontbuffer (0);
        backbuffer (1);
    }

    if(!Axsite->val)
	lmcolor(LMC_COLOR);

    fprintf(stderr,"%d sites displayed out of %d sites read.\n", cnt, cnt2);

}     


