

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

void 
do_site_display()
{
char *desc;
double newsite[3];
float z_up, fsite[3];
double e_ing, n_ing;
int cnt=0, cnt2=0;

    rewind(Sitefd);
    z_up = X_Res / 2.0;
    while(G_get_site (Sitefd, &e_ing, &n_ing, &desc) > 0){

        e_ing = G_adjust_easting (e_ing, &wind);
	newsite[X] = (e_ing - wind.west) * XYscale;
	newsite[Y] = (n_ing - wind.south) * XYscale;
	
	if(Site_cat_isZ){
	    if (desc){
		if (desc[0] == '#'){
		    fsite[Z] = (atof(&desc[1]) - (double)Zoff) * Z_exag + z_up;
		    fsite[X] = (float)newsite[X];
		    fsite[Y] = (float)newsite[Y];
		    draw_x (fsite, STcolor, 1.0);
		    cnt++;
		}
	    }
	    /*
	    else if(viewcell_interp(newsite)){
		fsite[X] = (float)newsite[X];
		fsite[Y] = (float)newsite[Y];
		fsite[Z] = (float)(newsite[Z] - (double)Zoff) * Z_exag + z_up;
		draw_x (fsite, STcolor, 1.0);
		cnt++;
	    }
	    */
	}

	else{
	    if(viewcell_interp(newsite)){
		fsite[X] = (float)newsite[X];
		fsite[Y] = (float)newsite[Y];
		fsite[Z] = (float)(newsite[Z] - (double)Zoff) * Z_exag + z_up;
		draw_x (fsite, STcolor, 1.0);
		cnt++;
	    }
	}

	cnt2++;
    }
    fprintf(stderr,"%d sites displayed out of %d sites read.\n", cnt, cnt2);

}     


