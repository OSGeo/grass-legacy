/****************************************************************************
 *
 * MODULE:       r.coin
 *
 * AUTHOR(S):    Michael O'Shea - CERL
 *               Michael Shapiro - CERL
 *
 * PURPOSE:      Calculates the coincidence of two raster map layers.
 *
 * COPYRIGHT:    (C) 2006 by the GRASS Development Team
 *
 *               This program is free software under the GNU General Public
 *               License (>=v2). Read the file COPYING that comes with GRASS
 *               for details.
 *
 ***************************************************************************/

#include <stdlib.h>
#include "coin.h"
#include <grass/glocale.h>


int interactive_version (void)
{
    int  cols;
    char key;
    char line[128],outname[128],command[256];
    char ans[80];

    setbuf(stderr,NULL);

    G_system("clear");
    G_message(_("GIS Coincidence Tabulation Facility\n"));
    G_message(_("This utility will allow you to compare the "
                "coincidence of two map layers\n"));

    mapset1 = G_ask_cell_old("Enter Name of Map Layer 1",map1name);
    if(!mapset1)
	    exit (0);

    mapset2 = G_ask_cell_old("Enter Name of Map Layer 2",map2name);
    if(!mapset2)
	    exit (0);

    make_coin();
    check_report_size();


    while(1)
    {
	G_system("clear");
	G_message(_("The report can be made in one of 8 units."));
	G_message(_("Please choose a unit by entering one of the "
                    "following letter codes:"));
	G_message(_("     'c': cells"));
	G_message(_("     'p': percent cover of region"));
	G_message(_("     'x': percent of '%s' category (column)"), map1name);
	G_message(_("     'y': percent of '%s' category (row)"), map2name);
	G_message(_("     'a': acres"));
	G_message(_("     'h': hectares"));
	G_message(_("     'k': square kilometers"));
	G_message(_("     'm': square miles\n"));
	G_message(_("     'Q': quit"));
	fprintf(stderr,"> ");

	*ans = 0;
	if(!G_gets(ans)) continue;
	if(sscanf(ans,"%c",&key) != 1)
		continue;

	switch (key){
	case 'c':
	case 'p':
	case 'x':
	case 'y':
	case 'a':
	case 'h':
	case 'k':
	case 'm':
	    print_coin(key,80,1);
	    break;
	case 'Q': exit(0);
	default: continue;
	}

	sprintf(command,"$GRASS_PAGER %s",dumpname);
	G_system(command);

	while(1)
	{
	    fprintf(stderr, _("Do you wish to save this report in a file? (y/n) [n] "));
	    *ans = 0;
	    if(!G_gets(ans))continue;
	    G_strip (ans);
	    if(ans[0] != 'y' && ans[0] != 'Y') break;

	    fprintf(stderr, _("Enter the file name\n> "));
	    if (!G_gets(line)) continue;
	    if(sscanf(line,"%s",outname) != 1) continue;
	    if(outname[0] != '/'){
		sprintf(command,"cp %s %s/%s",dumpname,G_home(),outname);
		fprintf(stderr, _("'%s' being saved in your home directory"), outname);
	    }
	    else{
		sprintf(command,"cp %s %s", dumpname, outname);
		fprintf(stderr, _("'%s' being saved"), outname);
	    }
	    G_system(command);
	    fprintf(stderr,"\n");
	    break;
	}

	while(1)
	{
	    *ans = 0;
	    fprintf(stderr, _("Do you wish to print this report? (y/n) [n] "));
	    if(!G_gets(ans)) continue;
	    G_strip (ans);
	    if(ans[0] != 'y' && ans[0] != 'Y') break;

ask132:
	    fprintf(stderr, _("Do you wish it printed in 80 or 132 columns?\n> "));
	    *ans = 0;
	    if(!G_gets(ans)) continue;
	    G_strip (ans);
	    if(sscanf(ans,"%d",&cols) != 1) goto ask132;
	    if(cols == 132) print_coin(key,132,1);
	    else if (cols != 80) goto ask132;
	    sprintf(command,"lpr %s",dumpname);
	    G_system(command);
	    break;
	}

	do
	{
	    fprintf(stderr, _("Do you wish to run this report with a "
                              "different unit of measure? (y/n) [y] "));
	    *ans = 0;
	}
	while(!G_gets(ans));
	G_strip (ans);
	if (*ans == 'n' || *ans == 'N') break;
    }

    return 0;
}
