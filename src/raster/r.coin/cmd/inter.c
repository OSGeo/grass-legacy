#include <stdlib.h>
#include "coin.h"

int interactive_version (void)
{
    int  cols;
    char key;
    char line[128],outname[128],command[256];
    char ans[80];

    setbuf(stderr,NULL);

    system("clear");
    fprintf(stderr,"GIS Coincidence Tabulation Facility\n\n");

    fprintf(stderr,
"This utility will allow you to compare the coincidence of two map layers\n\n");

    mapset1 = G_ask_cell_old("Enter Name of Map Layer 1",map1name);
    if(!mapset1)
	    exit (0);

    mapset2 = G_ask_cell_old("Enter Name of Map Layer 2",map2name);
    if(!mapset2)
	    exit (0);

    make_coin(1);
    check_report_size();


    while(1)
    {
	system("clear");
	fprintf(stderr,
"The report can be made in one of 8 units.\n");
	fprintf(stderr,
"Please choose a unit by entering one of the following letter codes:\n");
	fprintf(stderr,"     'c': cells\n");
	fprintf(stderr,"     'p': percent cover of region\n");
	fprintf(stderr,"     'x': percent of '%s' category (column)\n",map1name);
	fprintf(stderr,"     'y': percent of '%s' category (row)\n",map2name);
	fprintf(stderr,"     'a': acres\n");
	fprintf(stderr,"     'h': hectares\n");
	fprintf(stderr,"     'k': square kilometers\n");
	fprintf(stderr,"     'm': square miles\n");
	fprintf(stderr,"\n");
	fprintf(stderr,"     'Q': quit\n");
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
	system(command);

	while(1)
	{
	    fprintf(stderr,"Do you wish to save this report in a file? (y/n) [n] ");
	    *ans = 0;
	    if(!G_gets(ans))continue;
	    G_strip (ans);
	    if(ans[0] != 'y' && ans[0] != 'Y') break;

	    fprintf(stderr,"Enter the file name\n> ");
	    if (!G_gets(line)) continue;
	    if(sscanf(line,"%s",outname) != 1) continue;
	    if(outname[0] != '/'){
		sprintf(command,"cp %s %s/%s",dumpname,G_home(),outname);
		fprintf(stderr,"'%s' being saved in your home directory", outname);
	    }
	    else{
		sprintf(command,"cp %s %s",dumpname,outname);
		fprintf(stderr,"'%s' being saved",outname);
	    }
	    system(command);
	    fprintf(stderr,"\n");
	    break;
	}

	while(1)
	{
	    *ans = 0;
	    fprintf(stderr,"Do you wish to print this report? (y/n) [n] ");
	    if(!G_gets(ans)) continue;
	    G_strip (ans);
	    if(ans[0] != 'y' && ans[0] != 'Y') break;

ask132:
	    fprintf(stderr,"Do you wish it printed in 80 or 132 columns?\n> ");
	    *ans = 0;
	    if(!G_gets(ans)) continue;
	    G_strip (ans);
	    if(sscanf(ans,"%d",&cols) != 1) goto ask132;
	    if(cols == 132) print_coin(key,132,1);
	    else if (cols != 80) goto ask132;
	    sprintf(command,"lpr %s",dumpname);
	    system(command);
	    break;
	}

	do
	{
	    fprintf(stderr,
"Do you wish to run this report with a different unit of measure? (y/n) [y] ");
	    *ans = 0;
	}
	while(!G_gets(ans));
	G_strip (ans);
	if (*ans == 'n' || *ans == 'N') break;
    }

    return 0;
}
