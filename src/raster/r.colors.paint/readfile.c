#include "gis.h"
#include "stdio.h"
#include "table.h"

readfile(devname,name)
char *devname;
char *name;

{
	FILE *colrtable;
	char buf[256];
	int numcolrs = 0;
	int i = 0;
	int good = 1;
	int bad = -1;
	char ctabfile[256];

	/** open the file $GISBASE/etc/Gcolortab/device/colortablename ******************/

	sprintf(ctabfile,"%s/etc/Gcolortab/%s/%s",G_gisbase(),devname,name);

	if ((colrtable = fopen(ctabfile,"r")) == (FILE *) NULL)
	{
		printf("Couldn't open %s for reading\n",ctabfile);
		printf("Available colortables for device %s:\n", devname) ;
		sprintf(ctabfile,"cd %s/etc/Gcolortab/%s; ls",G_gisbase(),devname);
		system(ctabfile) ;
		return bad;
	}

	/** read info in file *********************************************/

	fscanf(colrtable,"%s",buf);
	strcpy(clist.table_name,buf);
	fscanf(colrtable,"%s",buf);
	strcpy(clist.dev_name,buf);
	fscanf(colrtable,"%s",buf);
	strcpy(clist.type_map,buf);
	fscanf(colrtable,"%s",buf);
	clist.max_colors = atoi(buf);

	do
	{
		fscanf(colrtable,"%s",buf);
		if (strcmp(buf,"END") != 0)
		{
			clist.color_nums[numcolrs] = atoi(buf);
			numcolrs++;
		}
	} while(strcmp(buf,"END") != 0);

	return good;

}
