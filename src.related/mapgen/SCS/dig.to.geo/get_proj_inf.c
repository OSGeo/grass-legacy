#include <stdio.h>
#include "gis.h"
#define PROJECTION_FILE "PROJ_INFO"
#define ERR -1


main(argc,argv)
int argc;
char *argv[];
{

char path[256], buffb[256], *str;
struct Key_Value *in_proj_keys;
int num, i;

/* get input projection parameters */
if (argc == 1)
	G__file_name (path, "", PROJECTION_FILE, G_mapset());
else
	G__file_name (path, "", PROJECTION_FILE, argv[1]);
if (access(path,0) != 0)
	{   
		fprintf(stderr,"Warning file %s not found \n",path );
		fprintf(stderr,"Run the program g.setproj\n");
		exit(ERR);
	}   
in_proj_keys = G_read_key_value_file(path,&num);
if (num != 0)
	{
	fprintf(stderr,"Warning Mapset PROJECTION_INF file not found \n");
	fprintf(stderr,"Using the mapset window file\n");
	exit(ERR);
	}

str = G_find_key_value("name",in_proj_keys);
if (strcmp(str,"Lat/Long") != 0)
	{
	for (i=1; i<=in_proj_keys->nitems-1; i++)
		{ 
		printf("+%s=%s ",
		in_proj_keys->key[i],in_proj_keys->value[i]);
		} 
	}
}
