#include <string.h>
#include <unistd.h>
#include "gis.h"
#include "local_proto.h"

#define MAX_OVERLAYS	6
#define CTG_HEADER      "LULC Binary File"

int main (int argc, char *argv[])
{

	struct GModule *module;
	char buffer[255];
	FILE  *f2;
	int num, map_code, value[MAX_OVERLAYS];
	int utm[2];
	char map_name[80], header[20];
	struct Cell_head window;
	struct Option *opt1;

	module = G_define_module();
	module->description =
		"Extracts Landuse/Landcover data in the ASCII "
		"Composite Theme Grid (CTG) data format distributed by the "
		"USGS in to a working file for m.lulc.USGS.";

        opt1 = G_define_option() ;
	opt1->key        = "output" ;
	opt1->type       = TYPE_STRING ;
	opt1->required   = YES ;
	opt1->description= "Name of the output file" ;
							 
        if (G_parser(argc, argv))
		exit(-1);

	if (access(opt1->answer,0) == 0) {
		fprintf (stdout,"<%s> already exists\n", opt1->answer);
		exit(0);
	}


	num=read_ctg_header(&window,map_name,&map_code);
	if (num == '\0') {
		puts("The Composite Theme Grid file contains no overlays");
		exit(1);
	}

	if ((f2 = fopen(opt1->answer,"wb")) == NULL) {
		puts("Unable to Open Output File");
		exit(0);
	}
	strcpy(header,CTG_HEADER);
	fwrite((char *) header,sizeof(char),16,f2);
	fwrite((char *) &num,sizeof(int),1,f2);
	fwrite((char *) &map_code,sizeof(int),1,f2);
	fwrite((char *) map_name,sizeof(char),80,f2);
	fwrite((char *) &window,sizeof(struct Cell_head),1,f2);
			

	while (!feof(stdin)) {
		fgets(buffer,100,stdin);
		sscanf(buffer+4,"%8d%8d%10d%10d%10d%10d%10d%10d",
			&utm[0], &utm[1],
			&value[0],&value[1], &value[2],&value[3],
			&value[4],&value[5]);
		fwrite((char *) utm,sizeof(int),2,f2);
		fwrite((char *) value,sizeof(int),6,f2);
	} 
	fclose(f2);

	exit(0);
}

	
	
