#include <stdio.h>
#include <math.h>
#include <gis.h>
#include <string.h>
#include <sys/file.h>


#define MAX_OVERLAYS	6
#define CTG_HEADER      "LULC Binary File"

char buffer[255];


main (argc,argv)
	int argc;
	char *argv[];
{

	FILE  *f2;
	int i, num, map_code, value[MAX_OVERLAYS];
	int utm[2];
	char map_name[80], header[20];
	struct Cell_head window;

	if (argc < 2) {
		printf("USGAE: Mlulc.read <outfile>");
		exit(0);
	}
	if (access(argv[1],0) == 0) {
		printf("<%s> already exists\n",argv[1]);
		exit(0);
	}


	num=read_ctg_header(&window,map_name,&map_code);
	if (num == NULL) {
		puts("The Composite Theme Grid file contains no overlays");
		exit(1);
	}

	if ((f2 = fopen(argv[1],"wb")) == NULL) {
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
}

	
	
