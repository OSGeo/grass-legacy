/* Program that runs atl2dig2.c alone. */

/* vcont.c */

#include <stdio.h>
#include <string.h>
#include "gis.h"

FILE *log;

main(argc,argv)
int argc;
char *argv[];
{
	char out[50];
	int err;
	struct Option *input, *type;

	if((log=fopen("log","a"))==NULL)
		exit(-1);

	G_gisinit("Import from ATLAS");

	input = G_define_option();
	input->key = "input";
	input->description = "Atlas filename, no extension";
	input->type = TYPE_STRING;
	input->required = YES;

	type = G_define_option();
	type->key = "type";
	type->description = "Type: a=Area, l=Line";
	type->type = TYPE_STRING;
	type->required = NO;
	type->answer = "A";
	type->options = "A,a,L,l";

	if(G_parser(argc,argv))
	{	printf("ERROR - see log\n");
		fprintf(log,"%s %s: Command line error.\n\n",argv[0],input->answer);
		fprintf(log,"USAGE: v.cont  atlas_file_name(no ext)  type(A,L,P)\n");
		fprintf(log,"******************************\n");
		exit(-1);
	}
	G_strcpy(out,input->answer);
	G_strcat(out,".out");

	err=Vimport2(out,input->answer);
	fclose(log);
	exit(0);
}
