/***************************************************************************
 * $Id$
 *
 * MODULE: 	v.in.atlas (of NRCS?)
 * AUTHOR(S):	R. L. Glenn ?, unknown GRASS author
 * PURPOSE: 	Single program that converts ATLAS ASCII files to 
 *              GRASS vector files.
 *              Creates a logfile in the working directory. 
 *
 * COPYRIGHT:  	(C) 2000 by the GRASS Development Team
 *
 *   	    	This program is free software under the GPL (>=v2)
 *   	    	Read the file COPYING that comes with GRASS for details.
 ****************************************************************************
 * $Log$
 * Revision 1.4  2000-11-28 13:07:29  andreas
 * added module descr.
 *
 * Revision 1.3  2000/11/06 19:42:22  andreas
 * changed sequence of call to G_parser and opening log file
 *
 */

/* vinatnrcs.c */

#include <stdio.h>
#include <string.h>
#include "gis.h"

FILE *log;

main(argc,argv)
int argc;
char *argv[];
{
	char atl_file[50], tmpi[50], tmp[20], out[50];
	int err;
	struct Option *input, *type, *globe;
	struct Flag *flag;
	struct GModule *module;

	G_gisinit(argv[0]);

	module = G_define_module();
	module->description = 
		"Import ATLAS GIS vector files into GRASS binary vector files. ";
	
	input = G_define_option();
	input->key = "input";
	input->description = "Atlas filename, no extension";
	input->type = TYPE_STRING;
	input->required = YES;

	type = G_define_option();
	type->key = "type";
	type->description = "Type: a=Area, l=Line, p=Point";
	type->type = TYPE_STRING;
	type->required = NO;
	type->answer = "A";
	type->options = "A,a,L,l,P,p";

	flag = G_define_flag();
	flag->key = 'd';
	flag->description = "Do areas or lines adjoin creating duplicate arcs";
	
	if (G_parser(argc,argv))
		exit(-1);
		
  	if((log=fopen("log","a"))==NULL)
  	{
		G_fatal_error("Can not open log file");
		exit(-1);
	}
          
	G_strcpy(atl_file,input->answer);
	G_strcat(atl_file,".bna");
	G_strcpy(tmpi,"tmpi23");
	G_strcpy(tmp,"tmp23");
	G_strcpy(out,input->answer);
	G_strcat(out,".out");

	if(strcmp(type->answer,"P")==0 || strcmp(type->answer,"p")==0)
	{
		printf("POINTS\n");
		err=Vimport1(atl_file,input->answer);
		if(err != 0) exit(-1);
		err=Vimport2(atl_file,input->answer);
		if(err != 0) exit(-1);
		exit(0);
	}
	if(strcmp(type->answer,"L")==0 || strcmp(type->answer,"l")==0)
	{
		printf("LINES\n");
		err=Vimport1(atl_file,input->answer);
		if(err != 0) exit(-1);
		G_strcpy(tmpi,atl_file);
	}
	if(strcmp(type->answer,"A")==0 || strcmp(type->answer,"a")==0)
	{
		printf("AREAS\n");
		err=isle(atl_file,tmpi);
		if(err != 0) exit(-1);
		err=Vimport1(tmpi,input->answer);
		if(err != 0) exit(-1);
	}
	if(flag->answer>0 && (strcmp(type->answer,"L")==0 || strcmp(type->answer,"A")==0 || strcmp(type->answer,"a")==0 || strcmp(type->answer,"l")==0))
	{
		err=grfix(tmpi,out,tmp);
		if(err != 0) exit(-1);
	}
	else
	{
		G_strcpy(out,tmpi);
	}
	if(strcmp(type->answer,"L")==0 || strcmp(type->answer,"A")==0 || strcmp(type->answer,"l")==0 || strcmp(type->answer,"a")==0)
	{
		err=Vimport2(out,input->answer);
		if(err == 0) 
		{
			G_system("rm *.out");
			G_system("rm tmp*");
		}
	}
	fclose(log);
	exit(0);
}
