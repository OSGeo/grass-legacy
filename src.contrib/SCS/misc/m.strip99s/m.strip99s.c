/*
*	Created by:  Ken Sibley, Soil Conservation Service, USDA
*	Purpose   :  This program strips the no data, "99", areas 
*		     out and replaces them with spaces.  This will
*		     allow the data to be imported with r.in.miads.
*/
#include <stdio.h>
#include <string.h>
#include "gis.h"

char a[100];
FILE *infile, *outfile;

main(argc,argv)
int argc;
char **argv;
{

	int x,i;
	char inbuf[2];
	char *aptr;
	struct Option *inopt,
		      *outopt;

	inopt = G_define_option();
	inopt->key	   = "input";
	inopt->description = "input file";
	inopt->type	   = TYPE_STRING;
	inopt->required	   = YES;

	outopt = G_define_option();
	outopt->key	     = "output";
	outopt->description  = "output file";
	outopt->type	     = TYPE_STRING;
	outopt->required     = YES;

	if(G_parser(argc,argv))
	  exit(-1);

	if(strcmp(inopt->answer, outopt->answer) == 0)
	{
	  G_fatal_error("Input and output files cannot be the same.\n");
	}
	if((infile = fopen(inopt->answer, "r")) == NULL)
	{
	  printf("input file not available\n");
	  exit(1);
	}
	if((outfile = fopen(outopt->answer, "w")) == NULL)
	{
	  printf("output file not available\n");
	  exit(1);
	}

	while (fgets(a, 100, infile) != NULL)  /* gets one line */
	{
	  aptr = a;
/* This section checks for "99"s in the data columns */
	  for(x=0; x<71;x=x+2)
	  {
	    inbuf[0]=aptr[x];
	    inbuf[1]=aptr[x+1];
	    if (strncmp("99",inbuf,2) == 0)
	    {
	      inbuf[0]=' ';
	      inbuf[1]=' ';  
	    }
            fprintf(outfile,"%s",inbuf); 
	    inbuf[0]=' ';
	    inbuf[1]=' ';  
	  }
/* This section merely copies the last part of the line, the id, 
   back out to the output file
*/
	  x=72;
	  while (x<80)
	  {
	    inbuf[0]=aptr[x];
	    fprintf(outfile,"%c",inbuf[0]);
	    inbuf[0]=' ';
	    inbuf[1]=' ';  
	    x++;
	  }
	  fprintf(outfile,"\n");
	}
}
