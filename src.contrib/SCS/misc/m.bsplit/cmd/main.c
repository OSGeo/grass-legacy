/* @(#)main.c	1.1   3/11/91 GRASS4.0*/
/* @(#)main.c	1.0   1/1/89 */
/* m.bsplit 
*    Created by : R.L.Glenn , Soil Conservation Service, USDA
*    Purpose: Productivity tool
*             program is used to split large binary files, such
*             a cell files, into 500,000 byte or less size files.
*             Primarily to allow uucp movement of such files.
*
        fprintf(stderr,"\nbsplit [-s<size>] input_file [output_file_beginning letter]\n");
        fprintf(stderr,"             |                        |\n");
        fprintf(stderr,"defaults: 500000                     bx\n");
*  ------Rev 4.+ arguements --------------------------------------
*    Input arguements:
*             m.bsplit input=input file name 
*                    [size=size of split files]
*                    [prefix=output file beginning]
*
*  flags:
*          none
*
*/
#include <fcntl.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <ctype.h>
#include <errno.h>
#include <unistd.h>
#include <math.h>
#include "gis.h"

#define	WFLAG	(O_CREAT | O_TRUNC | O_WRONLY)
#define	MODE	0666

int 
main (int argc, char *argv[])

{
	int ier, size, n_read, n_write;
	int i, j, k, byte_cnt, arg_pos;
	int fdI, fdO;
	char *apntr, in_name[100], out_name[100], name_app1[2], name_app2[2], *buffer;
        struct Option *sizeopt, *inopt, *prefopt;

	buffer    = malloc(2000);

        G_gisinit (argv[0]);

          /* set up the options and flags for the command line parser */

       inopt = G_define_option();
       inopt->key              = "input";
       inopt->type             =  TYPE_STRING;
       inopt->required         =  YES;
       inopt->description      =  "Input file name";
									       
       sizeopt = G_define_option();
       sizeopt->key              = "size";
       sizeopt->type             =  TYPE_INTEGER;
       sizeopt->required         =  NO;
       sizeopt->description      =  "split up file size value ";

       prefopt = G_define_option();
       prefopt->key              = "prefix";
       prefopt->type             =  TYPE_STRING;
       prefopt->required         =  NO;
       prefopt->description      =  "Prefix for split file names ";

       if (G_parser (argc, argv))
              exit(-1);
												    
              /* start checking flags and options */
        if (sizeopt->answer != NULL)
	   {
	   sprintf(buffer,"%s",sizeopt->answer);
  	   ier = sscanf (buffer, "%d",&size) ;
	   if (ier != 1)
	      {
	      fprintf (stderr,"\n Error in size option \n");
	      exit(1);
	      }
           }
         else   /* No size specified, default 500000 */
	      size = 500000;

/*  input file name ? */
	sprintf(in_name,"%s",inopt->answer);
	if ((fdI = access(in_name,4)) != 0)
		{
		fprintf(stderr,"\nfile <%s> NOT found!\n",in_name);
		exit(1);
		}
	
	if((fdI = open(in_name,0)) < 0)
	    {
	    fprintf(stderr,"\n\terror opening file\n");
            exit(1);
	    }

         for (i=1; i<=26; i++)
	     {
	     for (j=1; j<=26; j++)
	       {
/*  output file name ?  */
	       out_name[0] = '\000';
	       apntr = out_name;
	       if ( prefopt->answer != NULL)
	            strcat (out_name, prefopt->answer);
	       else
	            strcat (out_name,"bx");
  	       sprintf(name_app1,"%c",toascii(i+96));
  	       sprintf(name_app2,"%c",toascii(j+96));
	       strcat (out_name, name_app1);
	       strcat (out_name, name_app2);

	       if((fdO = open(apntr,WFLAG,MODE)) < 0)
	           {
	           fprintf(stderr,"\n\terror opening file: ");
                   fprintf(stderr,"error status = %d\n",errno);
                   exit(1);
	           }
	       byte_cnt = 0;
/* read-write loop */
               while(1)
		 {
    	         if((n_read = read(fdI,buffer,1000)) < 0){ 
		         fprintf(stderr,"\n\terror reading file: ");
                         fprintf(stderr,"error status = %d\n",errno);
		         exit(1);
	         }
    	         buffer[n_read] = 0;
      	         if((n_write = write(fdO,buffer,n_read)) < 0){ 
		         fprintf(stderr,"\n\tnerror writting file: ");
                         fprintf(stderr,"error status = %d\n",errno);
		         exit(1);
	         }
		 if (n_read < 1000) goto AT_END;
		 byte_cnt = byte_cnt + n_read;
		 if ((byte_cnt + 1000) > size)
		    {
      	            if((k = close(fdO)) != 0){ 
		         fprintf(stderr,"\n\tnerror closing file: ");
                         fprintf(stderr,"error status = %d\n",errno);
		         exit(1);
	                 }
		    break;
		    }
                 }     /* end of while loop */
	       }      /* end of j loop */
	    }        /* end of i loop */

/* end of program */
AT_END:
	close(fdI);
        exit(0);
}
