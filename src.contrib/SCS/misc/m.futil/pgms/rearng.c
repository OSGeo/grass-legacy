/* @(#)main.c	1.0   11/10/88 
* Created by:  R.Glenn, SCS
*/
#include <stdio.h>
#include <ctype.h>

/*  arguement list :
*  none, interactive program
*/

main() 
{
	int i, j, k, bytes, old, new, end;
	int record, reccnt=0;
	int chng[25][4];
	char lf='\n', null='\0', *gets();
	char answer[10], buffer [2049], Obuffer[2049];
	FILE *input_file, *output_file;

#ifdef SYSV
	system("tput clear");
#else
	system("clear");
#endif

	printf("\n\tCreate new file by -\n\t\tmoving characters from column to column\n");

/* open input, ascii file */
	printf("\n\tEnter the name of the input file ?\n");
	printf("\tHit <return> to cancel request\n");
	printf("\t> ");
	gets(buffer);
	if (strlen(buffer) == 0) exit();
	if (access(buffer,4) != 0)
		{
		printf("\t ERROR - couldn't open input file <%s>\n",buffer);
	        exit ();
		}
	input_file = fopen (buffer,"r");


/* open output, ascii file */
	printf("\n\tEnter the name of the output file ?\n");
	printf("\tHit <return> to cancel request\n");
	printf("\t> ");
	gets(buffer);
	if (strlen(buffer) == 0)
	   {
	   fclose(input_file);
	   exit();
	   }
  	if ( access(buffer,2) != 0 && access(buffer,2) != -1 )
	 	{           
		printf("\t ERROR - couldn't open output file <%s>\n",buffer);
	        fclose(input_file);
	        exit ();
		}
	output_file = fopen (buffer,"w");

	printf("\n\tEnter the columns to be re-arranged in each file record.\n");

/* process columns to be re-arranged */
	for (i=0;i<=25;i++)
	   {
	   printf("\n\tFor change %d : move (number of characters) ? ",i+1);
	   gets(answer);
	   if (strlen(answer) == 0) break;
	   sscanf(answer,"%d",&chng[i][0]);
	   printf("\n\t                beginning at column ? ");
	   gets(answer);
	   if (strlen(answer) == 0) break;
	   sscanf(answer,"%d",&chng[i][1]);
	   chng[i][1]--;
	   printf("\t                          to column ? ");
	   gets(answer);
	   if (strlen(answer) == 0) break;
	   sscanf(answer,"%d",&chng[i][2]);
	   chng[i][2]--;
	   }

	if (i > 24)
	  { 
	  printf("\n\t*** WARNING *** Maximum number of changes has been reached.");
	  printf("\n\t                  Do you wish to QUIT ( y/[n] ) ? ");
	  gets(answer);
	  if (answer[0] == 'Y' || answer[0] == 'y')
	    {
            fclose(input_file);
            fclose(output_file);
            exit();
            }
          }

/* Check for no changes */
	if ( i == 0 )
	   {
	   fclose(input_file);
	   fclose(output_file);
	   exit();
	   }
       i--;

       printf("\n\tRe-arrange: started ");
/* Main process */
	    for (record=0;;++record)
	        {             
		for (j=0; j<=2047; j++) /* clear input buffer */
		  { buffer[j] = '\040'; }
                buffer[2048] = null;
		                      /* get input file records */
		if (!fgets (buffer, sizeof buffer, input_file)) goto AT_END;
		reccnt++;
/*  printf("\tfile input = %d\n\t%s\n",strlen(buffer),buffer);*/
		k = strlen(buffer) - 1;  /* remove line-feed */
		buffer[k] = '\040';
		end = 0;
		for (j=0; j<=2047; j++) /* clear output buffer */
		  { Obuffer[j] = '\040'; }
                Obuffer[2048] = null;
/*printf("length= %d,  %s\n",strlen(Obuffer),Obuffer);*/
		for (j=0; j<=i; j++)
		  {
		  bytes = chng[j][0];
		  old = chng[j][1];
		  new = chng[j][2];
			/* keep track of max length of new record */
		  if ((new + bytes) > end) end = new + bytes;
/*printf(" move %d bytes from col. %d to col. %d, length %d\n",bytes,old,new,end);*/
		  mvbyt (0,bytes, &buffer[old], &Obuffer[new]);
		  for (k=0; k<=end; k++)  /* get rid of any moved nulls */
  		    {
		    if (Obuffer[k] == null) Obuffer[k] = '\040';
		    }
                  }
		Obuffer[end] = lf;
		Obuffer[end+1] = null;
/*printf("\t    output = %d\n\t%s\n",strlen(Obuffer),Obuffer);*/
    		fputs(Obuffer,output_file); 
		if (reccnt > 50)
		   {
		   printf("."); fflush(stdout);
		   reccnt = 0;
		   }
                }

 /* all done, good-bye */
      AT_END :
	 printf(" completed\n");
         fclose(input_file);
         fclose(output_file);
	 exit(0);
}

mvbyt (typ, tcnt, addr1, addr2)
	int typ, tcnt;
	char *addr1, *addr2;
{
	int mcnt;
	for (mcnt=0; mcnt<tcnt; ++mcnt)  {
		*(addr2+mcnt) = *(addr1+mcnt);
		}
	 /* if want \0 at end typ must be 1 */
	if (typ) *(addr2+mcnt) = 000;  
}
