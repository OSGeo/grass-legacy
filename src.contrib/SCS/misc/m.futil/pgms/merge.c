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
	int ier, cnt, i, k=0, l=0;
	int  record, reccnt=0, search, recd;
	int chng[3][2], f1_str, f1_end, f2_str, f2_end;
	char *ptr1, *ptr2, null='\0';
	char buffer1 [2049], buffer2 [2049], col1[14], col2[14];
        char answer[10], *gets(), Obuffer[2049];
	FILE *input_file1, *input_file2, *output_file;

#ifdef SYSV
	system("tput clear");
#else
	system("clear");
#endif

	printf("\n\tCreate a new file by -\n\t\tmerging two files with matching data in select columns\n\n");

/* open input 1, ascii file */
	printf("\n\tEnter the name of the first file ?\n");
	printf("\tHit <return> to cancel request\n");
	printf("\t> ");
	gets(buffer1);
	if (strlen(buffer1) == 0) exit();
	if (access(buffer1,4) != 0)
		{
		printf("\t ERROR - couldn't open input file <%s>\n",buffer1);
	        exit ();
		}
	input_file1 = fopen (buffer1,"r");

/* open input 2, ascii file */
	printf("\n\tEnter the name of the second file ?\n");
	printf("\tHit <return> to cancel request\n");
	printf("\t> ");
	gets(buffer1);
	if (strlen(buffer1) == 0) exit();
	if (access(buffer1,4) != 0)
		{
		printf("\t ERROR - couldn't open input file <%s>\n",buffer1);
		fclose(input_file1);
	        exit ();
		}
	input_file2 = fopen (buffer1,"r");

/* open output, ascii file */
	printf("\n\tEnter the name of the output file ?\n");
	printf("\tHit <return> to cancel request\n");
	printf("\t> ");
	gets(buffer1);
	if (strlen(buffer1) == 0)
	   {
	   fclose(input_file1);
	   fclose(input_file2);
	   exit();
	   }
  	if ( access(buffer1,2) != 0 && access(buffer1,2) != -1 )
	 	{           
		printf("\t ERROR - couldn't open output file <%s>\n",buffer1);
	        fclose(input_file1);
	        fclose(input_file2);
	        exit ();
		}
	output_file = fopen (buffer1,"w");

	printf("\n\tEnter the columns to be matched in each file record.\n");

/* process columns to be re-arranged */
	for (i=0;i<=1;i++)
	   {
	   printf("\n\tFor file %d  match :\n",i+1);
	   printf("\n\t                beginning at column ? ");
	   gets(answer);
	   if (strlen(answer) == 0) break;
	   sscanf(answer,"%d",&chng[i][0]);
	   chng[i][0]--;
	   printf("\t                          to column ? ");
	   gets(answer);
	   if (strlen(answer) == 0) break;
	   sscanf(answer,"%d",&chng[i][1]);
	   chng[i][1]--;
	   }

/* Check for no changes */
	if ( i != 2 )
	   {
	   fclose(input_file1);
	   fclose(input_file2);
	   fclose(output_file);
	   exit();
	   }

/* set the columns */
	    f1_str = chng[0][0];
	    f1_end = chng[0][1];
	    f2_str = chng[1][0];
	    f2_end = chng[1][1];
       printf("\n\tMerge: started ");

/* Main process */
	    for (record=0;;++record)
	        {                    /* select file1 records */
		if (!fgets (buffer1, sizeof buffer1, input_file1)) goto AT_END;
		reccnt++;
/*printf("\tfile1 input = %d\n\t%s\n",strlen(buffer1),buffer1);*/
		i = f1_end - f1_str + 1;
		Obuffer[0] = null;
		strcat(Obuffer,buffer1);
		mvbyt (1,i, &buffer1[f1_str], &col1[0]);
		ptr1 = col1;

	/* find match string in second file */
		search = 1;
		rewind (input_file2);
                while ( search )
		       {
	               for (recd=0;;++recd)
	                    {		 /* second file search */
		            if (!fgets (buffer2, sizeof buffer2, input_file2)) goto at_end;
/*printf("\tfile2 input = %d\n\t%s\n",strlen(buffer2),buffer2);*/
		            i = f2_end - f2_str + 1;
		            mvbyt (1, i, &buffer2[f2_str], &col2[0]);
		            ptr2 = col2;

	/* compare for match */
	       	            if (strcmp(ptr1,ptr2) == 0)
			      { /* match */
			      k = strlen(Obuffer) - 1;
			      l = strlen(buffer2);
			      i = l - f2_end;
                              mvbyt (1, i, &buffer2[f2_end + 1], &Obuffer[k]);
		  	      break;
			      }
			    }         /* end of file2 search */
at_end: 
		            search = 0;
		      }             /* end while loop */
/*		      printf("%s",Obuffer); */
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
         fclose(input_file1);
         fclose(input_file2);
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
