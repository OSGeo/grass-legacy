/* @(#)DO_txt_file.c	1.0   04/90 */
/* created by: R.L.Glenn, USDA, SCS */

#include <stdio.h>
#include <string.h>
#include "gis.h"
#include "v_in_arc.h"

/**************************************************************/
int DO_txt_file (char *fp_nam1, char *fp_nam2)
/**************************************************************/
{
	FILE	*fp1,  *fp2;
	int	i, items=0;
	char    txtbuf[512];
	char    tmpbuf[512];
	char    inbuf[512];
	char    name[20];
	char    col[5];

	if ((fp1=fopen(fp_nam1,"r")) == NULL) return (-1);
                /* check first record, should contain DATAFILE ... */
	if (!fgets(txtbuf,512,fp1)) return (-1);
        sscanf(txtbuf,"%s%*s",name);
        if (strcmp(name,"DATAFILE") != 0)
           {        /* modified data file, copy to temp file */
           /*rewind(fp1);*/
           inbuf[0] = '\0';
           strcat(inbuf,"\tno column header available\n");
           goto RE_WRT;
           }
        else        /* looks like INFO output, process it */
           rewind(fp1);

		/* skip record 1, since it has DATAFILE NAME */
	if (!fgets(txtbuf,512,fp1)) return (-1);
		/* record 2 contains # of items */
	if (!fgets(txtbuf,512,fp1)) return (-1);
	sscanf(txtbuf, "%s%*s", tmpbuf);
	process_inp(tmpbuf);
	if (sscanf(tmpbuf,"%d",&items) != 1) return (-1);
		/* skip record 3, contains INFO TABLE DEFINITIONS */
	if (!fgets(txtbuf,512,fp1)) return (-1);
	inbuf[0] = '\0';
	strcat(inbuf,"  rec# ");
/*fprintf (stdout,"items= %d\n",items);*/
	i = 1;
	for (;;)
  	  {
      if (!fgets(txtbuf,512,fp1)) return (-1);
/*  fprintf (stdout,"txt= |%s|",txtbuf);*/
  	  if (sscanf(txtbuf,"%s%s%*s",col,name) != 2) return (-2);
/*fprintf (stdout,"i= %d, col= |%s|, name= |%s|, result= %d\n",i,col,name,strcmp(col,"1"));*/
          if (i > 1 && (strcmp(col,"1") == 0)) break;
  	  if (i <= items)
     	    {
     	    strcat(inbuf,name);
     	    strcat(inbuf," ");
     	    i++;
     	    }
  	  }
        strcat(inbuf,"\n");
		/* write the new temporary file */
RE_WRT:	if ( (fp2 = fopen(fp_nam2,"w")) == NULL)
   		G_fatal_error("File status error on DO_txt_file\n");
                /* put out the item names */      
  	fputs(inbuf,fp2);
        if (strlen(inbuf) > 7) 
		{
		fputs(txtbuf,fp2);  /*last record read */
		}
		/* now read the rest of the orig. txt file to temporary */
	for (;;)
  	  {
  	  if (fgets(txtbuf,512,fp1) == NULL) break;
  	  fputs(txtbuf,fp2);
  	  }
	fclose(fp1);
	fclose(fp2);
	return(0); 
}
