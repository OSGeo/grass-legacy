#include "gis.h"
#define LINE 80
#define TRUE 0
#define FALSE 1

infxQry(selectFile,raster,rastout,label)
  char *selectFile, *raster, *rastout, *label;
  {
    FILE *fpin, *fpout;
    int catcnt, hit, TMP;
    char buf[1024];
    char *tmpfile_out ;
    char *tmpfile_rules ;
    char *reduced_key;
    char *rindex();

    catcnt = 1;
    TMP = FALSE;
    tmpfile_out= G_tempfile() ;

           
/* Check for presence of selectfile first */
	if((fpin = fopen(selectFile,"r")) == NULL) {
            fprintf(stderr, "File read error on temporary file (sql).\n");
	    exit(-1);
           }

        sprintf(buf,"sqlplus -s %s @%s | awk '{ if (NF == 1) { print $1 } else { print $0 } }' > %s",
		G__getenv("DATABASE"),selectFile,tmpfile_out);
        system(buf);
	
	if((fpin = fopen(tmpfile_out,"r")) == NULL) {
            fprintf(stderr, "File read error on temporary file (sql output).\n");
	    exit(-1);
           }

    	tmpfile_rules= G_tempfile() ;
	if((fpout = fopen(tmpfile_rules,"w")) == NULL) {
            fprintf(stderr, "File write error on temprorary file (rules).\n");
	    exit(-1);
           }


        while (!feof(fpin) )
		if(fgets(buf,LINE,fpin)) 
		   if (strlen(buf) > 1) { 
                        G_squeeze(buf);
			if (label)
             		   fprintf(fpout,"%s=%d %s(%d)\n",buf, catcnt, label,catcnt);
			else
			   fprintf(fpout,"%s=%d\n",buf, catcnt);
		}	
		  else   { /* increment cat count */
		    			catcnt++;
		}

        fprintf(fpout,"end\n");
        fclose(fpin);
        fclose(fpout);


	if (!rastout) {
	    rastout="tmp.recl" ;
	    TMP = TRUE;
	}

	sprintf(buf,"r.reclass input=%s output=%s < %s\n",
		raster, rastout, tmpfile_rules);

	system(buf);
	   
	sprintf(buf,"d.rast %s\n",rastout);
	system(buf);

	unlink(tmpfile_out);
	unlink(tmpfile_rules);

	/* if user didn't specify raster output file - remove */
	if (TMP==TRUE) {
		G_remove("cell","tmp.recl") ;
		G_remove("cats","tmp.recl") ;
		G_remove("cellhd","tmp.recl") ;
		G_remove("hist","tmp.recl") ;
		G_remove("cell_misc","tmp.recl") ;
		G_remove("colr","tmp.recl") ;
	}
	return 0 ;
		
}

