#include "gis.h"
#define TRUE 0
#define FALSE 1

int readSQLout(tmpfile_out, outFlag, key, col, input, output)

	char *tmpfile_out;
	int outFlag;
	char *key, *col, *input, *output;
{

    FILE *fp, *fpin, *fpout;
    int i,  cat, TMP;
    char sysbuf[1024];          /* buffer to issue system cmd   */
    char buf[1024];             /* value in key column          */
    char label[1024];           /* current value in column      */
    char prev_label[1024] ;             /* running value for column     */
    int  colFlag, x;
    char *tmpfile_rules;
    char *reduced_col, *reduced_key;
    char *rindex();

	/* Read SQL output and generate reclass rules	*/

	/* Execute GRASS reclass			*/
	sprintf(sysbuf,
     "awk '{ if( NF == 2)  print $1,\"=\",int($2) }' < %s | r.reclass input=%s output=%s \n",
		tmpfile_out,input, output);
	system(sysbuf);
	   
	/* Display reclass map				*/
	sprintf(sysbuf,"d.rast %s\n",output);
	system(sysbuf);

	/* Remove temporay dot files			*/
/*	unlink(tmpfile_out); */

	/* if user didn't specify raster output file - cleanup */
		
return 0;
}

