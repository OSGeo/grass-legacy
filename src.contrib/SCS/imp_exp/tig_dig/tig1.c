static char rcsid[] = "$Header$";
/*
$Log$
Revision 1.1  1999-12-29 15:12:12  markus
Initial revision

 * Revision 1.2  1991/06/26  03:49:26  paul
 * *** empty log message ***
 *
 * Revision 1.1  1991/06/25  03:31:35  paul
 * Initial revision
 *
 * Revision 1.1  1991/05/29  03:50:43  paul
 * Initial revision
 *
*/

#include <stdio.h>

char buf1[2000],outbuf[1000];
FILE *fp1,*fp2;

main(argc,argv)
int argc;
char *argv[];
{
	int x,y,z;
	char buf[20];
	char *ibufp, *obufp;

	if(argc!=2){
		fprintf (stdout,"Usage:  tig inputfile\n");
		exit(-1);
	}
	strcpy(buf,argv[1]);
	strcat(buf,"x");
	if((fp1=fopen(argv[1],"r")) == NULL){
		fprintf (stdout,"File not found: %s\n",argv[1]);
		exit(-1);
	}
	if((fp2=fopen(buf,"w")) == NULL){
		fprintf (stdout,"Can't create: %s\n",buf);
		exit(-1);
	}
	if(fgets(buf1,2000,fp1) != NULL){
			/* 1st get FIPS codes */
		obufp = outbuf;
			/* state code */
		for (x=1, ibufp = buf1 + 130; x<3; x++, obufp++, ibufp++) 
			*obufp = *ibufp;
		if(*(obufp - 1) == 32) {
			obufp = outbuf;
			for (x=1, ibufp = buf1 + 132; x<3; x++, obufp++, ibufp++) 
			*obufp = *ibufp;
		}
			/* county code */
		for (x=1, ibufp = buf1 + 134; x<4; x++, obufp++, ibufp++) 
			*obufp = *ibufp;
		if(*(obufp - 1) == 32) {
			obufp = outbuf + 2;
			for (x=1, ibufp = buf1 + 137; x<4; x++, obufp++, ibufp++) 
			*obufp = *ibufp;
		}
		obufp++;
		*obufp = '\0';
			/* write out FIPS with 5 spaces so will sort to top */
		fprintf(fp2, "     %s\n", outbuf);
	}
	rewind(fp1);
	z=1;	
	/* now do all the lines */
    while(fgets(buf1,2000,fp1) != NULL){
	if((z % 20) == 0){
		fprintf (stdout,"%d\r",z);
	}
	ibufp = buf1 + 5;
	obufp = outbuf;
	for (x=1;x<12;x++, obufp++, ibufp++) /*record # and side code */
		*obufp = *ibufp;
	for (x=1, ibufp = buf1 + 55; x<4; x++, obufp++, ibufp++) /* cfcc */
		*obufp = *ibufp;

	for (x=1, ibufp = buf1 + 190; x<39; x++, obufp++, ibufp++) /* geos*/
		*obufp = *ibufp;

	obufp++;
	*obufp = '\0';

	fprintf(fp2, "%s\n", outbuf);
	z++;	
	}
	fprintf (stdout,"\n");
}
