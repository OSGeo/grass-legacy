/* %W% %G% */
#include <stdio.h>
#include "gis.h"
char SccsId[]="%Z% %I% %P% %G%";

char buf1[2000],outbuf[1000];
FILE *fp1,*fp2;
struct Categories cats;

main(argc,argv)
int argc;
char *argv[];
{
	int x,y,z, linecnt, catcnt;
	char buf[200];
	char name[50], *tmpname, *catptr, *dummyptr;
	struct Option *offs;

	/* Initialize gis library */
	G_gisinit(argv[0]);

        offs = G_define_option();
        offs->key                        = "input";
        offs->type                       = TYPE_STRING;
        offs->required           	= YES;
        offs->multiple           	= NO;
        offs->description                = "psu offset file name";

        if (G_parser (argc, argv))
               exit (-1);

	if((fp1=fopen(offs->answer,"r")) == NULL){
		fprintf (stdout,"File not found: %s\n",offs->answer);
		exit(-1);
	}

	G_ask_any("SUBJECT FILENAME", name, "SUBJ", "SUBJ", 1);
	while((dummyptr=fgets(buf1,2000,fp1)) != NULL){
		if (strlen(dummyptr) == 1) continue;
		z++;
	}
	rewind(fp1);
	G_init_cats((CELL)(z * 4), "Created by psusubj", &cats);
	G_set_cat( (CELL)0, "no data", &cats);

	z=1;
	while((dummyptr=fgets(buf1,2000,fp1)) != NULL){ 
		if (strlen(dummyptr) == 1) continue;
		G_strncpy(buf, buf1+6, 7);
		/*fprintf (stdout," z= %x  buf %s\n", buf1[1], buf);*/
		G_set_cat( (CELL)z, buf, &cats);
		z++;
	}
	G__write_cats("SUBJ", name, &cats);
			/* start at next subject  number */

	fp2 = G_fopen_append("SUBJ", name);
	

	catcnt = cats.num;
	linecnt = cats.num;
	for (x=1; x< catcnt;x++){
		catptr = G_get_cat((CELL)x, &cats);
		G_strcpy( buf, catptr);
		G_strcat( buf, "1");
		fprintf(fp2, "%d:%s\n", linecnt, buf);
		linecnt++;
		G_strcpy( buf, catptr);
		G_strcat( buf, "2");
		fprintf(fp2, "%d:%s\n", linecnt, buf);
		linecnt++;
		G_strcpy( buf, catptr);
		G_strcat( buf, "3");
		fprintf(fp2, "%d:%s\n", linecnt, buf);
		linecnt++;
	}
	fprintf (stdout,"\nDone\n");
}
