#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#define MAXNAME 80
#define ERR -1

struct  cat {
	char name[MAXNAME];
	};
struct cat *pcatlst;

char line[130],strng[100], c, type;
double X, Y;
FILE *catfp, *attfp;
int ncats,i, j;


main(argc,argv)
int argc;
char *argv[];
{

if (argc != 4)
	{
	fprintf(stderr,"Usage: att_cat num_cats dig_catsfile dig_attfile\n");
	exit(ERR);
	}
sscanf(argv[1],"%d",&ncats); 
ncats += 1;
if ((catfp = fopen(argv[2],"r")) == NULL) 
	{
	fprintf(stderr,"Error can not open dig_cats file %s\n",argv[2]);
	exit(ERR);
	}
if ((attfp = fopen(argv[3],"r")) == NULL) 
	{
	fprintf(stderr,"Error can not open dig_att file %s\n",argv[3]);
	exit(ERR);
	}
for (i=0; i<5; i++)
	while ((c = getc(catfp)) != '\n');

pcatlst = (struct cat *)malloc(ncats * MAXNAME);
        if (pcatlst == 0)
                {fprintf (stderr, "Out of Memory\n"); exit (ERR);}

while ((j = fscanf(catfp,"%s:",line)) != EOF && j != 0) 
	{
	j = 0;
	sscanf(line,"%d:%s",&i,strng);
	while (strng[j] != NULL) 
		{
		if (strng[j] == ':') strng[j] = '\t';
		j++;
		}
	strcpy(pcatlst[i].name,strng);
	}
/*DEBUG for (i=0;i<ncats;i++) printf("%d\t%s\n",i,pcatlst[i].name);*/

/*********** PROCESS the dig attribute file ****************/

while ((j = fscanf(attfp,"%c %lf %lf %d\n",&type,&X,&Y,&i)) != EOF && j != 0) {
	printf("%c	%lf	%lf	%s\n",type,X,Y,pcatlst[i].name);
	}
}
