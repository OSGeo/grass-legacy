#include "/home/grass3/src/libes/gis.h"
main(argc,argv) 
    int argc;
    char *argv[];
    {
    char *name;
    char *mapset;
    CELL *dataVals;
    int numData;
    char msg[100];
    int i;
    G_gisinit (argv[0]);


    printf("%s\n", G_whoami());
    exit(1);
    }
usage (progName) char *progName;
    {
    fprintf (stderr,"usage: %s layer\n", progName);
    exit(1);
    }

PrintData(dataVals, num)
    CELL *dataVals;
    int num;
    {
    register int i = 0;
    for(i = 0; i < num; i++)
	{
	printf("Data[%d] = %ld\n", i, dataVals[i]);
	}
    }
