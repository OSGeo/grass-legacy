
#include <stdio.h>
#include "legendlabel.h"

#define EQ(x,y) (strcmp(x,y)==0)
#define KEY(x) EQ(key,x)

static char *help[]=
{
	"vectname # desc",
    ""
};

getvect()
{
    char t1[128], t2[128];
    char buf[1024];
    char *key, *data;


	novects = 0;
    while (input(3,buf,help))
    {
	if (!key_data (buf, &key, &data))
	    continue;



	if (KEY ("vectname"))
	{
	char *ptr;
	char *edata;
	sscanf(data,"%s %s", t1, t2) ; 
	printf (" in vectname \n");

	getdata(data, &ptr, &edata);



	printf (" 2. \n");
	strcpy (vtmp[tvects].labelname, t1);
	strcpy (vtmp[tvects].label, edata);
	tvects++;
	novects++;

	continue;
	}


	error (key, data, "illegal request");

	}

}


