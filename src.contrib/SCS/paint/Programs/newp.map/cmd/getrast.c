#include <stdio.h>
#include "legendlabel.h"

#define EQ(x,y) (strcmp(x,y)==0)
#define KEY(x) EQ(key,x)

static char *help[]=
{
	"ramp 		value|label vertical|horizontal",
	"catnum		 # desc",
    ""
};

getrast()
{
    char t1[128], t2[128];
    char buf[1024];
    char *key, *data;


	nocats 	= 0;
	isramp 	= 0;
    while (input(3,buf,help))
    {
	if (!key_data (buf, &key, &data))
	    continue;



	if (KEY ("catnum"))
	{
	char *ptr;
	char *edata;
	int	 i;
	sscanf(data,"%s %s ", t1, t2) ; 
	getdata(data, &ptr, &edata);


	isramp = 0;

	cattmp[tcats].catnum =  atoi(t1);
	strcpy (cattmp[tcats].label, edata);
	tcats++;
	nocats++;

	continue;
	}

	if (KEY ("ramp"))
	{
	if (sscanf(data, "%s %s", t1, t2) != 2) 
	{
	error (key, data, "illegal reqrest");
	continue;
	}
	if ( (EQ(t1,"label")) || (EQ(t1,"value")) && ( EQ(t2, "horizontal") ||
		EQ(t2, "vertical"))) {
	strcpy (reqramp, t1);
	strcpy (ramporientation, t2);
	isramp = 1;
	}
	else
	error (key, data, "illegal reqrest");

	continue;

	}


	error (key, data, "illegal request");

	}

}


