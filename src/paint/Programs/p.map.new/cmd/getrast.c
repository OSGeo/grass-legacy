#include <stdio.h>
#include <string.h>
#include "legendlabel.h"
#include "local_proto.h"

#define EQ(x,y) (strcmp(x,y)==0)
#define KEY(x) EQ(key,x)

static char *help[]=
{
    "ramp 	value|label vertical|horizontal",
    "catnum     # desc",
    "catlabel	#",
    ""
};

int getrast (void)
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
	   sscanf(data,"%s %s ", t1, t2) ; 
	   getdata(data, &ptr, &edata);

	   isramp = 0;

	   if(sscanf(t1, "%lf-%lf", &cattmp[tcats].dmin, &cattmp[tcats].dmax)!=2)
	   {
	      sscanf(t1, "%lf", &cattmp[tcats].dmin);
	      cattmp[tcats].dmax = cattmp[tcats].dmin;
           }
	   strcpy (cattmp[tcats].label, edata);
	   tcats++;
	   nocats++;

	continue;
	}


	if (KEY ("catlabel"))
	{
	   sscanf(data, "%s ", t1);
	   isramp = 0;
	   if(sscanf(t1, "%lf-%lf", &cattmp[tcats].dmin, &cattmp[tcats].dmax)!=2)
	   {
	      sscanf(t1, "%lf", &cattmp[tcats].dmin);
	      cattmp[tcats].dmax = cattmp[tcats].dmin;
           }
	   strcpy (cattmp[tcats].label, "label");
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

	   if ( (EQ(t1,"label")) || (EQ(t1,"value")) && ( EQ(t2, "horizontal") || EQ(t2, "vertical"))) {
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

     return 0;
}
