
#include <stdio.h>
#include <string.h>
#include "legendlabel.h"
#include "local_proto.h"

#define EQ(x,y) (strcmp(x,y)==0)
#define KEY(x) EQ(key,x)

static char *help[]=
{
	"sitename name desc",
    ""
};

int getsite (void)
{
    char t1[128], t2[128];
    char buf[1024];
    char *key, *data;


    nosites = 0;
    while (input(3,buf,help))
    {
	if (!key_data (buf, &key, &data))
	    continue;

	if (KEY ("sitename")) {
		char *ptr;
		char *edata;
	
		sscanf(data,"%s %s", t1, t2)  ;

		getdata(data, &ptr, &edata);

		strcpy (stmp[tsites].labelname, t1);
		strcpy (stmp[tsites].label, edata);
		tsites++;
		nosites++;

		continue;
	}
	error (key, data, "illegal request");
    }

    return 0;
}
