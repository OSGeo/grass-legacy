#include "gis.h"
#include "local_proto.h"

int ask_datum(char *datum, char *ellps, char *params)
{
    int answer;
    /* Sorry, without the next line g.setproj will crash with a bus error
     * or segmentation fault further down in main.c, if datum selection
     * is cancelled after this function is called. Haven't been able to
     * find the bug yet. PK */
    char dum[1000];
    
    answer = G_ask_datum_name(datum, ellps);
    if (answer > 0)
    {
        answer = G_ask_datum_params(datum, params);
 
        if(answer > 0)
            return 1;
        else
            return -1;
    }
    else
        return -1;


}


