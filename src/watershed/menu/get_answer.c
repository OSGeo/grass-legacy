#include "gis.h"

char
get_answer()
{
    char buf[100];
    char inputchar;

    if (!G_gets(buf))
    {
        do
        {
            printf("Enter y or n ");
        } while (!G_gets(buf));
    }

    while(1)
    {
        if (strncmp(buf,"Y",1) == 0)
            strcpy(buf,"y");
        if (strncmp(buf,"N",1) == 0)
            strcpy(buf,"n");
        if (strncmp(buf,"y",1) == 0 || strncmp(buf,"n",1) == 0)
            break;
        do
        {
            printf("Enter y or n ");
        } while (!G_gets(buf));
    }

    inputchar = buf[0];
    return inputchar;

}
