#define EXTERN extern

#include "gis.h"
#include "edit.h"

choose_weapon(num_wtable,add_flag) int num_wtable; int add_flag;
{
    int i;
    int add_num;

    if (num_weapons == 0)
    {
        temp1_fd = fopen(temp1_name,"w");
        if (!temp1_fd)
        {
            fprintf(stderr,"Error in trying to open new weapons file\n");
            exit(4);
        }
    }
    else
    {
        temp1_fd = fopen(temp1_name,"a");
        if (!temp1_fd)
        {
            fprintf(stderr,"Error in trying to open weapons file\n");
            exit(4);
        }
    }

    add_num = ask_weapon(num_wtable,add_flag);
    num_weapons += add_num;

    write_weapon(temp1_fd,num_wtable,0);

    fclose(temp1_fd);
}
