#include <stdio.h>
char *item();

set_key (key, value)
    char *key, *value;
{
    if (key == NULL || *key == 0)
	delete_key (key);
    else
    {
	R_open_driver();
	select_current_window();
	R_pad_set_item (item(key),value);
	R_close_driver();
    }
}

append_key (key, value)
    char *key, *value;
{
    R_open_driver();
    select_current_window();
    R_pad_append_item (item(key),value);
    R_close_driver();
}

delete_key(key)
    char *key;
{
    R_open_driver();
    select_current_window();
    R_pad_delete_item (item(key));
    R_close_driver();
}

get_key (key, list, count)
    char *key;
    char ***list;
    int *count;
{
    R_open_driver();
    select_current_window();
    if(R_pad_get_item (item(key),list,count))
	*count = 0;
    R_close_driver();
}


static char *
item (key)
    char *key;
{
    static char name[50];

    sprintf (name, "d@%s", key);
    return name;
}

select_current_window()
{
    char name[100];
    *name = 0;
    D_get_cur_wind (name);
}
