static char background_color[50];

set_background_color (color) char *color;
{
    strcpy (background_color, color);
    set_key ("bg", color);
}

char *
get_background_color()
{
    char **list;
    int count;

    get_key ("bg", &list, &count);

    if (count)
    {
	strcpy (background_color, list[0]);
	R_pad_freelist (list, count);
    }
    else
	strcpy (background_color, "black");

    return background_color;
}
