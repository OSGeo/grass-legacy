#include <grass/gis.h>

int G_write_key_value_file (
    const char *file,
    const struct Key_Value *kv,
    int *stat)
{
    FILE *fd;

    *stat = 0;
    fd = fopen(file, "w");
    if (fd == NULL)
	*stat = -3;
    else if(G_fwrite_key_value(fd, kv) != 0 || fclose(fd) == EOF)
	*stat = -4;
    return (*stat != 0);
}

struct Key_Value *G_read_key_value_file(const char *file, int *stat)
{
    FILE *fd;
    struct Key_Value *kv;

    *stat = 0;
    fd = fopen (file, "r");
    if (fd == NULL)
    {
	*stat = -1;
	return NULL;
    }
    kv = G_fread_key_value (fd);
    fclose (fd);
    if (kv == NULL)
	*stat = -2;
    return kv;
}
