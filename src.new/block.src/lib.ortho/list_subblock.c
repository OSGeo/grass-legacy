#include "dba_imagery.h"

I_list_subblock (block, subblock, Block_Image_Group_Ref, fd)
    char *block;
    char *subblock;
    struct Block_Image_Group_Ref *Block_Image_Group_Ref;
    FILE *fd;
{
    char buf[80];
    int i;
    int len, tot_len;
    int max;

    if (Block_Image_Group_Ref->nfiles <= 0)
    {
	fprintf (fd, "subblock [%s] of block [%s] is empty\n",
	    subblock, block);
	return;
    }
    max = 0;
    for (i=0; i < Block_Image_Group_Ref->nfiles; i++)
    {
	sprintf (buf, "%s in %s", Block_Image_Group_Ref->file[i].name, Block_Image_Group_Ref->file[i].mapset);
	len = strlen(buf)+4;
	if (len > max) max = len;
    }
    fprintf (fd, "subblock [%s] of block [%s] Block_Image_Group_References the following cellfiles\n", subblock, block);
    fprintf (fd, "-------------\n");
    tot_len = 0;
    for (i=0; i < Block_Image_Group_Ref->nfiles; i++)
    {
	sprintf (buf, "%s in %s", Block_Image_Group_Ref->file[i].name, Block_Image_Group_Ref->file[i].mapset);
	tot_len += max;
	if (tot_len > 78)
	{
	    fprintf (fd, "\n");
	    tot_len = max;
	}
	fprintf (fd, "%-*s", max, buf);
    }
    if (tot_len)
	fprintf (fd, "\n");
    fprintf (fd, "-------------\n");
}








