#include "viz.h"


g3read_header (Headfax)
    file_info *Headfax;
{
    int	intsize, floatsize;
    FILE *fp;
    char buf[80];
    int len;
    
    fp = Headfax->datainfp;

    /*read in header information and store in File_info struct */
    fseek (fp, 0L, 0);	

    len = strlen (GRID_ID);
    if (!fread (buf, 1, len, fp)) return (-1);
    buf[len] = 0;
    if (strncmp (GRID_ID, buf, len))
    {
        if (!strncmp ("grid003.01", buf, len))
            return (g3read_header_old (Headfax, fp));

        fprintf(stderr, "Error: header mismatch '%s' - '%s'\n", GRID_ID, buf);
        return (-1);
    }
    intsize = sizeof (int);
    floatsize = sizeof (float);

    if (!fread  ((int *)&Headfax->xdim, intsize, 1, fp))
	return (-1);
    if (!fread ((int *)&Headfax->ydim, intsize, 1, fp))
	return (-1);
    if (!fread ((int *)&Headfax->zdim, intsize, 1, fp))
	return (-1);
    if (!fread ((float *)&Headfax->north, floatsize, 1, fp))
	return (-1);
    if (!fread ((float *)&Headfax->south, floatsize, 1, fp))
	return (-1);
    if (!fread ((float *)&Headfax->east, floatsize, 1, fp))
	return (-1);
    if (!fread ((float *)&Headfax->west, floatsize, 1, fp))
	return (-1);
    if (!fread ((float *)&Headfax->top, floatsize, 1, fp))
	return (-1);
    if (!fread ((float *)&Headfax->bottom, floatsize, 1, fp))
	return (-1);
    if (!fread ((float *)&Headfax->ns_res, floatsize, 1, fp))
	return (-1);
    if (!fread ((float *)&Headfax->ew_res, floatsize, 1, fp))
	return (-1);
    if (!fread ((float *)&Headfax->tb_res, floatsize, 1, fp))
	return (-1);
    if (!fread ((int *)&Headfax->zone, intsize, 1, fp))
	return (-1);
    if (!fread ((int *)&Headfax->proj, intsize, 1, fp))
	return (-1);
    if (!fread ((int *)&Headfax->type, intsize, 1, fp))
	return (-1);
    if (!fread ((float *)&Headfax->min, floatsize, 1, fp))
	return (-1);
    if (!fread ((float *)&Headfax->max, floatsize, 1, fp))
	return (-1);
    Headfax->headsize = ftell (Headfax->datainfp);
    return (1);
}
g3read_header_old (Headfax, fp)
    file_info *Headfax;
    FILE *fp;
{
    int	intsize, floatsize;
    float tmp;


    intsize = sizeof (int);
    floatsize = sizeof (float);
   fprintf (stderr, "in read old\n"); 
    if (!fread  ((int *)&Headfax->xdim, intsize, 1, fp))
        return (-1);
    if (!fread ((int *)&Headfax->ydim, intsize, 1, fp))
        return (-1);
    if (!fread ((int *)&Headfax->zdim, intsize, 1, fp))
        return (-1);
    if (!fread ((float *)&tmp, floatsize, 1, fp))
        return (-1);
    if (!fread ((float *)&tmp, floatsize, 1, fp))
        return (-1);
    if (!fread ((float *)&tmp, floatsize, 1, fp))
        return (-1);
    if (!fread ((int *)&Headfax->type, intsize, 1, fp))
        return (-1);
    if (!fread ((float *)&Headfax->min, floatsize, 1, fp))
        return (-1);
    if (!fread ((float *)&Headfax->max, floatsize, 1, fp))
        return (-1);
    Headfax->headsize = ftell (fp);
    get_region_info (Headfax);
    
    return (1);
}

float
get_value (msg)
    char *msg;
{
    float tmp;
   
    fprintf (stderr, msg);
    scanf ("%f", &tmp);
    return (tmp);
}
/********************************** get_region_info ***************************/
get_region_info (Headfax)
    file_info *Headfax;
{
    fprintf (stderr, 
        "Old version of grid3 file does not include region information.\n");
    fprintf (stderr, 
	"Please enter the following values.\n");	
    
    Headfax->north
     = get_value ("North edge of region:  ");
    Headfax->south
     = get_value ("South edge of region:  ");
    Headfax->east
     = get_value ("East edge of region:  ");
    Headfax->west
     = get_value ("West edge of region:  ");
    Headfax->top
     = get_value ("Top edge of region:  ");
    Headfax->bottom
     = get_value ("Bottom edge of region:  ");
    Headfax->ns_res
     = get_value ("North/South resolution:");
    Headfax->ew_res
     = get_value ("East/West resolution:");
    Headfax->tb_res
     = get_value ("Top/Bottom resolution:");
    Headfax->zone
     = (int)get_value("Zone:");
    Headfax->proj
     = (int)get_value("Projection type:");
}

/********************************** write_header ******************************/
/********************************** write_header ******************************/
/********************************** write_header ******************************/

g3write_header (Headfax)
    file_info *Headfax;
{
    /* DEBUG */
    FILE *fp;
    fp = Headfax->dataoutfp;

    /* print the header code on first line of file */
    if  (!fwrite (GRID_ID, 1, strlen (GRID_ID), fp))
	return (-1);
    /*DEBUG*/
    fprintf (stderr, "xdim %d\n", Headfax->xdim);
    fprintf (stderr, "ydim %d\n", Headfax->ydim);
    fprintf (stderr, "zdim %d\n", Headfax->zdim);
    fprintf (stderr, "north %f\n", Headfax->north);
    fprintf (stderr, "south %f\n", Headfax->south);
    fprintf (stderr, "east %f\n", Headfax->east);
    fprintf (stderr, "west %f\n", Headfax->west);
    fprintf (stderr, "top %f\n", Headfax->top);
    fprintf (stderr, "bottom %f\n", Headfax->bottom);
    fprintf (stderr, "ns %f\n", Headfax->ns_res);
    fprintf (stderr, "ew %f\n", Headfax->ew_res);
    fprintf (stderr, "tb %f\n", Headfax->tb_res);
    fprintf (stderr, "zone %d\n", Headfax->zone);
    fprintf (stderr, "proj %d\n", Headfax->proj);
    fprintf (stderr, "type %d\n", Headfax->type);
    fprintf (stderr, "min %f\n", Headfax->min);
    fprintf (stderr, "max %f\n", Headfax->max);
    /*END DEBUG */


    /* print the dimensions of the data on line 2 */
    fwrite (&Headfax->xdim, sizeof (Headfax->xdim), 1, fp);
    fwrite (&Headfax->ydim, sizeof (Headfax->ydim), 1, fp);
    fwrite (&Headfax->zdim, sizeof (Headfax->zdim), 1, fp);

    /* print the region info  of the data on line 3 */
    fwrite (&Headfax->north, sizeof (Headfax->north), 1, fp);
    fwrite (&Headfax->south, sizeof (Headfax->south), 1, fp);
    fwrite (&Headfax->east, sizeof (Headfax->east), 1, fp);
    fwrite (&Headfax->west, sizeof (Headfax->west), 1, fp);
    fwrite (&Headfax->top, sizeof (Headfax->top), 1, fp);
    fwrite (&Headfax->bottom, sizeof (Headfax->bottom), 1, fp);
    fwrite (&Headfax->ns_res, sizeof (Headfax->ns_res), 1, fp);
    fwrite (&Headfax->ew_res, sizeof (Headfax->ew_res), 1, fp);
    fwrite (&Headfax->tb_res, sizeof (Headfax->tb_res), 1, fp);
    fwrite (&Headfax->zone, sizeof (Headfax->zone), 1, fp);
    fwrite (&Headfax->proj, sizeof (Headfax->proj), 1, fp);


    /* print out code for data type 1=short 2=int 3=float */
    fwrite (&Headfax->type, sizeof (Headfax->type), 1, fp);

    /* print out code for min and max values */
    fwrite (&Headfax->min, sizeof (Headfax->min), 1, fp);
    fwrite (&Headfax->max, sizeof (Headfax->max), 1, fp);

    return (1);
}

print_head_info (head)
    file_info *head;
{
    fprintf (stderr, "xdim = %d ydim = %d zdim = %d\n",
	head->xdim, head->ydim, head->zdim);
    fprintf (stderr, " n = %f s = %f e = %f w = %f\n",
	head->north, head->south, head->east, head->west);
    fprintf (stderr, "t = %f b = %f\n", head->top, head->bottom);
    fprintf (stderr, "ns_res = %f ew_res = %f tb_res = %f\n", 
	head->ns_res, head->ew_res, head->tb_res);
}
