#define EXTERN extern

#include "gis.h"
#include "edit.h"

read_target(fd,num_wtable) FILE *fd;
{
    char buf[100];
    char readbuf[1024];
    int i;

    sprintf(buf,"Error reading target file -- incorrect format\n");

    for (i=0; i<num_wtable; i++)
    {

        if (fgets(readbuf,1024,fd) == NULL)
            G_fatal_error(buf);
        if (sscanf(readbuf,"%s %lf %lf %f",
          temp_targ[i].id, &temp_targ[i].easting,
          &temp_targ[i].northing, &temp_targ[i].gc_fact) != 4)
            G_fatal_error(buf);

        if ( (temp_targ[i].easting <= window.west) ||
            (temp_targ[i].easting >= window.east) ||
            (temp_targ[i].northing >= window.north) ||
            (temp_targ[i].northing <= window.south) )
        {
            fprintf(stderr, "Fatal error!!\n");
            fprintf(stderr, "Point coordinates outside current window for target point %s\n",
              temp_targ[i].id);
            sleep(2);
            exit(-2);
        }

    }
}
