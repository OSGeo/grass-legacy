#define EXTERN extern

#include "gis.h"
#include "edit.h"

write_target(fd) FILE *fd;
{

    int i;

    fprintf(fd,"%d\n", num_targets);

    for (i=0; i<num_targets; i++)
    {

        fprintf(fd,"%s %lf %lf %f\n",
          targets[i].id, targets[i].easting,
          targets[i].northing, targets[i].gc_fact);

    }

}
