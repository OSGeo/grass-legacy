#define EXTERN extern

#include "gis.h"
#include "edit.h"

write_weapon(fd,num_wtable,write_all)

FILE *fd;
int num_wtable;
int write_all;

{

    int i;

    for (i=0; i<num_wtable; i++)
    {

        if ( weapons[i].include[0] || write_all)
        {


        fprintf(fd,"%d %f %f %f %f %f %f %f %f %f %f %f\n",
          weapons[i].code, weapons[i].data[0],
          weapons[i].data[1], weapons[i].data[2], weapons[i].data[3],
          weapons[i].data[4], weapons[i].data[5], weapons[i].data[6],
          weapons[i].data[7], weapons[i].data[8], weapons[i].data[9],
          weapons[i].data[10]);

        fprintf(fd,"%s\n",weapons[i].name);

        fprintf(fd,"%f %f %f %f %f %f %f %f %f %f %f %f %f %f %f\n",
          weapons[i].data[11], weapons[i].data[12],
          weapons[i].data[13], weapons[i].data[14], weapons[i].data[15],
          weapons[i].data[16], weapons[i].data[17], weapons[i].data[18],
          weapons[i].data[19], weapons[i].data[20], weapons[i].data[21],
          weapons[i].data[22], weapons[i].data[23], weapons[i].data[24],
          weapons[i].data[25]);
        }

    }

}
