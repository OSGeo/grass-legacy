#define EXTERN extern

#include "gis.h"
#include "edit.h"

read_weapon(fd,beg_num,read_num,used_num)
    FILE *fd;
    int beg_num;
    int read_num;
    int used_num;
{
    char buf[100];
    char readbuf[1024];
    int i;

    sprintf(buf,"Error reading weapons file -- incorrect format\n");

    for (i=beg_num; i<beg_num+read_num; i++)
    {

        if (fgets(readbuf,1024,fd) == NULL)
            G_fatal_error(buf);
        if (sscanf(readbuf,"%d %f %f %f %f %f %f %f %f %f %f %f",
             &weapons[i].code, &weapons[i].data[0],
          &weapons[i].data[1], &weapons[i].data[2], &weapons[i].data[3],
          &weapons[i].data[4], &weapons[i].data[5], &weapons[i].data[6],
          &weapons[i].data[7], &weapons[i].data[8], &weapons[i].data[9],
          &weapons[i].data[10]) != 12)
            G_fatal_error(buf);

        if (G_getl(readbuf,1024,fd) == NULL)
            G_fatal_error(buf);
        G_strncpy(weapons[i].name,readbuf,sizeof(weapons[i].name)-1);

        if (fgets(readbuf,1024,fd) == NULL)
            G_fatal_error(buf);
        if (sscanf(readbuf,"%f %f %f %f %f %f %f %f %f %f %f %f %f %f %f",
          &weapons[i].data[11], &weapons[i].data[12],
          &weapons[i].data[13], &weapons[i].data[14], &weapons[i].data[15],
          &weapons[i].data[16], &weapons[i].data[17], &weapons[i].data[18],
          &weapons[i].data[19], &weapons[i].data[20], &weapons[i].data[21],
          &weapons[i].data[22], &weapons[i].data[23], &weapons[i].data[24],
          &weapons[i].data[25]) != 15)
            G_fatal_error(buf);

/* used_num = 0 means do not mark the weapon codes for the weapon types
    currently being read in.  otherwise, used_num should be set to the
    appropriate number for current, permanent, or local marking.  if
    used_num is less than zero, this means to unmark the weapon types
    currently being read in. */

        weapons[i].include[0] = 0;

        if (used_num)
        {
            if (used_num < 0)
            {
                if ( (weap_codes[weapons[i].code]/used_num)*used_num ==
                   weap_codes[weapons[i].code] )
                {
                    weap_codes[weapons[i].code] /= used_num;
                }
            }
            else
            {
                if ( (weap_codes[weapons[i].code]/used_num)*used_num !=
                   weap_codes[weapons[i].code])
                    weap_codes[weapons[i].code] *= used_num;
            }
        }

    }

}
