#define EXTERN extern

#include "gis.h"
#include "edit.h"

add_weapon()
{

    int i;
    int newnum;
    char addopt[3][2];
    int count;
    char buf[100];
    char readbuf[1024];
    int num;
    int line;
    int ret_num;
    int num_wtable;
    int err_num;
    int charge_err;

    char *temp_name;
    FILE *temp_fd;
    char *mapset;
    char name[25];
    FILE *fd;
    char err_line[80];
    int mark[3];
    int cmp_weap();

    newnum = 0;

    for (i=0; i<3; i++)
        addopt[i][0] = 0;

    mark[0] = 3;
    mark[1] = 5;
    mark[2] = 2;

    mapset = G_find_file("noise","weapons","");
    if (!mapset)
        sprintf(err_line,"                             (Warning -- no files exist)");
    else
        sprintf(err_line," ");

    V_clear();

    V_line(2," Place an 'x' beside desired options");
    V_line(4,"  ADD WEAPON TYPES TO:");
    V_line(6," ___ current run");
    V_line(7,err_line);
    V_line(7," ___ location specific table");
    V_line(8," ___ permanent weapons table");

    for (i=0; i<3; i++)
    {
        addopt[i][0] = 0;
        V_ques(addopt[i],'s',i+6,2,1);
    }

    while(1)
    {

        V_intrpt_ok();
        if (!V_call())
            return;

        if (addopt[1][0])   /* add to location specific */
        {
            mapset = G_ask_old("Enter gun table file",
                name,"noise/weapons", "for storing weapon types");
            if (!mapset)
                addopt[1][0] = 0;
            else
            {
                fd = G_fopen_old("noise/weapons", name, mapset);
                if (!fd)
                {
                    addopt[1][0] = 0;
                    fprintf(stderr,"Error in opening file %s\n", name);
                    sleep(2);
                }
                else
                {
                    if (sscanf(fgets(readbuf,1024,fd),"%d",&num_wtable) != 1)
                    {
                        addopt[1][0] = 0;
                        fprintf(stderr,"Error with file %s\n", name);
                        fprintf(stderr,"Try another if you wish\n");
                        fclose(fd);
                        sleep(2);
                    }
                    else
                    {

                        weapons = (WEAPONS *)G_calloc(num_wtable,sizeof(WEAPONS));

                        read_weapon(fd, 0, num_wtable, mark[1]);
                        fclose (fd);

                        free(weapons);
                        break;
                    }
                }
            }
        }
        else
            break;
    }

    if (addopt[1][0] || addopt[2][0])   /* add to location or perm */
    {
        temp_name = G_tempfile();
        temp_fd = fopen(temp_name,"w");
        if (!temp_fd)
        {
            fprintf(stderr,"Error in opening temp file\n");
            exit(5);
        }
    }

    weapons = (WEAPONS *)G_calloc(1,sizeof(WEAPONS));

    sprintf(weapons[0].include,"x");

    if (addopt[0][0])   /* if add to current run */
    {
        if (num_weapons == 0)
        {
            temp1_fd = fopen(temp1_name,"w");
            if (!temp1_fd)
            {
                fprintf(stderr,"Error in assigning current weapons\n");
                exit(4);
            }
        }
        else
        {
            temp1_fd = fopen(temp1_name,"a");
            if (!temp1_fd)
            {
                fprintf(stderr,"Error in assigning current weapons\n");
                exit(4);
            }
        }
    }

    while (1)
    {
        weapons[0].code = 0;
        weapons[0].name[0] = 0;
        for (i=0; i<26; i++)
            weapons[0].data[i] = 0.;

        V_clear();

        V_line(2,"Weapon Code: ");
        V_line(3,"Weapon Name: ");

        V_ques(&weapons[0].code,'i',2,14,2);
        V_ques(weapons[0].name,'s',3,14,60);

        V_line(5,"Projectile weight: ");

        V_ques(&weapons[0].data[0],'f',5,20,7);

        V_line(7,"Propellant weight for charge zone 1 (in pounds): ");
        V_line(8,"Propellant weight for charge zone 2 (in pounds): ");
        V_line(9,"Propellant weight for charge zone 3 (in pounds): ");
        V_line(10,"Propellant weight for charge zone 4 (in pounds): ");
        V_line(11,"Propellant weight for charge zone 5 (in pounds): ");
        V_line(12,"Propellant weight for charge zone 6 (in pounds): ");
        V_line(13,"Propellant weight for charge zone 7 (in pounds): ");
        V_line(14,"Propellant weight for charge zone 8 (in pounds): ");
        V_line(15,"Propellant weight for charge zone 9 (in pounds): ");
        V_line(16,"Propellant weight for charge zone 10 (in pounds): ");

        V_ques(&weapons[0].data[1],'f',7,50,7);
        V_ques(&weapons[0].data[2],'f',8,50,7);
        V_ques(&weapons[0].data[3],'f',9,50,7);
        V_ques(&weapons[0].data[4],'f',10,50,7);
        V_ques(&weapons[0].data[5],'f',11,50,7);
        V_ques(&weapons[0].data[6],'f',12,50,7);
        V_ques(&weapons[0].data[7],'f',13,50,7);
        V_ques(&weapons[0].data[8],'f',14,50,7);
        V_ques(&weapons[0].data[9],'f',15,50,7);
        V_ques(&weapons[0].data[10],'f',16,50,7);

        V_line(18,"Parameter A: ");
        V_line(19,"Parameter B: ");

        V_ques(&weapons[0].data[11],'f',18,14,7);
        V_ques(&weapons[0].data[12],'f',19,14,7);

        while(1)
        {

            ret_num = 1;
            V_intrpt_msg("RETURN TO MAIN MENU");
            V_intrpt_ok();
            if ( (ret_num = V_call()) == 0)
                break;

            charge_err = 0;

            for (i=1; i<10; i++)
            {
                if ((weapons[0].data[i+1] > 0.) &&
                    (weapons[0].data[i+1] < weapons[0].data[i]))
                {
                    charge_err = 1;
                    fprintf(stderr,"\nError:  data for charge zone [%d] is",
                      i+1);
                    fprintf(stderr," less than data for charge zone [%d]\n",
                      i);
                }
            }

            if (charge_err)
            {
                fprintf(stderr,"\n - data for propellant weights must increase as charge zone numbers increase\n");
                printf("\n Hit return to continue...");
                G_gets(buf);
            }

            err_num = 0;

            if ( weapons[0].data[1] == 0)
            {
                err_num = 1;
                fprintf(stderr,
                  "\nMust provide non-zero data for charge zone 1\n");
            }

            G_strip(weapons[0].name);

            if (!weapons[0].name[0])
            {
                fprintf(stderr,"\nMust provide non-empty weapons name\n");
                err_num = 1;
            }

            if (weapons[0].code <= 0)
            {
                fprintf(stderr,"\nError:  weapons code must be an integer between 1 and 99 inclusive\n");
                err_num = 1;
            }

            if ( weap_codes[weapons[0].code] > 1 )
            {
                err_num = 1;
                fprintf(stderr,"\nWeapon code %d already used -- choose another\n", weapons[0].code);
                fprintf(stderr,"Unused codes are:\n");
                count = 0;
                for (i=1; i<MAX_WEAP; i++)
                {
                    if (weap_codes[i] == 1)
                    {
                        fprintf(stderr,"%d ",i);
                        count++;
                        if (count == 15)
                        {
                            count = 0;
                            fprintf(stderr,"\n");
                        }
                    }
                }

            } /* end if code is already set */

            if (err_num)
            {
                printf("\n Hit return to continue...");
                G_gets(buf);
                continue;
            }

            if (charge_err)
                continue;
            else
                break;
        } /* end while loop */

/*        strcat(weapons[0].name,"\n"); */

        if (ret_num == 0)
            break;

        V_clear();
        V_line(2,"Weapon Code:");
        V_line(3,"Weapon Name:");
        V_const(&weapons[0].code,'i',2,15,2);
        V_const(weapons[0].name,'s',3,15,60);

        V_line(5,"Decibel difference at   0 degrees: ");
        V_line(6,"Decibel difference at  30 degrees: ");
        V_line(7,"Decibel difference at  60 degrees: ");
        V_line(8,"Decibel difference at  90 degrees: ");
        V_line(9,"Decibel difference at 120 degrees: ");
        V_line(10,"Decibel difference at 150 degrees: ");
        V_line(12,"Average decibel difference: ");

        V_ques(&weapons[0].data[13],'f',5,37,7);
        V_ques(&weapons[0].data[14],'f',6,37,7);
        V_ques(&weapons[0].data[15],'f',7,37,7);
        V_ques(&weapons[0].data[16],'f',8,37,7);
        V_ques(&weapons[0].data[17],'f',9,37,7);
        V_ques(&weapons[0].data[18],'f',10,37,7);
        V_ques(&weapons[0].data[25],'f',12,37,7);

        V_intrpt_msg("CANCEL NEW WEAPON TYPE");
        V_intrpt_ok();
        if ( !V_call() )
            break;

        weapons[0].data[19] = 0;
        weapons[0].data[20] = weapons[0].data[18];
        weapons[0].data[21] = weapons[0].data[17];
        weapons[0].data[22] = weapons[0].data[16];
        weapons[0].data[23] = weapons[0].data[15];
        weapons[0].data[24] = weapons[0].data[14];

        newnum++;

        for (i=0; i<3; i++)
        {
            if (addopt[i][0])
                weap_codes[weapons[0].code] *= mark[i];
        }

        if (addopt[0][0])
            write_weapon(temp1_fd,1,1);

        if ( addopt[1][0] || addopt[2][0])
        {

            fprintf(temp_fd,"%d %f %f %f %f %f %f %f %f %f %f %f\n",
              weapons[0].code, weapons[0].data[0],
              weapons[0].data[1], weapons[0].data[2], weapons[0].data[3],
              weapons[0].data[4], weapons[0].data[5], weapons[0].data[6],
              weapons[0].data[7], weapons[0].data[8], weapons[0].data[9],
              weapons[0].data[10]);
    
            fprintf(temp_fd,"%s\n",weapons[0].name);
    
            fprintf(temp_fd,"%f %f %f %f %f %f %f %f %f %f %f %f %f %f %f\n",
              weapons[0].data[11], weapons[0].data[12],
              weapons[0].data[13], weapons[0].data[14], weapons[0].data[15],
              weapons[0].data[16], weapons[0].data[17], weapons[0].data[18],
              weapons[0].data[19], weapons[0].data[20], weapons[0].data[21],
              weapons[0].data[22], weapons[0].data[23], weapons[0].data[24],
              weapons[0].data[25]);
        }
    }

    free(weapons);

    if (addopt[0][0])
        fclose(temp1_fd);

    if ( addopt[1][0] || addopt[2][0] )
        fclose(temp_fd);

    if (newnum > 0)
    {
        if (addopt[0][0])
        {
            num_weapons += newnum;
            temp1_fd = fopen(temp1_name,"r");
            if (!temp1_fd)
            {
                fprintf(stderr,"Error in attempt to sort current weapons types\n");
                exit(7);
            }

            weapons = (WEAPONS *)G_calloc(num_weapons,sizeof(WEAPONS));
            read_weapon(temp1_fd,0,num_weapons,0);
            fclose(temp1_fd);

            qsort(weapons,num_weapons,sizeof(WEAPONS),cmp_weap);
            temp1_fd = fopen(temp1_name,"w");
            write_weapon(temp1_fd,num_weapons,1);
            fclose(temp1_fd);
            free(weapons);
        }

        for (i=1; i<3; i++)
        {
            if (addopt[i][0])
            {
                if (i == 1)
                    fd = G_fopen_old("noise/weapons",name,mapset);
                else
                    fd = fopen(perm_name,"r");

                if (!fd)
                {
                    if (i == 1)
                        fprintf(stderr,"Error in adding new weapon types to %s\n",name);
                    else
                        fprintf(stderr,"Error in adding new weapon types to %s\n",perm_name);
                    fprintf(stderr,"New weapon types will not be stored\n");
                    continue;
                }

                if (sscanf(fgets(readbuf,1024,fd),"%d",&num_wtable) != 1)
                {
                    if (i == 1)
                        fprintf(stderr,"Error in adding new weapon types to %s\n",name);
                    else
                        fprintf(stderr,"Error in adding new weapon types to %s\n",perm_name);
                    fprintf(stderr,"New weapon types will not be stored\n");
                    continue;
                }

                weapons = (WEAPONS *)G_calloc(num_wtable+newnum,
                   sizeof(WEAPONS));

                read_weapon(fd,0,num_wtable,0);
                fclose(fd);

                temp_fd = fopen(temp_name,"r");
                if (!temp_fd)
                {
                    if (i == 1)
                        fprintf(stderr,"Error in adding new weapon types to %s\n",name);
                    else
                        fprintf(stderr,"Error in adding new weapon types to %s\n",perm_name);
                    fprintf(stderr,"New weapon types will not be stored\n");
                    continue;
                }

                read_weapon(temp_fd,num_wtable,num_wtable+newnum,0);
                fclose(temp_fd);

                num_wtable += newnum;
                qsort(weapons,num_wtable,sizeof(WEAPONS),cmp_weap);

                if (i == 1)
                    fd = G_fopen_old("noise/weapons",name,mapset);
                else
                    fd = fopen(perm_name,"w");

                if (!fd)
                {
                    if (i == 1)
                        fprintf(stderr,"Error in adding new weapon types to %s\n",name);
                    else
                        fprintf(stderr,"Error in adding new weapon types to %s\n",perm_name);
                    fprintf(stderr,"New weapon types will not be stored\n");
                    continue;
                }

                fprintf(fd,"%d\n",num_wtable);
                write_weapon(fd,num_wtable,1);

                free(weapons);
                fclose(fd);

            }   /* close if structure on addopt */
        }    /* close for loop */
    }    /* close if newnum > 0 */
                
}

static
cmp_weap(a,b)

    WEAPONS *a, *b;
{
    if (a->code > b->code) return 1;
    if (a->code < b->code) return -1;
    return 0;
}
