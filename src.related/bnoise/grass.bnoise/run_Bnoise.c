#define EXTERN extern

#include "gis.h"
#include "edit.h"

run_Bnoise()
{

    int i,j;
    char answer;
    char get_answer();
    char tmp_path[1024];

    FILE *tape7_fd;
    FILE *Bnoise_in_fd;
    FILE *tape1_fd;
    char tape7_name[1024];
    char Bnoise_in_name[1024];
    char tape1_name[1024];

    char new_name[24];
    int new_fd;

    char command[1024];

    char include[6][2];
    float numdays;
    float res;
    float file_res;
    int filerows, filecols;
    float gc_fact;
    float inv[3];
    double x_num, y_num;

    char buf[1024];

    float n,s,e,w;
    float value;
    char label[10];
    CELL *grid;
    CELL *ptr;
    int nrows, ncols;

    FILE *fd,*popen();


    sprintf(tmp_path,"%s/%s/%s/.tmp", G_gisdbase(), G_location(), G_mapset());

/* remove all files in case Bnoise has been run previously -- the fortran
   code of Bnoise is unfriendly towards pre-existing "new" files.  however,
   the weapons file should be saved (rewritten) as the user may try another
   run (with changes) after return to the main menu.  */

    sprintf(command,"cd %s; rm *",tmp_path);
    system(command);

    temp1_fd = fopen(temp1_name,"w");
    write_weapon(temp1_fd,num_weapons,1);
    fclose(temp1_fd);

    if (!G_ask_cell_new("resulting map layer", new_name))
    {
        finish();
        return;
    }

/* need this file which is a produce of tabgen -- fix the system to
   regenerate this whenever perm table is updated, and find a perm
   place to keep the file  */

    sprintf(command,"cp %s/etc/TAPE20.DAT %s",G_gisbase(),tmp_path);
    system(command);

    sprintf(tape7_name,"%s/TAPE7.DAT", tmp_path);

    tape7_fd = fopen(tape7_name,"w");
    if (!tape7_fd)
    {
        G_fatal_error("cannot open TAPE7 input file for Bnoise");
        exit(-2);
    }

    write7(tape7_fd);
    fclose(tape7_fd);

    if (fd = popen("cd; pwd", "r"))
    {
        if (fscanf(fd,"%s",buf) == 1)
        {
            strcat(buf,"/OUTFILE");
            sprintf(command,"cd %s; echo %s > OUTFILE", tmp_path, buf);
            system(command);
        }
    }
    else
    {
        fprintf(stderr,"Problems accessing home directory -- cannot run bnoise\n");
        exit(7);
    }

    sprintf(Bnoise_in_name,"%s/BNOISEIN.DAT", tmp_path);

    Bnoise_in_fd = fopen(Bnoise_in_name,"w");
    if (!Bnoise_in_fd)
    {
        G_fatal_error("cannot open BNOISEIN input file for Bnoise");
        exit(-2);
    }

/*  PROGRAMMERS:  Note here that an assumption is made that
    the given GRASS window and user supplied coordinates are in
    meters.  Once other coordinate systems are implemented which
    may be in feet (state plane for instance), the code here should
    be changed to reflect the correct information to Bnoise.  Plans
    are that the information about feet vs. meters will be implied
    in the projection number information.  If this can be counted
    on to give accurate information for all cases, the programmer
    should code that in here to give the information to Bnoise
    without prompting the user for another piece of input...
*/

    fprintf(Bnoise_in_fd,"METERS\n");
    fprintf(Bnoise_in_fd,"MAP\n");

    while (1)
    {
        V_clear();

        V_line(2,"PLACE AN x BESIDE ALL DESIRED PRINTOUTS");
        V_line(4," ___  data base information");
        V_line(5," ___  target vs firing point table");
        V_line(6," ___  target vs gun type table");
        V_line(7," ___  gun type vs target table");
        V_line(8," ___  gun type vs firing point table");
        V_line(9," ___  'extraneous data' message");

        for (i=0; i<6; i++)
            V_ques(include[i], 's', i+4, 2, 1);

        V_line(12," number of days represented by input data:");

        V_ques(&numdays,'f',12,44,4);

        V_call();

        if (numdays > 0.)
            break;
        else
        {
            fprintf(stderr,"Must provide non-zero number of days.\n");
            printf("\nHit return to continue...");
            G_gets(buf);
            continue;
        }
    }

    for (i=0; i<6; i++)
    {
        if (include[i][0])
            fprintf(Bnoise_in_fd,"1");
        else
            fprintf(Bnoise_in_fd,"0");
    }
    fprintf(Bnoise_in_fd,"\n");

    fprintf(Bnoise_in_fd,"%10.0f\n",numdays);

    fprintf(Bnoise_in_fd," 1\n");

    res = window.ns_res;
    fprintf(Bnoise_in_fd,"%10.0f\n",res);

    fprintf(Bnoise_in_fd,"BOUNDS\n");

/*    x_num = (float)((int)window.west%100000);
    y_num = (float)((int)window.south%100000);
    fprintf(Bnoise_in_fd,"%10.0f%10.0f\n", x_num, y_num); */
    fprintf(Bnoise_in_fd,"%10.0f%10.0f\n", window.west, window.south);

/*    x_num = (float)((int)window.east%100000);
    y_num = (float)((int)window.north%100000);
    fprintf(Bnoise_in_fd,"%10.0f%10.0f\n", x_num, y_num); */
    fprintf(Bnoise_in_fd,"%10.0f%10.0f\n", window.east, window.north);

    fprintf(Bnoise_in_fd,"FORM-A\n");

    for (i=0; i<3; i++)
        include[i][0] = 0;

    V_clear();

    V_line(2,"PLACE AN x BESIDE THE DESIRED CHARGE AVERAGING TECHNIQUE");
    V_line(4," ___  maximum charge zone");
    V_line(5," ___  average of charge zones");
    V_line(6," ___  actual TNT charge equivalents");

    for (i=0; i<3; i++)
        V_ques(include[i], 's', i+4, 2, 1);

    gc_fact = 1.5;
    V_line(9,"Enter desired ground correction factor [1.5]");
    V_ques(&gc_fact,'f',9,47,5);

    while(1)
    {
        V_call();

        j = 0;
        for (i=0; i<3; i++)
        {
            if (include[i][0])
                j++;
        }
        if (j == 1)
            break;
        else
        {
            fprintf(stderr,"Must choose one and only one technique\n");

            printf("\n Hit return to continue...");
            G_gets(buf);
        }
    }

    if (include[0][0])
        fprintf(Bnoise_in_fd," MAX      %10.2f\n", gc_fact);
    else if (include[1][0])
        fprintf(Bnoise_in_fd,"IAVE      %10.2f\n", gc_fact);
    else if (include[2][0])
        fprintf(Bnoise_in_fd,"CAVE      %10.2f\n", gc_fact);

    fprintf(Bnoise_in_fd,"%10.0f\n",numdays);

    fprintf(Bnoise_in_fd,"PUDDLE GRID\n");

    V_clear();

    for (i=0; i<3; i++)
        include[i][0] = 0;

    V_line(2,"ENTER DESIRED PUDDLE GRID PARAMETERS");
    V_line(4," Temperature inversion factor at ground surface:");
    V_line(5," Temperature inversion factor (1 to 500 m):");
    V_line(6," Temperature inversion factor (1 to 3000 m):");

    for (i=0; i<3; i++)
    {
        inv[i] = 0.;
        V_ques(&inv[i],'f',i+4,50,6);
    }

    V_line(10,"PLACE AN x BESIDE DESIRED NOISE INCLUSION");
    V_line(12," ___  day noise only (0700 to 2200 hours)");
    V_line(13," ___  night noise only (2200 to 0700 hours)");
    V_line(14," ___  both day and night noise");

    for (i=0; i<3; i++)
        V_ques(include[i], 's', i+12, 2, 1);

    while(1)
    {
        V_call();

        j = 0;
        for (i=0; i<3; i++)
        {
            if (include[i][0])
                j++;
        }
        if (j == 1)
            break;
        else
        {
            fprintf(stderr,"Must choose one and only one noise inclusion option\n");

            printf("\n Hit return to continue...");
            G_gets(buf);
        }
    }

    fprintf(Bnoise_in_fd,"%10.1f%10.1f%10.1f%10.0f",
      inv[0], inv[1], inv[2], res);

    if (include[0][0])
        fprintf(Bnoise_in_fd,"DAY\n");
    else if (include[1][0])
        fprintf(Bnoise_in_fd,"NIGHT\n");
    else if (include[2][0])
        fprintf(Bnoise_in_fd,"BOTH\n");

    fprintf(Bnoise_in_fd,"STOP\n");

    fclose(Bnoise_in_fd);

    sprintf(command,"cd %s; %s/etc/bnoise.exe",
      tmp_path, G_gisbase());

    sprintf(tape1_name,"%s/TAPE1.DAT", tmp_path);

    system(command);

    if (access(tape1_name,"r"))
    {
        fprintf(stderr,"Bnoise program did not complete correctly\n");
        fprintf(stderr," Please check input values and information given in bnoise output and try again\n");

        printf("\n Hit return to continue...");
        G_gets(buf);
        return;
    }

    tape1_fd = fopen(tape1_name,"r");
    if (!tape1_fd)
    {
        G_fatal_error("cannot open TAPE1.DAT output file from Bnoise");
        exit(-2);
    }

    G_get_window(&window);
    nrows = G_window_rows();
    ncols = G_window_cols();

    if ((new_fd = G_open_cell_new(new_name)) < 0)
    {
        fprintf(stderr,"Error creating new cell file");

        printf("\n Hit return to continue...");
        G_gets(buf);

        window.north -= (window.ns_res/2.);
        window.south += (window.ns_res/2.);
        window.east -= (window.ew_res/2.);
        window.west += (window.ew_res/2.);
        return;
    }

    G_getl(buf,100,tape1_fd);
    sscanf(buf,"%f %f %f %f %f", &file_res, &w, &s, &e, &n);

    while (G_getl(buf,100,tape1_fd))
    {
        sscanf(buf,"%s %d %d", label, &filerows, &filecols);
        label[4] = 0;
        if (strncmp(label,"GRID",4) == 0)
            break;
    }

    filerows++;
    filecols++;

    if ((nrows != filerows) || (ncols != filecols))
    {
        fprintf(stderr,"Warning -- number of rows/cols in current window does not match output from bnoise!!\n");
fprintf(stderr,"From window:  %d rows and %d cols\n",nrows, ncols);
fprintf(stderr,"From file:  %d rows and %d cols\n",filerows, filecols);
        sleep(3);
    }

    file_res = (n-s)/(filerows-1);
    if (file_res != window.ns_res)
    {
        fprintf(stderr,"Warning -- ns_res doesn't match output from bnoise\n");
		fprintf(stderr,"From file %f\n",file_res);
        sleep(3);
    }

    file_res = (e-w)/(filecols-1);
    if (file_res != window.ew_res)
    {
        fprintf(stderr,"Warning -- ew_res doesn't match output from bnoise\n");
		fprintf(stderr,"From file %f\n",file_res);
        sleep(3);
    }

    grid = G_allocate_cell_buf();

/* read grid values from bnoise output file, and write them to new
   GRASS cell file.  If the value has been assigned -99 by the
   bnoise program (denoting no noise at this location), use value
   of 0 (=no data in GRASS).
*/

    for (i=0; i<nrows; i++)
    {
        ptr = grid;
        for (j=0; j<ncols; j++)
        {
            if (fscanf(tape1_fd,"%f",&value) != 1)
            {
                fprintf(stderr,"Error in bnoise output file...cannot complete\n");
                printf("\n Hit return to continue...");
                G_gets(buf);
                return;
            }
            if (value == -99.)
                value = 0.;
            *ptr++ = (CELL)(value * 1000.0);
        }
        if (G_put_map_row(new_fd, grid, i) < 0)
        {
            G_fatal_error("error while writing to cell file [%s]\n",
              new_name);
            exit(-2);
        }
    }

    fclose(tape1_name);
    G_close_cell(new_fd);

/* reset boundaries for window in case user executes bnoise program
   again before quitting.
*/
    window.north -= (window.ns_res/2.);
    window.south += (window.ns_res/2.);
    window.east -= (window.ew_res/2.);
    window.west += (window.ew_res/2.);

}
