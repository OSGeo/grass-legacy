/********************* intro ********************************
*
*  This is the main Menu which calls sub-menus and guids
*  project administration.
*
*********************************************************/

# include "gis.h";
# include "segment.h";
# include "wrat.h";
# include "math.h";


main(argc,argv) char *argv[];
{
G_gisinit(argv[0]); 

/*  initialize all variables used as character
    A strings, and the table for tallying which
    programs in this series have been completed  */


    strcpy(proj_name,"_");
    strcpy(elev_map_name,"_");
    strcpy(ideal_map_name,"_");
    strcpy(dir_map_name,"_");
    strcpy(da_map_name,"_");
    strcpy(slope_map_name,"_");
    strcpy(landcover_map_name,"_");
    strcpy(Hsoil_map_name,"_");
    strcpy(soiltext_map_name,"_");
    strcpy(Ksoil_map_name,"_");
    strcpy(runoff_map_name,"_");
    strcpy(rain_map_name,"_");
    strcpy(qp_map_name,"_");
    strcpy(mq_map_name,"_");
    strcpy(seds_map_name,"_");
    strcpy(ns_map_name,"_");
    strcpy(ps_map_name,"_");
    strcpy(cods_map_name,"_");
    strcpy(sedf_map_name,"_");
    strcpy(nf_map_name,"_");
    strcpy(pf_map_name,"_");
    strcpy(codf_map_name,"_");
    strcpy(bmps_map_name,"_");
    strcpy(bmpn_map_name,"_");
    strcpy(bmpp_map_name,"_");
    strcpy(bmpc_map_name,"_");
    rain = 0;
    out=0;

    pho = .05;
    nitro = .48;
    dust = 2.3;
    slopelen = 150;
    rei = 175;
    avrain = 42;

    sprintf(location, "%s",  G_location_path());
    sprintf(name, "%s/.wrat_assumptions",location);

    if ((assump_fd = fopen(name, "r+")) == NULL)
         {
          assump_fd = fopen(name, "w");
          write_assump_first();
         }


/*  First screen, introducing the system, and asking for a project title.
    The project under this title may have already begun.  If so, the
    project information will be searched for and read in from the file.  */


while(1)
   {
   out = 0;
   while(!out)
        {
        error = 0;
        option = 0;

        V_clear();
        V_line(1, "         WELCOME TO THE WATER RESOURCE ASSESSMENT TOOL");
        V_line(3, "  This program will help you through the steps needed to assess");
        V_line(4, "  hydrologic and water quality characteristics of a study area.  If");
        V_line(5, "  you are starting a new project, be sure your window is set correctly. ");
        V_line(6, "  The current window, when you start a project, is recorded and");
        V_line(7, "  invoked automatically every time you work on that project.");
        V_line(8, "  Input and product map names are also recorded in a `wrat/project file'.");

        V_line(10,"  Choose desired option:");
        V_line(11,"  1.  Create new project");
        V_line(12,"  2.  Create new project based on currently existing project");
        V_line(13,"  3.  Work on an existing project");
        V_line(14,"  4.  Modify hidden assumptions");
        V_line(16,"  5.  Remove old files");
        V_line(18,"  6.  Exit");

        V_line(20,"       Desired option:  ");

        V_ques(&option,'i',20,25,1);


        V_intrpt_msg("EXIT");
        V_intrpt_ok();
        if (V_call() == 0)
            exit(2);

        switch (option)
        {
        case 1:    proj_mapset = G_ask_new("Enter project name:",proj_name,
                     "wrat/project", "parameter storing","");
                   if (!proj_mapset)
                       {
                       error = 1;
                       option=0;
                       continue;
                       }
                   proj_fd = G_fopen_new("wrat/project", proj_name);
                   if (!proj_fd)
                       {
                       fprintf(stderr,"Problem opening project file [%s]\n",
                       proj_name);
                       error = 1;
                       option=0;
                       break;
                       }
                   if (G_get_window (&window) < 0)
                       {
                       printf("can't read current window !\n");
                       printf("exiting ");
                       option=0;
                       break;
                       }

                   write_new_file(proj_fd);
                   fclose(proj_fd);
                   out = 1;
                   break;

        case 2:    proj_mapset = G_ask_old("Enter old project name:",proj_name,
                     "wrat/project", "parameter storing","");
                   if (!proj_mapset)
                       {
                       error = 1;
                       option=0;
                       break;
                       }
                   proj_fd = G_fopen_old("wrat/project", proj_name,
                     proj_mapset);
                   if (!proj_fd)
                       {
                       fprintf(stderr,"Problem opening project file [%s]\n",
                       proj_name);
                       error = 1;
                       option=0;
                       break;
                       }
                   read_file(proj_fd);
                   fclose(proj_fd);


		   G__get_window(&window,"windows",proj_name,proj_mapset);
  		   if( G_set_window(&window) != 1)
                         {
                         printf(" oops! could not set window, leaving\n");
                         sleep(3);
                         option=0;
                         break;
                         }
                    else
                         G_system("d.erase");

                   if (G_get_window (&window) < 0)
                       {
                       printf("can't read current window !\n");
                       printf("exiting ");
                       option=0;
                       break;
                       }

                   proj_mapset = G_ask_new("Enter new project name:",proj_name,
                     "wrat/project", "parameter storing","");
                   if (!proj_mapset)
                       {
                       error = 1;
                       option=0;
                       break;
                       }


                   proj_fd = G_fopen_new("wrat/project", proj_name);
                   if (!proj_fd)
                       {
                       fprintf(stderr,"Problem opening project file [%s]\n",
                       proj_name);
                       error = 1;
                       option=0;
                       break;
                      }

                   write_new_file(proj_fd);
                   fclose(proj_fd);
                   out = 1;
                   break;

        case 3:    proj_mapset = G_ask_old("Enter existing project name:",proj_name,
                     "wrat/project", "parameter storing","");
                   if (!proj_mapset)
                       {
                       error = 1;
                       option=0;
                       break;
                       }
                   proj_fd = G_fopen_old("wrat/project", proj_name,
                     proj_mapset);
                   if (!proj_fd)
                       {
                       fprintf(stderr,"Problem opening project file [%s]\n",
                       proj_name);
                       error = 1;
                       option=0;
                       break;
                       }
                   read_file(proj_fd);
                   fclose(proj_fd);
print_proj();
printf("this is the current project file:\n");
sleep(3);

		   sprintf(command,"g.region region=%s\n",proj_name);
                   G_system(command);
                   G_system("d.erase");

                   out = 1;
                   break;

	case 4:
                   assump();
                   option = 0;
                   break;

        case 5:    
                   remove(); 
                   option = 0;
                   break;

        case 6:   
                   exit(0);
                   break;

        default:   fprintf(stderr,"Please choose a given option number\n");
                   sleep(2);
                   break;
        }

    }  /** end main menu while !out  **/


    out = 0;
    while (!out)
    {
        option = 0;

        V_clear();
        V_line(2,"            PROJECT NAME:");
        V_const(proj_name,'s',2,28,40);

        V_line(4, "     What would you like to do ?");
        V_line(7, "     1.  Terrain Analysis");
        V_line(9, "     2.  Runoff Calculations");
        V_line(11,"     3.  Pollution Analysis ");

        V_line(14,"     4.  Quit");
    
        V_line(18,"         Choose desired option:  ");
    
        V_ques(&option,'i',18,34,1);

        V_call();

   switch(option)
      {
      case 1:
             ta();
             out = 0;
             break;


        case 2:
             runoff();
             break;

        case 3: 
            qual();
            break;

        case 4: 
            out=1;
            break;


       default:   
             fprintf(stderr,"Enter number between 1 and 4\n");
             sleep(2);
             break;
        }

    }  /** end while not out ***/
  } /*   end big while (1)  */
                       
}  /********************* end main  ***********************/


write_new_file(proj_fd)
int proj_fd;
{
for(i=1;i<=6;i++)
   {
   complete[i] = 0;
   }

G__put_window(&window,"windows",proj_name,proj_mapset);

/*
sprintf(command,"g.region save=%s\n",proj_name);
ret = G_system(command);
*/

fprintf(proj_fd, "Project Name :%s\n\n",proj_name);
fprintf(proj_fd, "Original Elevation Model .....: %s\n", elev_map_name);
fprintf(proj_fd, "Idealized Elevation Model ....: %s\n", ideal_map_name);
fprintf(proj_fd, "Drainage Direction Map .......: %s\n", dir_map_name);
fprintf(proj_fd, "Drainage Accumulation Map ....: %s\n", da_map_name);
fprintf(proj_fd, "Slope Map ....................: %s\n", slope_map_name);
fprintf(proj_fd, "Land Cover Map ...............: %s\n", landcover_map_name);
fprintf(proj_fd, "Hydrologic Soil Group Map ....: %s\n", Hsoil_map_name);
fprintf(proj_fd, "Soil Texture Map .............: %s\n", soiltext_map_name);
fprintf(proj_fd, "Soil K Factor Map ............: %s\n", Ksoil_map_name);
fprintf(proj_fd, "Runoff Map ...................: %s\n", runoff_map_name);
fprintf(proj_fd, "Rainfall Map .................: %s\n", rain_map_name);
fprintf(proj_fd, "Peak Discharge ...............: %s\n", qp_map_name);
fprintf(proj_fd, "Sedement Source Area Map .....: %s\n", seds_map_name);
fprintf(proj_fd, "Nitrogen Source Area Map .....: %s\n", ns_map_name);
fprintf(proj_fd, "Phosphorous Source Area Map ..: %s\n", ps_map_name);
fprintf(proj_fd, "COD Source Area Map ..........: %s\n", cods_map_name);
fprintf(proj_fd, "Sedement Flux Map ............: %s\n", sedf_map_name);
fprintf(proj_fd, "Nitrogen Flux Map ............: %s\n", nf_map_name);
fprintf(proj_fd, "Phosphorous Flux Map .........: %s\n", pf_map_name);
fprintf(proj_fd, "COD Flux Map .................: %s\n", codf_map_name);
fprintf(proj_fd, "Design Storm Depth ...........: %f\n", rain);
fprintf(proj_fd, "%d %d %d %d %d %d ",complete[1], complete[2], complete[3], complete[4], complete[5], complete[6]);

}



write_file()
{
proj_fd = G_fopen_new("wrat/project", proj_name);
if (!proj_fd)
    {
    fprintf(stderr,"Problem opening project file [%s]\n", proj_name);
     error = 1;
}

fprintf(proj_fd, "Project Name :%s\n\n",proj_name);
fprintf(proj_fd, "Original Elevation Model .....: %s\n", elev_map_name);
fprintf(proj_fd, "Idealized Elevation Model ....: %s\n", ideal_map_name);
fprintf(proj_fd, "Drainage Direction Map .......: %s\n", dir_map_name);
fprintf(proj_fd, "Drainage Accumulation Map ....: %s\n", da_map_name);
fprintf(proj_fd, "Slope Map ....................: %s\n", slope_map_name);
fprintf(proj_fd, "Land Cover Map ...............: %s\n", landcover_map_name);
fprintf(proj_fd, "Hydrologic Soil Group Map ....: %s\n", Hsoil_map_name);
fprintf(proj_fd, "Soil Texture Map .............: %s\n", soiltext_map_name);
fprintf(proj_fd, "Soil K Factor Map ............: %s\n", Ksoil_map_name);
fprintf(proj_fd, "Runoff Map ...................: %s\n", runoff_map_name);
fprintf(proj_fd, "Rainfall Map .................: %s\n", rain_map_name);
fprintf(proj_fd, "Peak Discharge ...............: %s\n", qp_map_name);
fprintf(proj_fd, "Sedement Source Area Map .....: %s\n", seds_map_name);
fprintf(proj_fd, "Nitrogen Source Area Map .....: %s\n", ns_map_name);
fprintf(proj_fd, "Phosphorous Source Area Map ..: %s\n", ps_map_name);
fprintf(proj_fd, "COD Source Area Map ..........: %s\n", cods_map_name);
fprintf(proj_fd, "Sedement Flux Map ............: %s\n", sedf_map_name);
fprintf(proj_fd, "Nitrogen Flux Map ............: %s\n", nf_map_name);
fprintf(proj_fd, "Phosphorous Flux Map .........: %s\n", pf_map_name);
fprintf(proj_fd, "COD Flux Map .................: %s\n", codf_map_name);
fprintf(proj_fd, "Design Storm Depth ...........: %f\n", rain);
fprintf(proj_fd, "%d %d %d %d %d %d ",complete[1], complete[2], complete[3], complete[4], complete[5], complete[6]);

fclose(proj_fd);
}


read_file(proj_fd)
int proj_fd;
{
fscanf(proj_fd, "Project Name :%s\n\n",proj_name);
fscanf(proj_fd, "Original Elevation Model .....: %s\n", elev_map_name);
fscanf(proj_fd, "Idealized Elevation Model ....: %s\n", ideal_map_name);
fscanf(proj_fd, "Drainage Direction Map .......: %s\n", dir_map_name);
fscanf(proj_fd, "Drainage Accumulation Map ....: %s\n", da_map_name);
fscanf(proj_fd, "Slope Map ....................: %s\n", slope_map_name);
fscanf(proj_fd, "Land Cover Map ...............: %s\n", landcover_map_name);
fscanf(proj_fd, "Hydrologic Soil Group Map ....: %s\n", Hsoil_map_name);
fscanf(proj_fd, "Soil Texture Map .............: %s\n", soiltext_map_name);
fscanf(proj_fd, "Soil K Factor Map ............: %s\n", Ksoil_map_name);
fscanf(proj_fd, "Runoff Map ...................: %s\n", runoff_map_name);
fscanf(proj_fd, "Rainfall Map .................: %s\n", rain_map_name);
fscanf(proj_fd, "Peak Discharge ...............: %s\n", qp_map_name);
fscanf(proj_fd, "Sedement Source Area Map .....: %s\n", seds_map_name);
fscanf(proj_fd, "Nitrogen Source Area Map .....: %s\n", ns_map_name);
fscanf(proj_fd, "Phosphorous Source Area Map ..: %s\n", ps_map_name);
fscanf(proj_fd, "COD Source Area Map ..........: %s\n", cods_map_name);
fscanf(proj_fd, "Sedement Flux Map ............: %s\n", sedf_map_name);
fscanf(proj_fd, "Nitrogen Flux Map ............: %s\n", nf_map_name);
fscanf(proj_fd, "Phosphorous Flux Map .........: %s\n", pf_map_name);
fscanf(proj_fd, "COD Flux Map .................: %s\n", codf_map_name);
fscanf(proj_fd, "Design Storm Depth ...........: %f\n", &rain);
fscanf(proj_fd, "%d %d %d %d %d %d ",&complete[1], &complete[2], &complete[3], &complete[4], &complete[5], &complete[6]);

}

print_proj()

{
printf( "Project Name :%s\n\n",proj_name);
printf( "Original Elevation Model .....: %s\n", elev_map_name);
printf( "Idealized Elevation Model ....: %s\n", ideal_map_name);
printf( "Drainage Direction Map .......: %s\n", dir_map_name);
printf( "Drainage Accumulation Map ....: %s\n", da_map_name);
printf( "Slope Map ....................: %s\n", slope_map_name);
printf( "Land Cover Map ...............: %s\n", landcover_map_name);
printf( "Hydrologic Soil Group Map ....: %s\n", Hsoil_map_name);
printf( "Soil Texture Map .............: %s\n", soiltext_map_name);
printf( "Soil K Factor map ............: %s\n", Ksoil_map_name);
printf( "Runoff Map ...................: %s\n", runoff_map_name);
printf( "Rainfall Map .................: %s\n", rain_map_name);
printf( "Peak Discharge ...............: %s\n", qp_map_name);
printf( "Sedement Source Area Map .....: %s\n", seds_map_name);
printf( "Nitrogen Source Area Map .....: %s\n", ns_map_name);
printf( "Phosphorous Source Area Map ..: %s\n", ps_map_name);
printf( "COD Source Area Map ..........: %s\n", cods_map_name);
printf( "Sedement Flux Map ............: %s\n", sedf_map_name);
printf( "Nitrogen Flux Map ............: %s\n", nf_map_name);
printf( "Phosphorous Flux Map .........: %s\n", pf_map_name);
printf( "COD Flux Map .................: %s\n", codf_map_name);
printf( "Design Storm Depth ...........: %f\n", rain);
printf( "%d %d %d %d %d %d \n\n",complete[1], complete[2], complete[3], complete[4], complete[5], complete[6]);
}



write_assump_first()

{
fprintf(assump_fd, "This file holds the active hidden assumptions.\n\n" );
fprintf(assump_fd, "Average annual rainfall = %f inches\n", avrain );
fprintf(assump_fd, "Average annual energy intensity of rain = %f \n", rei );
fprintf(assump_fd, "Average slope length in study area = %f feet \n", slopelen);
fprintf(assump_fd, "Dust & dirt accumulation per day = %f pounds \n", dust );
fprintf(assump_fd, "Concentration of Nitrogen in dust = %f g/Kg \n", nitro);
fprintf(assump_fd, "Concentration of Phosphorous in dust = %f g/Kg \n", pho);

fclose(assump_fd);

}

