
#include "gis.h";
#include "wrat.h"

runoff()
{
end = 0;
while(!end)
   {
   option = 0;

   V_clear();
   V_line(2,"                 RUNOFF ANALYSIS");
   V_line(4, "         What would you like to do?");
   if(complete[5])
       V_line(7, "    1.  Produce Runoff Map          DONE");
   else
       V_line(7, "    1.  Produce Runoff Map");

   if(complete[6])
      V_line(9, "    2.  Model Peak Discharge        DONE");
   else
      V_line(9, "    2.  Model Peak Discharge");

   V_line(11,"    3.  quit");

   V_line(18,"    Choose desired option:  ");
   V_ques(&option,'i',18,29,1);

   V_intrpt_ok();
   if( !V_call() )
      option = 3;

switch (option)
  {
  case 1:
  end = 0;
  ok = 0;
  while(!end)
    {

    printf("\n\n\n\n\n\n             WELCOME TO THE RUNOFF MODULE\n\n\n ");
    printf("      This program produces a runoff map based on the Soil\n");
    printf("    Conservation Service's curve number method. \n\n");
    printf("      You will be asked for the names of the landcover and\n");
    printf("    hydrologic soil group maps, as well as information about\n"); 
    printf("    the current and recent rain storms.  A rainfall map or\n");
    printf("    design storm may be used.\n\n\n\n");

    rmap = (G_yes("    Shall we continue ?", 1));
    if(!rmap)
       {
        end = 1;
        continue;
       }

    while(!end && !ok)
       { 
        ok = 1;
        V_clear();
        V_line(2, "         Please enter these map names.");
        V_line(7, "    The landcover map ...................");
        V_line(9, "    Map of hydrologic soil groups .......");
        V_line(15,"    Name to call the runoff map .........");

        V_ques(landcover_map_name,'s',7,42,20);
        V_ques(Hsoil_map_name,'s',9,42,20);
        V_ques(runoff_map_name,'s',15,42,20);

        V_intrpt_ok();
        if(!V_call() )
          {
          end = 1;
          ok = 1;
          break;
          }

        sprintf(command, "g.list rast\n");

        if(strncmp(landcover_map_name, "list",4)==0)
          {
          G_system(command);
          ok = 0;
printf(" in ro landcover map  = %s \n strmp = %d  \n ",landcover_map_name,ret );
sleep (3);
          continue;
          }

       if(strncmp(Hsoil_map_name, "list", 4)==0)
         {
         G_system(command);
         ok = 0;
printf(" in ra Hsoil = %s \n strmp =  \n ",Hsoil_map_name);
sleep (3);
         continue;
         }

      if(strncmp(runoff_map_name, "list", 4)==0)
        {
        G_system(command);
        ok = 0;
        continue;
        }

      mapset = G_find_cell(landcover_map_name,"");
      if(mapset == NULL)
        {
        printf("Landcover map '%s' not found.\n");
        printf("Please try again. \n\n");
        sleep(3);
        ok = 0;
        continue;
        }

     mapset = G_find_cell(Hsoil_map_name,"");
     if(mapset == NULL)
        {
        printf("Hydrologic soils group map '%s' not found.\n");
        printf("Please try again. \n\n");
        ok = 0;
        sleep(3);
        continue;
        }
    } /* end while !end && !ok */

       printf("\n\n\n\n\n\n");
   if(!end)
       {
       rmap = (G_yes("Do you want to use a design storm?", 1));


       if (rmap)    /* get design storm depth */ 
          {
          V_clear();
          V_line(10, "     Enter the amount of rainfall for the design storm");
          V_line(12, "     in inches. "); 
          V_ques(&rain, 'f', 12, 50, 4);

          V_intrpt_ok();
          if (!V_call())
             {
             end = 1;
             break;
             } 
          } 


       else            /*  ask for rainfall map */
           {
            rain_mapset = G_ask_cell_old("What is the name of the rainfall map to be used?", rain_map_name);
            if (rain_mapset == NULL)
               {
               end = 1;
               continue;
               }
           }   /* end if ! rmap  */
        }

amc=2.000;
ok=0;
while(!ok) 
   {
   V_clear();
   V_line(3, "      You must enter the antecedent moisture condition.");
   V_line(5, "      This is a number between 1 and 3. Below is a rough guide.");
   V_line(12, "      amc      inches of rain during last 5 days "); 
   V_line(13, "             growing season     nongrowing season     "); 
   V_line(15, "       1           < 1.4             < .5"); 
   V_line(16, "       2        >1.4 & < 2.1      > .5 & < 1.1"); 
   V_line(17, "       3           > 2.1             >1.1"); 
   V_ques(&amc, 'f', 7, 55, 4);

       V_intrpt_ok();
       if (!V_call())
          {
          end = 1;
          break;
          }

    if(amc>=1 && amc<=3)
       ok=1;
     else
       {
       printf(" oops!  amc must be between 1 & 3 (inclusive) \n");
       sleep(3);
       }
   } /* end while  */

   
        if(cn() == 1)
           {
           complete[5] = 1;
           sprintf(command, "d.rast %s\n",runoff_map_name);
           G_system(command);
           write_file();
           }
        else
          {
          fprintf(stderr, "Bad exit status from RUNOFF -- step not completed\n");
          printf("\nHit return to continue...");
          }
        end = 1;
    }    
    end = 0;
    break;


   case 2:
     end = 0;
     ok = 0;
     while(!end)
       {
       while (!ok)
          {
          ok = 1;
          V_clear();
          V_line(2, "         Please enter these map names.");
          V_line(5, "    Idealized elevation map .............");
          V_line(7, "    Drainage direction map ..............");
          V_line(9, "    Drainage ccumulation map ............"); 
          V_line(11,"    Runoff map ..........................");
          V_line(16,"    Name to call Peak Discharge map .....");

         V_const(ideal_map_name,'s',5,42,20);
         V_const(dir_map_name,'s',7,42,20);
         V_const(da_map_name,'s',9,42,20);
         V_ques(runoff_map_name,'s',11,42,20);
         V_ques(qp_map_name,'s',16,42,20);

         V_intrpt_ok();
         if(!V_call())
           {
           end = 1;
           continue;
           }

        sprintf(command,"g.list rast");
        if(strncmp(runoff_map_name, "list", 4) == 0)
          {
          G_system(command);
          ok = 1;
          continue;
          }

        if(strncmp(qp_map_name, "list", 4) == 0)
          {
          G_system(command);
          ok = 1;
          continue;
          }

        if(strncmp(mq_map_name, "list", 4) == 0)
          {
          G_system(command);
          ok = 1;
          continue;
          }

     /* check for input maps */

        ideal_mapset = G_find_cell(ideal_map_name,"");
        if(ideal_mapset == NULL)
           {
           printf("Idealized elevation map '%s' not found.\n", ideal_map_name);
           printf("Please try again. \n\n");
           ok = 0;
           sleep(3);
          }

       dir_mapset = G_find_cell(dir_map_name,"");
        if(dir_mapset == NULL)
           {
           printf("Drainage direction map '%s' not found.\n",dir_map_name);
           printf("Please try again. \n\n");
           ok = 0;
           sleep(3);
           }

        da_mapset = G_find_cell(da_map_name,"");
        if(da_mapset == NULL)
           {
           printf("Drainage accumulation map '%s' not found.\n",da_map_name);
           printf("Please try again. \n\n");
           ok = 0;
           sleep(3);
           }
        runoff_mapset = G_find_cell(runoff_map_name,"");
        if(runoff_mapset == NULL)
           {
           printf("Runoff map '%s' not found.\n",runoff_map_name);
           printf("Please try again. \n\n");
           ok = 0;
           sleep(3);
           }
      }   /* end  !ok while */

      if(!end)
        {
         end =1;
/*
        sprintf(command,"Qp %s %s %s %s %s\n",ideal_map_name,dir_map_name,da_map_name,runoff_map_name,qp_map_name);
*/

        if(qp())
           {
           complete[6] = 1;
           sprintf(command, "d.rast %s\n",qp_map_name);
           G_system(command);
           write_file();
           }
        else
          {
          fprintf(stderr, "Bad exit status from Qp -- step not completed\n");
          printf("\nHit return to continue...");
          }
       } /* end if ! end */

     } /* end end while */
    end = 0;
    break;

     case 3:
        end = 1;
        break;

     default:
       fprintf(stderr,"Enter a number from this menu.\n");
       sleep(2);
       break;

} /* end switch */

} /*end while */

return(1);
} /*****************  end runoff ******************************/
