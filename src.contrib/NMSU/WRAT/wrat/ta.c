#include "gis.h";
#include "wrat.h"

ta()
{
int here = 1;
end = 0;
while(!end)
   {
option = 0;

V_clear();
V_line(2,"            TERRAIN ANALYSIS");
V_line(4, "       What would you like to do?");
if(complete[1])
    V_line(7, "    1.  Create an Idealized Elevation Model    DONE");
else
    V_line(7, "    1.  Create an Idealized Elevation Model");

if(complete[2])
    V_line(9, "    2.  Produce Drainage Direction Map         DONE");
else
    V_line(9, "    2.  Produce Drainage Direction Map");

if(complete[3])
    V_line(11,"    3.  Produce Drainage Accumulation Map      DONE");
else
    V_line(11,"    3.  Produce Drainage Accumulation Map ");

if(complete[4])
    V_line(13,"    4.  Create a Slope Map                     DONE");
else
    V_line(13,"    4.  Create a Slope Map ");

V_line(15,"    5.  Quit");
    
V_line(19,"    Choose desired option:  ");
V_ques(&option,'i',19,29,1);

V_intrpt_ok();
if(! V_call())        /*  call & check intrpt */
    option = 5;
   

 
switch (option)
     {
      case 1:  
       here = 1;
       while(here)
          {
          V_clear();
          V_line(2,"         IDEALIZED ELEVATION MODEL ");
          V_line(4,"     Please supply these map names.");
          V_line(7, "    name of the base elevation model ...........");
          V_line(9, "    name to call the idealized elevation model..");
          V_ques(elev_map_name,'s',7,50,20);
          V_ques(ideal_map_name,'s',9,50,20);

 	  V_intrpt_ok();
          if( V_call() == 0 )         /*  call & check intrpt */
             {
             here = 0;
             continue;
             }
 
          if(strcmp (elev_map_name,"list") ==0 )  /* see if list asked for */
              {
              sprintf(command,"g.list rast\n");
              G_system(command); 
              continue;
              }

          elev_mapset = G_find_cell(elev_map_name,"");  /* see if map exists  */
          if(elev_mapset == NULL)
             {
              printf("Elevation Map '%s' not found\n",elev_map_name);
              printf("please try again.\n");
              sleep(3);
              continue;
             }

         if(elev_mapset != NULL && ideal_map_name != "_" && ideal_map_name != "")
            {  
            here = 0;

           if (ideal() )
               {
               complete[1] = 1;
               write_file();
               sprintf(command, "d.rast %s\n",ideal_map_name);
               G_system(command);
               }
           else
               {
               fprintf(stderr,"Bad exit status from IDEAL -- step not completed\n");
               printf("\nHit return to continue...");
               }
           }
       } /*   end while  */

            break;

     case 2:
       end=0;
       while(!end)
          {
          V_clear();
          V_line(2,"                    DRAINAGE DIRECTION");
          V_line(7, "    name of the idealized elevation model ......");
          V_line(9, "    name to call the drainage direction map.....");
          V_const(ideal_map_name,'s',7,50,20);
          V_ques(dir_map_name,'s',9,50,20);

 	  V_intrpt_ok();
          if( V_call() == 0 )         /*  call & check intrpt */
             {
             end = 1;
             continue;
             }
 
          if(strcmp (dir_map_name,"list") ==0 )  /* see if list asked for */
              {
              sprintf(command,"g.list rast\n");
              G_system(command); 
              continue;
              }

          ideal_mapset = G_find_cell(ideal_map_name,"");  /* see if map exists  */
          if(ideal_mapset == NULL)
             {
              printf("Idealized Elevation Map '%s' not found\n",ideal_map_name);
              printf("please try again.\n");
              sleep(3);
              continue;
             }

         if(ideal_mapset != NULL && dir_map_name != "_" && dir_map_name != "")
            {  
            end = 1;

           if (upall() == 1 )
               {
               complete[2] = 1;
               write_file();
               sprintf(command, "d.rast %s\n",dir_map_name);
               G_system(command);
               }
           else
             {
             fprintf(stderr,"Bad exit status from DRAINING -- step not completed\n");

             printf("\nHit return to continue...");
             }
          }


          }
          end = 0;
          break;


     case 3:
       end = 0;
       while(!end)
          {
          V_clear();
          V_line(2,  "            DRAINAGE ACCUMULATION");
          V_line(7, "    name of the drainage direction .............");
          V_line(9, "    name to call the drainage accumulation map..");
          V_const(dir_map_name,'s',7,50,20);
          V_ques(da_map_name,'s',9,50,20);

 	  V_intrpt_ok();
          if( V_call() == 0)                  /* call & check intrpt */
            {
            end = 1;
            continue;
            }

          dir_mapset = G_find_cell(dir_map_name,"");   /* check for maps */
          if(dir_mapset == NULL)
             {
             printf("Drainage Direction Map '%s' not found\n",dir_map_name);
             printf("please try again.\n");
             sleep(3);
             }

         if(dir_mapset != NULL && da_map_name != "_" && da_map_name != "")
            {
            end = 1;

           if (da())
              {
               complete[3] = 1;
               write_file();                 /* update project file  */
               sprintf(command, "d.rast %s\n",da_map_name);
               G_system(command);
              }
           else
             {
              fprintf(stderr,"Bad exit status from DA -- step not completed\n");
              printf("\nHit return to continue...");
              sleep(4);
             }
          }

        } 
        end = 0;
        break;


      case 4:
        end = 0;
        while(!end)
          {  
          V_clear();
          V_line(2, "                     SLOPE MAP ");
          V_line(7, "    name of the dealized elevation mopdel?......");
          V_line(9, "    name to call the slope map..................");
          V_const(ideal_map_name,'s',7,50,20);
          V_ques(slope_map_name,'s',9,50,20);

 	  V_intrpt_ok();
          if( V_call() == 0 )                /* call & check intrpt  */
            {
            end = 1;
            continue;
            }

          mapset = G_find_cell(ideal_map_name,"");  /* check for map */
          if(mapset == NULL)
             {
             printf(" Idealized Elevation Map '%s' not found\n",dir_map_name);
             printf("please try again.\n");
             continue;
             }

         if(mapset != NULL && slope_map_name != "_" && slope_map_name != "")
            {
            end = 1;
            sprintf(command,"r.slope.aspect elevation=%s slope=%s\n",ideal_map_name,slope_map_name);

           if (G_system(command) == 0 )
              {
              complete[4] = 1;
              write_file();
               sprintf(command, "d.rast %s\n", slope_map_name);
               G_system(command);
              }
           else
             {
             fprintf(stderr,"Bad exit status from SLOPE  -- step not completed\n");
             printf("\nHit return to continue...");
             }
          }
        }
        end = 0;
        break;

      case 5:
        end = 1;
        break;
    
      default :
        fprintf(stderr,"Please select a given option number.\n");
        end = 1;
        sleep(3);
        break;

     } /*  end switch */
}  /*  end while */

return (1);

} /************************* end main *******************************/

