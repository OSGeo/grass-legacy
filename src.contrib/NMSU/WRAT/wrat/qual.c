
#include "gis.h";

#include "wrat.h";

qual()
 {
 int flag=1;


 drydays = 7;
 ok=0, end = 0;
 check_assumptions();


 while(1)
    {

    option = 0;

    V_clear();
    V_line(2, "                 POLLUTION ANALYSIS");
    V_line(4, "         What would you like to do?");
    V_line(7, "    1.  Produce Maps of Contaminant Source Areas");
    V_line(9, "    2.  Route Contaminants through the Study Area");

    V_line(14,"    3.  quit");

    V_line(19,"    Choose desired option:  ");
    V_ques(&option,'i',19,29,1);

    V_intrpt_ok();
    if( !V_call() )
        option = 3;
	  
    switch (option)
       {

    case 1:      /****** contaminant source areas   *************/
     
    printf("                CONTANINANT SOURCE AREAS\n\n");
    printf("    It is possible to make source area maps for several\n");
    printf("    contaminants: sediment, nitrogen, phosphorous, and COD.\n");
    printf("    You must produce a sediment source area map to make\n");
    printf("    the other contaminant source area maps.  However, you may\n");
    printf("    choose to make only a sediment map, or a sediment map and\n");
    printf("    and combination of the other maps.\n\n");
    printf(" \n\n\n");

    if (1 != ( G_yes ("    Shall we continue ?",1)))
       break;

    ok = 0;
    while(!ok)
          {
          ok = 1;
          V_clear();
          V_line(2, "         Please enter these map names.");
          V_line(5, "    Land cover map   ....................");
          V_line(7, "    Runoff map ..........................");
          V_line(9, "    Slope map ..........................."); 
          V_line(11,"    Map of erodibility 'K' factors ......");
          V_line(13,"    Soil texture map ....................");
          V_line(15,"    Sediment source area map ............");
          V_line(17,"    If you used a design storm,");
          V_line(18,"         how many inches of rain `fell'..");
          V_line(19,"    If you used a rainfall map,");
          V_line(20,"                the Rainfall map name ...");

          V_ques(landcover_map_name,'s',5,42,20);
          V_ques(runoff_map_name,'s',7,42,20);
          V_ques(slope_map_name,'s',9,42,20);
          V_ques(Ksoil_map_name,'s',11,42,20);
          V_ques(soiltext_map_name,'s',13,42,20);
          V_ques(seds_map_name,'s',15,42,20);
          V_ques(&rain,'f',18,42,4);
          V_ques(rain_map_name,'s',20,42,20);

          V_intrpt_ok();
          if( !V_call() )
             break;
printf("\n\n\nin qual rain= %f rmap= %d\n",rain,rmap);
sleep(2);
	  if (rain == 0)
              rmap = 1;
          sprintf(command,"g.list rast\n");
          if(!strncmp(landcover_map_name, "list",4))
            {
            ok=0;
            G_system(command); 
            }

         if(!strncmp(runoff_map_name, "list",4))
            {
            ok=0;
            G_system(command); 
            }

         if(!strncmp(slope_map_name, "list",4))
            {
            ok=0;
            G_system(command); 
            }

         if(!strncmp(Ksoil_map_name, "list",4))
            {
            ok=0;
            G_system(command); 
            }

         if(!strncmp(soiltext_map_name, "list",4))
            {
            ok=0;
            G_system(command); 
            }

         if(!strncmp(rain_map_name, "list",4))
            {
            ok=0;
            G_system(command); 
            }

          mapset = G_find_cell(landcover_map_name, ""); 
          if(mapset == NULL)
             {
             printf("Landcover map `%s' not found.\n",landcover_map_name);
             printf("Please try again.\n\n");
             mapset=G_ask_cell_old("landcover map ?",landcover_map_name);
             } 
    
          mapset = G_find_cell(runoff_map_name, ""); 
          if(mapset == NULL)
             {
             printf("Runoff map `%s' not found.\n",runoff_map_name);
             printf("Please try again.\n\n");
             mapset=G_ask_cell_old("runoff map ?",runoff_map_name);
             } 
    
          mapset = G_find_cell(slope_map_name, ""); 
          if(mapset == NULL)
             {
             printf("Slope map `%s' not found.\n",slope_map_name);
             printf("Please try again.\n\n");
             mapset=G_ask_cell_old("slope map ?",slope_map_name);
             } 
    
          mapset = G_find_cell(Ksoil_map_name, ""); 
          if(mapset == NULL)
             {
             printf("K factor map `%s' not found.\n",Ksoil_map_name);
             printf("Please try again.\n\n");
             mapset=G_ask_cell_old("K factor map ?",Ksoil_map_name);
             } 
    
          mapset = G_find_cell(soiltext_map_name, ""); 
          if(mapset == NULL)
             {
             printf("Soil texture map `%s' not found.\n",soiltext_map_name);
             printf("Please try again.\n\n");
             mapset=G_ask_cell_old("Soil texture map ?",soiltext_map_name);
             } 

          if(rain == 0)
            {
             mapset = G_find_cell(rain_map_name, ""); 
             if(mapset == "NULL")
               {
               printf("Rainfall map `%s' not found.\n",rain_map_name);
               printf("Please try again.\n\n");
               mapset=G_ask_cell_old("Rainfall map ?",rain_map_name);
               }
              else
                rmap = 1;  
printf("qual lookd for and found a rain map\n");
sleep(2);
           }
     } /*  end while !ok */

     V_clear();
     V_line(2, "    How many days have passed since the last ");
     V_line(3, "  rain storm?  This is used to define the amount of ");
     V_line(4, "  dust and dirt to accumulated on streets and roads.");
     V_line(5, "  If you are modeling an area which sweeps their ");
     V_line(6, "  streets every week consider 7 days if no substantial");
     V_line(7, "  rain has fallen since.  This is also the default");
     V_line(8, "  which corresponds to average conditions.");
     V_line(11,"  Days since last rain? ..............");

     V_ques(&drydays,'f',11,42,4);

     V_intrpt_ok();
     if( !V_call() )
          break;


     ok=0;
     while(!ok)
       {
       ok=1;

       V_clear();
       V_line(2, "       Enter the names of those maps you wish to ");
       V_line(3, "    create.  Remember it is not possible to route");
       V_line(4, "    contaminants without their source area maps");
       V_line(5, "    and a sediment source area map");

       V_line(8, "    Sediment source area map ............");
       V_line(10,"    Nitrogen source area map ............");
       V_line(12,"    Phosphorous source area map ........."); 
       V_line(14,"    COD source area map .................");

       V_ques(seds_map_name,'s',8,42,20);
       V_ques(ns_map_name,'s',10,42,20);
       V_ques(ps_map_name,'s',12,42,20);
       V_ques(cods_map_name,'s',14,42,20);

       V_intrpt_ok();
       if( !V_call() )
          break;

      if(strcmp(seds_map_name, "_") == 0)
         {
          printf("You must enter a name for a sediment source area map.\n");
          sleep(4);
          ok = 0;
         }

      /* set flags for optional products */

      if(strcmp(ns_map_name, "_") )
           ns = 1;
      if(strcmp(ps_map_name, "_") )
           ps = 1;
      if(strcmp(cods_map_name, "_") )
           cod = 1;


      } /* end while !ok */
              

           if(cs() == 1)
               {
               write_file();
               sprintf(command, "d.rast %s\n", seds_map_name);
               G_system(command);
               }
           else
               {
               fprintf(stderr, "Bad exit status from CONTAMINANT SOURCE\n");
               sleep(3);
               }

         break;


    case 2:          /*** contaminant routing  ***/
      flag = 1;
      while (flag)
        {

       ok = 0;
       while (!ok)
         {
         ok = 1;
         V_clear();
         V_line(1, "              CONTAMINANT ROUTING");
         V_line(3, "       Enter the names of the source area maps for");
         V_line(4, "    contaminants to be routed. ");
         V_line(5, "    To get a listing of available map layers type ");
         V_line(6, "    list in place of a map name. ");

         V_line(8, "    Sediment source area map ............");
         V_line(10,"    Nitrogen source area map ............");
         V_line(12,"    Phosphorous source area map ........."); 
         V_line(14,"    COD source area map .................");

         V_ques(seds_map_name,'s',8,42,20);
         V_ques(ns_map_name,'s',10,42,20);
         V_ques(ps_map_name,'s',12,42,20);
         V_ques(cods_map_name,'s',14,42,20);
      
         V_intrpt_ok();

         if( !V_call() )
            {
            flag = 0;
            continue;
            }

          sprintf(command,"g.list rast\n");
          if(!strncmp(seds_map_name, "list",4))
            {
            ok=0;
            G_system(command); 
            }
          if(!strncmp(ns_map_name, "list",4))
            {
            ok=0;
            G_system(command); 
            }
          if(!strncmp(ps_map_name, "list",4))
            {
            ok=0;
            G_system(command); 
            }
          if(!strncmp(cods_map_name, "list",4))
            {
            ok=0;
            G_system(command); 
            }

      if(strncmp(seds_map_name,"_") != 0 )
         {
         seds_mapset = G_find_cell(seds_map_name, "");
         if(seds_mapset == NULL)
           {
           printf(" Sorry, sediment source map %s not found.\n",seds_map_name);
           printf(" Please try again.\n");
           sleep (4);
           ok = 0;
           }
         }
      if(strncmp(ns_map_name,"_") != 0 )
        {
        ns_mapset = G_find_cell(ns_map_name, "");
        if(ns_mapset == NULL)
           {
           printf(" Sorry, nitrogen source map %s not found.\n",ns_map_name);
           printf(" Please try again.\n");
           sleep (4);
           ok = 0;
           }
         }
      if(strncmp(ps_map_name,"_") != 0 )
         {
         ps_mapset = G_find_cell(ps_map_name, "");
         if(ps_mapset == NULL)
           {
           printf("Sorry, phosphorous source map %s not found.\n",ps_map_name);
           printf(" Please try again.\n");
           sleep (4);
           ok = 0;
           }
         }
     if(strncmp(cods_map_name,"_") != 0 )
        {
        cods_mapset = G_find_cell(cods_map_name, "");
        if(cods_mapset == NULL)
           {
           printf(" Sorry, COD source map %s not found.\n",cods_map_name);
           printf(" Please try again.\n");
           sleep (4);
           ok = 0;
           }
        }

    }    /*  end while !0k  */

 
       ok = 0;
       while(!ok && flag)
         {
          ok = 1;

          V_clear();
          V_line(2, "    Enter the names of those maps you wish to produce.");
          V_line(3, "    Please remember, there must be a corrsoponding");
          V_line(4, "    source area map to make a routing map.");
          V_line(7, "    Sediment routing map ................"); 
          V_line(9, "    Nitrogen routing map ................"); 
          V_line(11,"    Phosphorous routing map ............."); 
          V_line(13,"    COD routing map ....................."); 

          V_ques(sedf_map_name,'s',7,42,20);
          V_ques(nf_map_name,'s',9,42,20);
          V_ques(pf_map_name,'s',11,42,20);
          V_ques(codf_map_name,'s',13,42,20);

          V_intrpt_ok();

          if( !V_call() )
            {
             flag = 0;
             break;
            }

        }  /**** end while !ok && flag  **********/
 
       ok = 0;
       while(!ok && flag)
         {
          ok = 1;

          V_clear();
          V_line(2, "    If you have maps of Best Management Practices");
          V_line(3, "    you can enter them here.  These maps should");
          V_line(4, "    hold the % reduction of the contaminant flowing");
          V_line(5, "    through those cells.");
          V_line(7, "    Sediment BMPs map ..................."); 
          V_line(9, "    Nitrogen BMPs map ..................."); 
          V_line(11,"    Phosphorous BMPs map ................"); 
          V_line(13,"    COD BMPs map ........................"); 

          V_ques(bmps_map_name,'s',7,42,20);
          V_ques(bmpn_map_name,'s',9,42,20);
          V_ques(bmpp_map_name,'s',11,42,20);
          V_ques(bmpc_map_name,'s',13,42,20);

          V_intrpt_ok();

          if( !V_call() )
             {
              flag = 0;
              break;
             }

        }  /**** end while !ok && flag  **********/


       if (flag)     /*  if not cancled  */
          {
          if(testname(sedf_map_name))
              seds = 1;
          if(testname(nf_map_name))
              ns = 1;
          if(testname(pf_map_name))
              ps = 1;
          if(testname(codf_map_name))
              cods = 1;
          if(testname(bmps_map_name))
              bmps = 1;
          if(testname(bmpn_map_name))
              bmps = 1;
          if(testname(bmpp_map_name))
              bmpp = 1;
          if(testname(bmpc_map_name))
              bmpc = 1;


          if(rout() )
              {
              write_file();
              sprintf(command, "d.rast %s\n", sedf_map_name);
              G_system(command);
              }
          else
              {
              fprintf(stderr, "Bad exit status from CONTAMINANT ROUTE\n");
              sleep(3);
              }
          flag = 0;   /* don't repeat if it runs calls the program */
          } /*** end command line if *********/
       } /*  end while flag */
       break;


    case 3:
        return (1);
        break;

    }  /*** end switch  ***/

  } /**** end while 1  **/

}   /***** end qual  *******/

testname(name)
char *name;
{
if(strncmp(name,"_") !=0 && strncmp(name,"")!=0)
   return(1);
else
   return(0);
}


check_assumptions()
{
  FILE *assump_fd;
  char location[100], name[110];
  
    sprintf(location, "%s",  G_location_path());
    sprintf(name, "%s/.wrat_assumptions",location);

    if ((assump_fd = fopen(name, "r+")) == NULL)
         {
          printf(" OOPS !! error opening hidden assumptios file\n\n");
          printf(" Please run Modify hidden assumptions option\n");
          printf(" in the main menu. Thanks!\n\n");
          sleep(4);
         } 
      else 
         {
	  /* read_assump(assump_fd,avrain,rei,slopelen,dust,nitro,pho); */
	  read_assump();
         }


      if (avrain == 42)
         {
          printf("The average annual rainfall in the hidden assumption file\n");
          printf("is set at 42 inches, which is the default! \n");
          printf("Are you sure you don't want to modify that?\n\n");
         }

     if(rei == 150)
         {
          printf("\n\nThe average annual rainfall energy intensity\n");
          printf("in the hidden assumption file is set at 150,\n");
          printf("which is the default! \n");
          printf("Are you sure you don't want to modify that?\n\n");
         }

    if (rei == 150 || avrain == 42)
       {
       if (1 != ( G_yes ("    Shall we continue ?",1)))
          return(0);
       }

}



/* commenting out finction


read_assump(assump_fd,avrain,rei,slopelen,dust,nitro,pho)

    float avrain,rei,dust,nitro,pho;
    double slopelen;
    FILE assump_fd;
{
rewind(assump_fd);

fscanf(assump_fd, "This file holds the active hidden assumptions.\n\n" );
fscanf(assump_fd, "Average annual rainfall = %f inches\n", avrain );
fscanf(assump_fd, "Average annual energy intensity of rain = %f \n",rei );
fscanf(assump_fd, "Average slope length in study area = %f feet \n",slopelen);
fscanf(assump_fd, "Dust & dirt accumulation per day = %f pounds \n", dust );
fscanf(assump_fd, "Concentration of Nitrogen in dust = %f g/Kg \n",nitro);
fscanf(assump_fd, "Concentration of Phosphorous in dust = %f g/Kg \n",pho);

fclose(assump_fd);

}

*/
