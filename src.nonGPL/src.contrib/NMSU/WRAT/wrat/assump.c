#include "gis.h"
#include "wrat.h"


assump()
   {
   int go;

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
          printf("writing assumptions file\n");
          assump_fd = fopen(name, "w");
          write_assump();
         }
      else 
         {
          printf("reading assumptions file\n");
          read_assump();
         }



   printf(" \n\n\n\n\n          WELCOME to the HIDDEN ASSUMPTIONS MODULE\n\n");
   printf("   All models make assumptions about what's being modeled. This\n");
   printf("WRAT assumes the amount of rain per year and the erosive energy\n");
   printf("of that rain to be like New Jersey where it was first used. It\n");
   printf("makes similar assumptions about the terrain. WRAT also assumes \n");
   printf("that the pollution in urban areas conforms to national averages.\n");
   printf("     The next few screens will show you the current assumptions \n");
   printf("and suggested defaults based on New Jersey and national averages.\n");
   printf("If changes are made they will be reflected in ALL projects\n");
   printf("started AFTER these changes and until further changes\n");
   printf("are made. This allows you to set the paramiters\n");
   printf("for a geographic area.\n\n\n");

    go = (G_yes("    Shall we continue?",1));

    if (go)
        {
          V_clear();
          V_line (2, "              CLIMATIC AVERAGES ");
          V_line (5, " What is the average yearly rainfall in inches? ...");
          V_line (7, " What is the average annual erosive energy intensity");
          V_line (8, " of that yearly rainfall (NJ = 175) ...............");

          V_ques (&avrain, 'f',5,54,4);
          V_ques (&rei, 'f',8,54,4);

          V_call();
         }
    
    if (go)
        {
          V_clear();
          V_line (2,"             TERRAIN ASSUMPTION");
          V_line (5," The Universal Soil Loss Equation needs the average");
          V_line (7," `field slope length'. This in the number of feet water");
          V_line (8," flows across a field before it is concentrated into");
          V_line (9," a chanel by the topography or human factors like a");
          V_line (10," ditch, wall or road. If in doubt leave this one alone!");
          V_line (11," The default was originally 150  ");
          V_line (14," What do you wish to use for the field slope length? ..");

          V_ques (&slopelen, 'f',14,58,4);

          V_call();
         }

    if (go)
        {
          V_clear();
          V_line (2, "               URBAN POLLUTION  ");
          V_line (4, " The algorithm for urban NPS pollution assume a");
          V_line (5, " certain amount of dust and dirt accumulate per 100");
          V_line (6, " feet of curb length per day. It also assumes that ");
          V_line (7, " a certain amount of that dust and dirt is Nitrogen ");
          V_line (8, " and Phosphorous. ONLY adjust these assumptions IF");
          V_line (9, " YOU HAVE DATA TO SUPPORT THE CHANGE !!! ");
          V_line (12," Pounds of dust & dirt {2.3 lbs} ........... ");
          V_line (13," mg/Kg nitrogen    {0.48} .................. ");
          V_line (14," mg/Kg phosphorous {0.05} .................. ");

          V_ques (&dust, 'f',12,48,6);
          V_ques (&nitro, 'f',13,48,6);
          V_ques (&pho, 'f',14,48,6);

          V_call();
         }


    if ((assump_fd = fopen(name, "r+")) != NULL)
        write_assump();
     else
       {
        printf("oops!  trouble writing assumtions file \n\n");
        sleep(3);
       }


}



write_assump()

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



read_assump()

{
rewind(assump_fd);

fscanf(assump_fd, "This file holds the active hidden assumptions.\n\n" );
fscanf(assump_fd, "Average annual rainfall = %f inches\n", &avrain );
fscanf(assump_fd, "Average annual energy intensity of rain = %f \n", &rei );
fscanf(assump_fd, "Average slope length in study area = %f feet \n", &slopelen);
fscanf(assump_fd, "Dust & dirt accumulation per day = %f pounds \n", &dust );
fscanf(assump_fd, "Concentration of Nitrogen in dust = %f g/Kg \n", &nitro);
fscanf(assump_fd, "Concentration of Phosphorous in dust = %f g/Kg \n", &pho);

fclose(assump_fd);

}
