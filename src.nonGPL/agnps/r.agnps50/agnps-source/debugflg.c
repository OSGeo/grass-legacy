/*************** Documentation Start ****************************

NAME:

 ************************************************************
 *                                                          *
 *      debugflg.c                                          *
 *                                                          *
 ************************************************************

  This was added at the request of Fred Theuer to be used for
  "quick and easy" debugging without the need for re-compiling.
  The file read by this routine is DEBUG.FLG and will be read
  only if it exists within the executable working directory.
  The data structure format is found in the DEBUGFLG.H file.


SYNOPSIS:
***********************************************************/

/* header files included in this procedure */

#ifdef _DOS

 #include <stdlib.h>
 #include <stdio.h>
 #include <time.h>
 #include <alloc.h>
 #include <string.h>
 #include "debugflg.h"

#else

#include <stdlib.h>
#include <stdio.h>
#include <time.h>
#include <string.h>
#include "debugflg.h"

#define SEEK_SET 0

#endif

extern SED_TABLE_ENTRY   ctable;

/* global structures used in this procedure */

/*  NONE  */

/*

INPUT PARAMETERS:

  dbfp   (FILE)          - Debug Flag file pointer.
  tflags (FLAGS_TABLE)   - Verification table data structure pointer.
  bflags (FLAGS_BASIC)   - Basic Flag's data structure pointer.
  rflags (FLAGS_ROUTINE) - Routine Flag's data structure pointer.

OUTPUT PARAMETERS:

  tflags, bflags and rflags "loaded up".      -JW                  */


#ifdef _UNIX_K_AND_R

void read_db_file(dbfp, tflags, bflags, rflags)

     FILE *dbfp;
     FLAGS_TABLE *tflags;
     FLAGS_BASIC *bflags;
     FLAGS_ROUTINE *rflags;

#else

void read_db_file(FILE *dbfp, FLAGS_TABLE *tflags, FLAGS_BASIC *bflags,
		  FLAGS_ROUTINE *rflags)

#endif

{

 int   i = 0;               /* Counter variable.                          */
 char  line_buffer[79];     /* File line-reading buffer.                  */
 char  junk1[12],junk2[12]; /* Dummy junk strings.                        */

/* TABLES: */

 for (i=0; i<8; ++i)
  fgets(line_buffer,80,dbfp); /* Skims through file headings.             */

 sscanf(line_buffer,"%s %s %d ",junk1,junk2,&tflags->hydro_table);

 fgets(line_buffer,80,dbfp);
 sscanf(line_buffer,"%s %s %d ",junk1,junk2,&tflags->sed_table);

 fgets(line_buffer,80,dbfp);
 sscanf(line_buffer,"%s %s %d ",junk1,junk2,&tflags->chem_table);

 fgets(line_buffer,80,dbfp);
 sscanf(line_buffer,"%s %s %d ",junk1,junk2,&tflags->fdlt_table);

 fgets(line_buffer,80,dbfp);
 sscanf(line_buffer,"%s %s %d ",junk1,junk2,&tflags->impnd_table);

 fgets(line_buffer,80,dbfp);
 sscanf(line_buffer,"%s %s %d ",junk1,junk2,&tflags->sdtrp_table);

 for (i=0; i<5; ++i)
  fgets(line_buffer,80,dbfp); /* Skims through file headings.             */


/* BASICS: */

 sscanf(line_buffer,"%s %s %d ",junk1,junk2,&bflags->drain_area);

 fgets(line_buffer,80,dbfp);
 sscanf(line_buffer,"%s %s %d ",junk1,junk2,&bflags->cell_info);

 fgets(line_buffer,80,dbfp);
 sscanf(line_buffer,"%s %s %d ",junk1,junk2,&bflags->channel);

 fgets(line_buffer,80,dbfp);
 sscanf(line_buffer,"%s %s %d ",junk1,junk2,&bflags->flow);

 fgets(line_buffer,80,dbfp);
 sscanf(line_buffer,"%s %s %d ",junk1,junk2,&bflags->impound);

 fgets(line_buffer,80,dbfp);
 sscanf(line_buffer,"%s %s %d ",junk1,junk2,&bflags->nutrient);

 fgets(line_buffer,80,dbfp);
 sscanf(line_buffer,"%s %s %d ",junk1,junk2,&bflags->routing);

 fgets(line_buffer,80,dbfp);
 sscanf(line_buffer,"%s %s %d ",junk1,junk2,&bflags->sediment);

 fgets(line_buffer,80,dbfp);
 sscanf(line_buffer,"%s %s %d ",junk1,junk2,&bflags->tr_55);



 for (i=0; i<4; ++i)
  fgets(line_buffer,80,dbfp); /* Skims through file headings.             */


/* ROUTINES: */

 fgets(line_buffer,80,dbfp);
 sscanf(line_buffer,"%s %s %d ",junk1,junk2,&rflags->bulk_dens);

 fgets(line_buffer,80,dbfp);
 sscanf(line_buffer,"%s %s %d ",junk1,junk2,&rflags->calc_chan);

 fgets(line_buffer,80,dbfp);
 sscanf(line_buffer,"%s %s %d ",junk1,junk2,&rflags->calc_sed);

 fgets(line_buffer,80,dbfp);
 sscanf(line_buffer,"%s %s %d ",junk1,junk2,&rflags->cell_calc);

 fgets(line_buffer,80,dbfp);
 sscanf(line_buffer,"%s %s %d ",junk1,junk2,&rflags->chan_calc);

 fgets(line_buffer,80,dbfp);
 sscanf(line_buffer,"%s %s %d ",junk1,junk2,&rflags->chan_tr_55);

 fgets(line_buffer,80,dbfp);
 sscanf(line_buffer,"%s %s %d ",junk1,junk2,&rflags->curve_num);

 fgets(line_buffer,80,dbfp);
 sscanf(line_buffer,"%s %s %d ",junk1,junk2,&rflags->curve_ro);

 fgets(line_buffer,80,dbfp);
 sscanf(line_buffer,"%s %s %d ",junk1,junk2,&rflags->decay_nut);

 fgets(line_buffer,80,dbfp);
 sscanf(line_buffer,"%s %s %d ",junk1,junk2,&rflags->feedlot);

 fgets(line_buffer,80,dbfp);
 sscanf(line_buffer,"%s %s %d ",junk1,junk2,&rflags->hydrology);

 fgets(line_buffer,80,dbfp);
 sscanf(line_buffer,"%s %s %d ",junk1,junk2,&rflags->impound);

 fgets(line_buffer,80,dbfp);
 sscanf(line_buffer,"%s %s %d ",junk1,junk2,&rflags->init_sed);

 fgets(line_buffer,80,dbfp);
 sscanf(line_buffer,"%s %s %d ",junk1,junk2,&rflags->length);

 fgets(line_buffer,80,dbfp);
 sscanf(line_buffer,"%s %s %d ",junk1,junk2,&rflags->newsoil);

 fgets(line_buffer,80,dbfp);
 sscanf(line_buffer,"%s %s %d ",junk1,junk2,&rflags->overland);

 fgets(line_buffer,80,dbfp);
 sscanf(line_buffer,"%s %s %d ",junk1,junk2,&rflags->part_pest);

 fgets(line_buffer,80,dbfp);
 sscanf(line_buffer,"%s %s %d ",junk1,junk2,&rflags->pesticide);

 fgets(line_buffer,80,dbfp);
 sscanf(line_buffer,"%s %s %d ",junk1,junk2,&rflags->pntsrc);

 fgets(line_buffer,80,dbfp);
 sscanf(line_buffer,"%s %s %d ",junk1,junk2,&rflags->prep_route);

 fgets(line_buffer,80,dbfp);
 sscanf(line_buffer,"%s %s %d ",junk1,junk2,&rflags->rclmap);

 fgets(line_buffer,80,dbfp);
 sscanf(line_buffer,"%s %s %d ",junk1,junk2,&rflags->recursive);

 fgets(line_buffer,80,dbfp);
 sscanf(line_buffer,"%s %s %d ",junk1,junk2,&rflags->routing);

 fgets(line_buffer,80,dbfp);
 sscanf(line_buffer,"%s %s %d ",junk1,junk2,&rflags->sed_flow);

 fgets(line_buffer,80,dbfp);
 sscanf(line_buffer,"%s %s %d ",junk1,junk2,&rflags->sed_nutr);

 fgets(line_buffer,80,dbfp);
 sscanf(line_buffer,"%s %s %d ",junk1,junk2,&rflags->soil_nutr);

 fgets(line_buffer,80,dbfp);
 sscanf(line_buffer,"%s %s %d ",junk1,junk2,&rflags->src_cells);

 fgets(line_buffer,80,dbfp);
 sscanf(line_buffer,"%s %s %d ",junk1,junk2,&rflags->sum_psrc);

 fgets(line_buffer,80,dbfp);
 sscanf(line_buffer,"%s %s %d ",junk1,junk2,&rflags->terh);

 fgets(line_buffer,80,dbfp);
 sscanf(line_buffer,"%s %s %d ",junk1,junk2,&rflags->ters);

 fgets(line_buffer,80,dbfp);
 sscanf(line_buffer,"%s %s %d ",junk1,junk2,&rflags->tr_55);

 fgets(line_buffer,80,dbfp);
 sscanf(line_buffer,"%s %s %d ",junk1,junk2,&rflags->xeros);

 return;
}


#ifdef _UNIX_K_AND_R

void init_db_file(tflags, bflags, rflags)

     FLAGS_TABLE *tflags;
     FLAGS_BASIC *bflags;
     FLAGS_ROUTINE *rflags;

#else

void init_db_file(FLAGS_TABLE *tflags, FLAGS_BASIC *bflags,
		  FLAGS_ROUTINE *rflags)

#endif


{
 tflags->hydro_table = 0;
 tflags->sed_table   = 1;
 tflags->chem_table  = 0;
 tflags->fdlt_table  = 0;
 tflags->impnd_table = 0;
 tflags->sdtrp_table = 0;

 bflags->drain_area  = 0;
 bflags->cell_info   = 0;
 bflags->channel     = 0;
 bflags->flow        = 0;
 bflags->impound     = 0;
 bflags->nutrient    = 0;
 bflags->routing     = 0;
 bflags->sediment    = 0;
 bflags->tr_55       = 0;

 rflags->bulk_dens   = 0;
 rflags->calc_chan   = 0;
 rflags->calc_sed    = 0;
 rflags->cell_calc   = 0;
 rflags->chan_calc   = 0;
 rflags->chan_tr_55  = 0;
 rflags->curve_num   = 0;
 rflags->curve_ro    = 0;
 rflags->decay_nut   = 0;
 rflags->feedlot     = 0;
 rflags->hydrology   = 0;
 rflags->impound     = 0;
 rflags->init_sed    = 0;
 rflags->length      = 0;
 rflags->newsoil     = 0;
 rflags->overland    = 0;
 rflags->part_pest   = 0;
 rflags->pesticide   = 0;
 rflags->pntsrc      = 0;
 rflags->prep_route  = 0;
 rflags->rclmap      = 0;
 rflags->recursive   = 0;
 rflags->routing     = 0;
 rflags->sed_flow    = 0;
 rflags->sed_nutr    = 0;
 rflags->soil_nutr   = 0;
 rflags->src_cells   = 0;
 rflags->sum_psrc    = 0;
 rflags->terh        = 0;
 rflags->ters        = 0;
 rflags->tr_55       = 0;
 rflags->xeros       = 0;

 return;
}






#ifdef _UNIX_K_AND_R

void vsetup_1(vfy, name)

     FILE *vfy;
     char* name[];

#else

void vsetup_1(FILE *vfy,char* name[])

#endif

{
 int     page = 1;
 time_t  t;

 t=time(NULL);   /* Gets system time for the file footer...     */


 fprintf(vfy,"                      VERIFICATION TABLE NO. 1: Water\n\n");
 fprintf(vfy,"Watershed: %s\n\n",name);
 fprintf(vfy,"------------------------------------------------------------------------------------------------------------------------------------------------------\n");
 fprintf(vfy,"                                                                             CELL ID\n");
 fprintf(vfy,"  GROUP         VARIABLE               UNITS          \n");
 fprintf(vfy,"------------------------------------------------------------------------------------------------------------------------------------------------------\n\n");
 fprintf(vfy," Watershed     drainage area          acres...........\n");
 fprintf(vfy,"               total cell area        acres...........\n");
 fprintf(vfy,"               feedlot area           %% of cell area..\n");
 fprintf(vfy,"               impoundment area       %% of cell area..\n");
 fprintf(vfy,"               receiving cell         id..............\n");
 fprintf(vfy,"               next routing cell      id..............\n");
 fprintf(vfy,"------------------------------------------------------------------------------------------------------------------------------------------------------\n\n");
 fprintf(vfy," Overland      length                 feet............\n");
 fprintf(vfy,"               travel time            hours...........\n");
 fprintf(vfy,"------------------------------------------------------------------------------------------------------------------------------------------------------\n\n");
 fprintf(vfy," Shallow Conc  length                 feet............\n");
 fprintf(vfy,"               velocity               fps.............\n");
 fprintf(vfy,"               travel time            hours...........\n");
 fprintf(vfy,"------------------------------------------------------------------------------------------------------------------------------------------------------\n\n");
 fprintf(vfy," Channel       top width              feet............\n");
 fprintf(vfy,"               depth at bankfull      feet............\n");
 fprintf(vfy,"               segment length         feet............\n");
 fprintf(vfy,"               velocity               fps.............\n");
 fprintf(vfy,"               travel time            hours...........\n");
 fprintf(vfy,"               time of concentration  hours...........\n");
 fprintf(vfy,"------------------------------------------------------------------------------------------------------------------------------------------------------\n\n");
 fprintf(vfy," Hydrograph    peak discharge outlet  cfs.............\n");
 fprintf(vfy,"                                      inches/hour.....\n");
 fprintf(vfy,"               tot runoff vol outlet  acre-feet.......\n");
 fprintf(vfy,"                                      inches..........\n");
 fprintf(vfy,"               avg peak discharge     cfs.............\n");
 fprintf(vfy,"               average runoff volume  acre-feet.......\n");
 fprintf(vfy,"               runoff peak volume     %% of runoff.....\n");
 fprintf(vfy,"               time to peak           hours...........\n");
 fprintf(vfy,"               time to base           hours...........\n");
 fprintf(vfy,"------------------------------------------------------------------------------------------------------------------------------------------------------\n\n");
 fprintf(vfy,"%s                                                                                                                                              Page %d.",ctime(&t),page);

 return;
}


#ifdef _UNIX_K_AND_R

void vsetup_2(vfy, name)

     FILE *vfy;
     char* name[];

#else

void vsetup_2(FILE *vfy,char* name[])

#endif

{
 int     page = 1;
 time_t  t;

 t=time(NULL);   /* Gets system time for the file footer...     */


 fprintf(vfy,"                      VERIFICATION TABLE NO. 2: Sediment--all sources\n\n");
 fprintf(vfy,"Watershed: %s\n",name);
 fprintf(vfy,"------------------------------------------------------------------------------------------------------------------------------------------------------\n");
 fprintf(vfy," PARTICLE                                                                            CELL ID\n");
 fprintf(vfy," SIZE CLASS    VARIABLE                 UNITS            \n");
 fprintf(vfy,"------------------------------------------------------------------------------------------------------------------------------------------------------\n");
 fprintf(vfy," Clay          upstream clay amount     tons.............\n");
 fprintf(vfy,"               upstream clay concen     lbs-sed/lbs-H2O..\n");
 fprintf(vfy,"               tot in-cell clay sources tons.............\n");
 fprintf(vfy,"               downstream clay amount   tons.............\n");
 fprintf(vfy,"               downstream clay concen   lbs-sed/lbs-H2O..\n");
 fprintf(vfy,"------------------------------------------------------------------------------------------------------------------------------------------------------\n");
 fprintf(vfy," Silt          upstream silt amount     tons.............\n");
 fprintf(vfy,"               upstream silt concen     lbs-sed/lbs-H2O..\n");
 fprintf(vfy,"               tot in-cell silt sources tons.............\n");
 fprintf(vfy,"               downstream silt amount   tons.............\n");
 fprintf(vfy,"               downstream silt concen   lbs-sed/lbs-H2O..\n");
 fprintf(vfy,"------------------------------------------------------------------------------------------------------------------------------------------------------\n");
 fprintf(vfy," Small         upstream s-agg amount    tons.............\n");
 fprintf(vfy," Aggregate     upstream s-agg concen    lbs-sed/lbs-H2O..\n");
 fprintf(vfy,"               tot in-cell s-agg srces  tons.............\n");
 fprintf(vfy,"               downstream s-agg amount  tons.............\n");
 fprintf(vfy,"               downstream s-agg concen  lbs-sed/lbs-H2O..\n");
 fprintf(vfy,"------------------------------------------------------------------------------------------------------------------------------------------------------\n");
 fprintf(vfy," Large         upstream l-agg amount    tons.............\n");
 fprintf(vfy," Aggregate     upstream l-agg concen    lbs-sed/lbs-H2O..\n");
 fprintf(vfy,"               tot in-cell l-agg srces  tons.............\n");
 fprintf(vfy,"               downstream l-agg amount  tons.............\n");
 fprintf(vfy,"               downstream l-agg concen  lbs-sed/lbs-H2O..\n");
 fprintf(vfy,"------------------------------------------------------------------------------------------------------------------------------------------------------\n");
 fprintf(vfy," Sand          upstream sand amount     tons.............\n");
 fprintf(vfy,"               upstream sand concen     lbs-sed/lbs-H2O..\n");
 fprintf(vfy,"               tot in-cell sand sources tons.............\n");
 fprintf(vfy,"               downstream sand amount   tons.............\n");
 fprintf(vfy,"               downstream sand concen   lbs-sed/lbs-H2O..\n");
 fprintf(vfy,"------------------------------------------------------------------------------------------------------------------------------------------------------\n");
 fprintf(vfy," Total         upstream total amount    tons.............\n");
 fprintf(vfy," Sediment      upstream total concen    lbs-sed/lbs-H2O..\n");
 fprintf(vfy,"               tot in-cell total srces  tons.............\n");
 fprintf(vfy,"               downstream total amount  tons.............\n");
 fprintf(vfy,"               downstream total concen  lbs-sed/lbs-H2O..\n");
 fprintf(vfy,"------------------------------------------------------------------------------------------------------------------------------------------------------\n\n");
 fprintf(vfy,"%s                                                                                                                                              Page %d.\n\f",ctime(&t),page);


 fprintf(vfy,"                      VERIFICATION TABLE NO. 2a: Sediment Transport--clay\n");
 fprintf(vfy,"Watershed: %s\n",name);
 fprintf(vfy,"------------------------------------------------------------------------------------------------------------------------------------------------------\n");
 fprintf(vfy," PARTITION                                                                           CELL ID\n");
 fprintf(vfy," NUMBER      VARIABLE                        UNITS         \n");
 fprintf(vfy,"------------------------------------------------------------------------------------------------------------------------------------------------------\n");
 fprintf(vfy," All         constant of proportionality     non-dim.......\n");
 fprintf(vfy,"             particle fall velocity          ft/sec........\n");
 fprintf(vfy,"             stream segment length           feet..........\n");
 fprintf(vfy,"             transport capacity factor       non-dim.......\n");
 fprintf(vfy,"------------------------------------------------------------------------------------------------------------------------------------------------------\n");


 fprintf(vfy,"\f                      VERIFICATION TABLE NO. 2b: Sediment Transport--silt\n");
 fprintf(vfy,"Watershed: %s\n",name);
 fprintf(vfy,"------------------------------------------------------------------------------------------------------------------------------------------------------\n");
 fprintf(vfy," PARTITION                                                                           CELL ID\n");
 fprintf(vfy," NUMBER      VARIABLE                        UNITS         \n");
 fprintf(vfy,"------------------------------------------------------------------------------------------------------------------------------------------------------\n");
 fprintf(vfy," All         constant of proportionality     non-dim.......\n");
 fprintf(vfy,"             particle fall velocity          ft/sec........\n");
 fprintf(vfy,"             stream segment length           feet..........\n");
 fprintf(vfy,"             transport capacity factor       non-dim.......\n");
 fprintf(vfy,"------------------------------------------------------------------------------------------------------------------------------------------------------\n");


 fprintf(vfy,"\f                      VERIFICATION TABLE NO. 2c: Sediment Transport--small aggregate\n");
 fprintf(vfy,"Watershed: %s\n",name);
 fprintf(vfy,"------------------------------------------------------------------------------------------------------------------------------------------------------\n");
 fprintf(vfy," PARTITION                                                                           CELL ID\n");
 fprintf(vfy," NUMBER      VARIABLE                        UNITS         \n");
 fprintf(vfy,"------------------------------------------------------------------------------------------------------------------------------------------------------\n");
 fprintf(vfy," All         constant of proportionality     non-dim.......\n");
 fprintf(vfy,"             particle fall velocity          ft/sec........\n");
 fprintf(vfy,"             stream segment length           feet..........\n");
 fprintf(vfy,"             transport capacity factor       non-dim.......\n");
 fprintf(vfy,"------------------------------------------------------------------------------------------------------------------------------------------------------\n");


 fprintf(vfy,"\f                      VERIFICATION TABLE NO. 2d: Sediment Transport--large aggregate\n");
 fprintf(vfy,"Watershed: %s\n",name);
 fprintf(vfy,"------------------------------------------------------------------------------------------------------------------------------------------------------\n");
 fprintf(vfy," PARTITION                                                                           CELL ID\n");
 fprintf(vfy," NUMBER      VARIABLE                        UNITS         \n");
 fprintf(vfy,"------------------------------------------------------------------------------------------------------------------------------------------------------\n");
 fprintf(vfy," All         constant of proportionality     non-dim.......\n");
 fprintf(vfy,"             particle fall velocity          ft/sec........\n");
 fprintf(vfy,"             stream segment length           feet..........\n");
 fprintf(vfy,"             transport capacity factor       non-dim.......\n");
 fprintf(vfy,"------------------------------------------------------------------------------------------------------------------------------------------------------\n");


 fprintf(vfy,"\f                      VERIFICATION TABLE NO. 2e: Sediment Transport--sand\n");
 fprintf(vfy,"Watershed: %s\n",name);
 fprintf(vfy,"------------------------------------------------------------------------------------------------------------------------------------------------------\n");
 fprintf(vfy," PARTITION                                                                           CELL ID\n");
 fprintf(vfy," NUMBER      VARIABLE                        UNITS         \n");
 fprintf(vfy,"------------------------------------------------------------------------------------------------------------------------------------------------------\n");
 fprintf(vfy," All         constant of proportionality     non-dim.......\n");
 fprintf(vfy,"             particle fall velocity          ft/sec........\n");
 fprintf(vfy,"             stream segment length           feet..........\n");
 fprintf(vfy,"             transport capacity factor       non-dim.......\n");
 fprintf(vfy,"------------------------------------------------------------------------------------------------------------------------------------------------------\n");

 return;


}


#ifdef _UNIX_K_AND_R

void vfinish_2(vfy, part)

     FILE *vfy;
     int part;

#else

void vfinish_2(FILE *vfy,int part)

#endif

{

 char line_buffer[180];
 char temp_buffer[20];
 FILE *temp_fptr;

 int     i = 0;
 int     n = 0;
 int     x = 0;
 int page  = 2;
 time_t  t;

 t=time(NULL);   /* Gets system time for the file footer...     */

 fseek(vfy,0L,SEEK_SET);

 temp_fptr = fopen("tvfy2000.swp","w+");

/*********************************************** Read File Header *********/

 for (i = 0; i < 57; ++i)
  {
   fgets(line_buffer,180,vfy);
   fprintf(temp_fptr,"%s",line_buffer);
  }

 for (x = 0; x < 5; ++x)
  {
   for (n = 1; n <= part; ++n)
    {
     fprintf(temp_fptr,"%2d           unit-wdth H2O dischrg           cfs/ft........\n",n);
     fprintf(temp_fptr,"             unit-wdth trans capacity        lbs/sec/ft....\n");
     fprintf(temp_fptr,"             upstr unit-wdth dischrg         lbs/sec/ft....\n");
     fprintf(temp_fptr,"             dwnstr unit-wdth dischrg        lbs/sec/ft....\n");
     fprintf(temp_fptr,"             deposition number               non-dim.......\n");

     if (n < part)
       fprintf(temp_fptr,"------------------------------------------------------------------------------------------------------------------------------------------------------\n");
     else
       fprintf(temp_fptr,"------------------------------------------------------------------------------------------------------------------------------------------------------\n");

    }

   if (x < 4)
    {
     for (i = 0; i < 11; ++i)
      {
       fgets(line_buffer,180,vfy);
       fprintf(temp_fptr,"%s",line_buffer);
      }
    }
/*   else
    {
     for (i = 0; i < 1; ++i)
      {
       fgets(line_buffer,180,vfy);
       fprintf(temp_fptr,"%s",line_buffer);
      }
    }
*/
  }

 fseek(vfy,0L,SEEK_SET);
 fseek(temp_fptr,0L,SEEK_SET);

 while (fgets(line_buffer,180,temp_fptr) != NULL)
  fprintf(vfy,"%s",line_buffer);

/*********************************************** Clean up after myself ****/

 fclose(temp_fptr);
 remove("tvfy2000.swp");

 return;
}



#ifdef _UNIX_K_AND_R

void vsetup_3(vfy,name)

     FILE *vfy;
     char* name[];

#else

void vsetup_3(FILE *vfy,char* name[])

#endif


{
 int     page = 1;
 time_t  t;

 t=time(NULL);   /* Gets system time for the file footer...     */


 fprintf(vfy,"                      VERIFICATION TABLE NO. 3: Chemicals\n\n");
 fprintf(vfy,"Watershed: %s\n\n",name);
 fprintf(vfy,"------------------------------------------------------------------------------------------------------------------------------------------------------\n");
 fprintf(vfy,"                                                                             CELL ID\n");
 fprintf(vfy," CHEMICAL   GRP         VAR              UNITS        \n");
 fprintf(vfy,"------------------------------------------------------------------------------------------------------------------------------------------------------\n\n");
 fprintf(vfy," Nitrogen   Dissolved   decay factor     %%............\n");
 fprintf(vfy,"                        in from cells    lbs..........\n");
 fprintf(vfy,"                        generated local  lbs..........\n");
 fprintf(vfy,"                        out of cell      lbs..........\n");
 fprintf(vfy,"            Attached    in from cells    lbs..........\n");
 fprintf(vfy,"                        generated local  lbs..........\n");
 fprintf(vfy,"                        out of cell      lbs..........\n");
 fprintf(vfy,"            Total       in from cells    lbs..........\n");
 fprintf(vfy,"                        generated local  lbs..........\n");
 fprintf(vfy,"                        out of cell      lbs..........\n");
 fprintf(vfy,"                        enrichment ratio non-dim......\n");
 fprintf(vfy,"------------------------------------------------------------------------------------------------------------------------------------------------------\n\n");
 fprintf(vfy," Phosphorus Dissolved   decay factor     %%............\n");
 fprintf(vfy,"                        in from cells    lbs..........\n");
 fprintf(vfy,"                        generated local  lbs..........\n");
 fprintf(vfy,"                        out of cell      lbs..........\n");
 fprintf(vfy,"            Attached    in from cells    lbs..........\n");
 fprintf(vfy,"                        generated local  lbs..........\n");
 fprintf(vfy,"                        out of cell      lbs..........\n");
 fprintf(vfy,"            Total       in from cells    lbs..........\n");
 fprintf(vfy,"                        generated local  lbs..........\n");
 fprintf(vfy,"                        out of cell      lbs..........\n");
 fprintf(vfy,"                        enrichment ratio non-dim......\n");
 fprintf(vfy,"------------------------------------------------------------------------------------------------------------------------------------------------------\n\n");
 fprintf(vfy," Pesticides Dissolved   decay factor     %%............\n");
 fprintf(vfy,"                        in from cells    lbs..........\n");
 fprintf(vfy,"                        generated local  lbs..........\n");
 fprintf(vfy,"                        out of cell      lbs..........\n");
 fprintf(vfy,"            Attached    in from cells    lbs..........\n");
 fprintf(vfy,"                        generated local  lbs..........\n");
 fprintf(vfy,"                        out of cell      lbs..........\n");
 fprintf(vfy,"            Total       in from cells    lbs..........\n");
 fprintf(vfy,"                        generated local  lbs..........\n");
 fprintf(vfy,"                        out of cell      lbs..........\n");
 fprintf(vfy,"                        enrichment ratio non-dim......\n");
 fprintf(vfy,"------------------------------------------------------------------------------------------------------------------------------------------------------\n\n");
 fprintf(vfy," COD                    decay factor     %%............\n");
 fprintf(vfy,"                        in from cells    ppm..........\n");
 fprintf(vfy,"                                         lbs..........\n");
 fprintf(vfy,"                        generated local  ppm..........\n");
 fprintf(vfy,"                                         lbs..........\n");
 fprintf(vfy,"                        out of cell      ppm..........\n");
 fprintf(vfy,"                                         lbs..........\n");
 fprintf(vfy,"------------------------------------------------------------------------------------------------------------------------------------------------------\n\n");
 fprintf(vfy,"%s                                                                                                                                              Page %d.",ctime(&t),page+1);

 return;
}





#ifdef _UNIX_K_AND_R

void t1setup_1(htable)

     HYDRO_TABLE_ENTRY *htable;

#else

void t1setup_1(HYDRO_TABLE_ENTRY *htable)

#endif


{

 htable->column_id          = 0;
 htable->page_num           = 0;

 htable->drainage_area      = 0;
 htable->total_cell_area    = 0.0;
 htable->feedlot_area       = 0.0;
 htable->impoundment_area   = 0.0;
 htable->receiving_cell     = 0;
 htable->next_routing_cell  = 0;

 htable->ol_length          = 0.0;
 htable->ol_travel_time     = 0.0;

 htable->sc_length          = 0.0;
 htable->sc_velocity        = 0.0;
 htable->sc_travel_time     = 0.0;

 htable->ch_top_width       = 0.0;
 htable->ch_depth_bankfull  = 0.0;
 htable->ch_seg_length      = 0.0;
 htable->ch_velocity        = 0.0;
 htable->ch_travel_time     = 0.0;
 htable->ch_time_of_conc    = 0.0;

 htable->pk_dis_outlet_cfs  = 0.0;
 htable->pk_dis_outlet_iph  = 0.0;
 htable->tot_ro_outlet_af   = 0.0;
 htable->tot_ro_outlet_in   = 0.0;
 htable->avg_pk_discharge   = 0.0;
 htable->avg_ro_volume      = 0.0;
 htable->ro_pk_volume       = 0.0;
 htable->time_to_pk         = 0.0;
 htable->time_to_base       = 0.0;

 return;
}




#ifdef _UNIX_K_AND_R

void t2setup_2(stable)

     SED_TABLE_ENTRY *stable;

#else

void t2setup_2(SED_TABLE_ENTRY *stable)

#endif



{
 int particle  = 0;
 int partition = 0;

 for (particle = 1; particle <= 6; ++particle)
  {
   stable->column_id        = 0;
   stable->page_num         = 0;
   stable->num_partitions   = 0;

   stable->constant_prop[particle]     = 0.0;
   stable->part_fall_vel[particle]     = 0.0;
   stable->str_seg_length[particle]    = 0.0;
   stable->trans_cap_factor[particle]  = 0.0;

   for (partition = 1; partition <= 8; ++partition)
    {
     stable->water_discharge[partition][particle]    = 0.0;
     stable->sed_trans_cap[partition][particle]      = 0.0;
     stable->up_sed_discharge[partition][particle]   = 0.0;
     stable->down_sed_discharge[partition][particle] = 0.0;
     stable->deposition_number[partition][particle]  = 0.0;
    }
   }

 return;
}


#ifdef _UNIX_K_AND_R

void populate_vfy1(vfy_1, htable)

     FILE *vfy_1;
     HYDRO_TABLE_ENTRY *htable;

#else

void populate_vfy1(FILE *vfy_1,HYDRO_TABLE_ENTRY *htable)

#endif


{
 char line_buffer[180];
 char temp_buffer[20];
 FILE *temp_fptr;

 int  i = 0;
 int     n       = 0;

 fseek(vfy_1,0L,SEEK_SET);

 temp_fptr = fopen("tvfy1001.swp","w+");

/*********************************************** Read File Header *********/

 for (i = 0; i < 6; ++i)
  {
   fgets(line_buffer,180,vfy_1);
   fprintf(temp_fptr,"%s",line_buffer);
  }

/*********************************************** Read Column (Cell) *******/

 fgets(line_buffer,180,vfy_1);
 for (n = 0; line_buffer[n] != '\n'; ++n);

 if (n < 180)
  {
   line_buffer[n] = ('\0');
   sprintf(temp_buffer," %8d \0",htable->column_id);
   strcat(line_buffer,temp_buffer);
  }

 fprintf(temp_fptr,"%s\n",line_buffer);

 for (i = 0; i < 2; ++i)
  {
   fgets(line_buffer,180,vfy_1);
   fprintf(temp_fptr,"%s",line_buffer);
  }

/*********************************************** Read Drainage Area *******/

 fgets(line_buffer,180,vfy_1);
 for (n = 0; line_buffer[n] != '\n'; ++n);

 if (n < 180)
  {
   line_buffer[n] = ('\0');
   sprintf(temp_buffer," %8.0f \0",htable->drainage_area);
   strcat(line_buffer,temp_buffer);
  }

 fprintf(temp_fptr,"%s\n",line_buffer);


/*********************************************** Read Total Cell Area *****/

 fgets(line_buffer,180,vfy_1);
 for (n = 0; line_buffer[n] != '\n'; ++n);

 if (n < 180)
  {
   line_buffer[n] = ('\0');
   sprintf(temp_buffer," %8.3f \0",htable->total_cell_area);
   strcat(line_buffer,temp_buffer);
  }

  fprintf(temp_fptr,"%s\n",line_buffer);


/*********************************************** Read Feedlot Area ********/

 fgets(line_buffer,180,vfy_1);
 for (n = 0; line_buffer[n] != '\n'; ++n);

 if (n < 180)
  {
   line_buffer[n] = ('\0');

   if (htable->feedlot_area > 0.00)
    {
     sprintf(temp_buffer," %8.3f \0",htable->feedlot_area);
     strcat(line_buffer,temp_buffer);
    }
   else
    {
     sprintf(temp_buffer," %8s \0","        ");
     strcat(line_buffer,temp_buffer);
    }
  }

 fprintf(temp_fptr,"%s\n",line_buffer);


/*********************************************** Read Impoundment Area ****/

 fgets(line_buffer,180,vfy_1);
 for (n = 0; line_buffer[n] != '\n'; ++n);

 if (n < 180)
  {
   line_buffer[n] = ('\0');

   if (htable->impoundment_area > 0.00)
    {
     sprintf(temp_buffer," %8.3f \0",htable->impoundment_area);
     strcat(line_buffer,temp_buffer);
    }
   else
    {
     sprintf(temp_buffer," %8s \0","        ");
     strcat(line_buffer,temp_buffer);
    }
  }

 fprintf(temp_fptr,"%s\n",line_buffer);


/*********************************************** Read Receiving Cell ******/

 fgets(line_buffer,180,vfy_1);
 for (n = 0; line_buffer[n] != '\n'; ++n);

 if (n < 180)
  {
   line_buffer[n] = ('\0');
   sprintf(temp_buffer," %8d \0",htable->receiving_cell);
   strcat(line_buffer,temp_buffer);
  }

 fprintf(temp_fptr,"%s\n",line_buffer);


/*********************************************** Read Next Routing Cell ***/

 fgets(line_buffer,180,vfy_1);
 for (n = 0; line_buffer[n] != '\n'; ++n);

 if (n < 180)
  {
   line_buffer[n] = ('\0');

   if (htable->next_routing_cell > 0)
    {
     sprintf(temp_buffer," %8s \0",/*htable->next_routing_cell*/"        ");
     strcat(line_buffer,temp_buffer);
    }
   else
    {
     sprintf(temp_buffer," %8s \0","        ");
     strcat(line_buffer,temp_buffer);
    }
  }

 fprintf(temp_fptr,"%s\n",line_buffer);

 fgets(line_buffer,180,vfy_1);
 fprintf(temp_fptr,"%s",line_buffer);

 fgets(line_buffer,180,vfy_1);
 fprintf(temp_fptr,"%s",line_buffer);


/*********************************************** Read Ovrld Length ********/

 fgets(line_buffer,180,vfy_1);
 for (n = 0; line_buffer[n] != '\n'; ++n);

 if (n < 180)
  {
   line_buffer[n] = ('\0');

   if (htable->ol_length > 0.0)
    {
     sprintf(temp_buffer," %8.2f \0",htable->ol_length);
     strcat(line_buffer,temp_buffer);
    }
   else
    {
     sprintf(temp_buffer," %8s \0","        ");
     strcat(line_buffer,temp_buffer);
    }
  }

 fprintf(temp_fptr,"%s\n",line_buffer);


/*********************************************** Read Ovrld Travel Time ***/

 fgets(line_buffer,180,vfy_1);
 for (n = 0; line_buffer[n] != '\n'; ++n);

 if (n < 180)
  {
   line_buffer[n] = ('\0');

   if (htable->ol_length > 0.0)
    {
     sprintf(temp_buffer," %8.4f \0",htable->ol_travel_time);
     strcat(line_buffer,temp_buffer);
    }
   else
    {
     sprintf(temp_buffer," %8s \0","        ");
     strcat(line_buffer,temp_buffer);
    }
  }
 fprintf(temp_fptr,"%s\n",line_buffer);

 fgets(line_buffer,180,vfy_1);
 fprintf(temp_fptr,"%s",line_buffer);

 fgets(line_buffer,180,vfy_1);
 fprintf(temp_fptr,"%s",line_buffer);


/*********************************************** Read Shallow Length *****/

 fgets(line_buffer,180,vfy_1);
 for (n = 0; line_buffer[n] != '\n'; ++n);

 if (n < 180)
  {
   line_buffer[n] = ('\0');

   if (htable->sc_length > 0.00)
    {
     sprintf(temp_buffer," %8.3f \0",htable->sc_length);
     strcat(line_buffer,temp_buffer);
    }
   else
    {
     sprintf(temp_buffer," %8s \0","        ");
     strcat(line_buffer,temp_buffer);
    }
  }

 fprintf(temp_fptr,"%s\n",line_buffer);


/*********************************************** Read Shallow Velocity ***/

 fgets(line_buffer,180,vfy_1);
 for (n = 0; line_buffer[n] != '\n'; ++n);

 if (n < 180)
  {
   line_buffer[n] = ('\0');

   if (htable->sc_velocity > 0.0)
    {
     sprintf(temp_buffer," %8.4f \0",htable->sc_velocity);
     strcat(line_buffer,temp_buffer);
    }
   else
    {
     sprintf(temp_buffer," %8s \0","        ");
     strcat(line_buffer,temp_buffer);
    }
  }

 fprintf(temp_fptr,"%s\n",line_buffer);


/*********************************************** Read Shallow Trav Time **/

 fgets(line_buffer,180,vfy_1);
 for (n = 0; line_buffer[n] != '\n'; ++n);

 if (n < 180)
  {
   line_buffer[n] = ('\0');

   if (htable->sc_travel_time > 0.0)
    {
     sprintf(temp_buffer," %8.4f \0",htable->sc_travel_time);
     strcat(line_buffer,temp_buffer);
    }
   else
    {
     sprintf(temp_buffer," %8s \0","        ");
     strcat(line_buffer,temp_buffer);
    }
  }

 fprintf(temp_fptr,"%s\n",line_buffer);

 fgets(line_buffer,180,vfy_1);
 fprintf(temp_fptr,"%s",line_buffer);

 fgets(line_buffer,180,vfy_1);
 fprintf(temp_fptr,"%s",line_buffer);


/*********************************************** Read Chan Top Width *****/

 fgets(line_buffer,180,vfy_1);
 for (n = 0; line_buffer[n] != '\n'; ++n);

 if (n < 180)
  {
   line_buffer[n] = ('\0');
   sprintf(temp_buffer," %8.3f \0",htable->ch_top_width);
   strcat(line_buffer,temp_buffer);
  }

 fprintf(temp_fptr,"%s\n",line_buffer);


/*********************************************** Read Chan Dep @ Bnkfl ***/

 fgets(line_buffer,180,vfy_1);
 for (n = 0; line_buffer[n] != '\n'; ++n);

 if (n < 180)
  {
   line_buffer[n] = ('\0');
   sprintf(temp_buffer," %8.4f \0",htable->ch_depth_bankfull);
   strcat(line_buffer,temp_buffer);
  }

 fprintf(temp_fptr,"%s\n",line_buffer);


/*********************************************** Read Chan Segment Lenth */

 fgets(line_buffer,180,vfy_1);
 for (n = 0; line_buffer[n] != '\n'; ++n);

 if (n < 180)
  {
   line_buffer[n] = ('\0');
   sprintf(temp_buffer," %8.2f \0",htable->ch_seg_length);
   strcat(line_buffer,temp_buffer);
  }

 fprintf(temp_fptr,"%s\n",line_buffer);


/*********************************************** Read Chan Velocity ******/

 fgets(line_buffer,180,vfy_1);
 for (n = 0; line_buffer[n] != '\n'; ++n);

 if (n < 180)
  {
   line_buffer[n] = ('\0');
   sprintf(temp_buffer," %8.4f \0",htable->ch_velocity);
   strcat(line_buffer,temp_buffer);
  }

 fprintf(temp_fptr,"%s\n",line_buffer);


/*********************************************** Read Chan Travel Time ***/

 fgets(line_buffer,180,vfy_1);
 for (n = 0; line_buffer[n] != '\n'; ++n);

 if (n < 180)
  {
   line_buffer[n] = ('\0');
   sprintf(temp_buffer," %8.4f \0",htable->ch_travel_time);
   strcat(line_buffer,temp_buffer);
  }

 fprintf(temp_fptr,"%s\n",line_buffer);


/*********************************************** Read Chan Tm/Concen. ***/

 fgets(line_buffer,180,vfy_1);
 for (n = 0; line_buffer[n] != '\n'; ++n);

 if (n < 180)
  {
   line_buffer[n] = ('\0');
   sprintf(temp_buffer," %8.4f \0",htable->ch_time_of_conc);
   strcat(line_buffer,temp_buffer);
  }

 fprintf(temp_fptr,"%s\n",line_buffer);

 fgets(line_buffer,180,vfy_1);
 fprintf(temp_fptr,"%s",line_buffer);

 fgets(line_buffer,180,vfy_1);
 fprintf(temp_fptr,"%s",line_buffer);


/*********************************************** Read Pk Discharge ******/

 fgets(line_buffer,180,vfy_1);
 for (n = 0; line_buffer[n] != '\n'; ++n);

 if (n < 180)
  {
   line_buffer[n] = ('\0');
   sprintf(temp_buffer," %8.2f \0",htable->pk_dis_outlet_cfs);
   strcat(line_buffer,temp_buffer);
  }

 fprintf(temp_fptr,"%s\n",line_buffer);

 fgets(line_buffer,180,vfy_1);
 for (n = 0; line_buffer[n] != '\n'; ++n);

 if (n < 180)
  {
   line_buffer[n] = ('\0');
   sprintf(temp_buffer," %8.4f \0",htable->pk_dis_outlet_iph);
   strcat(line_buffer,temp_buffer);
  }

 fprintf(temp_fptr,"%s\n",line_buffer);


/*********************************************** Read Total Runoff ******/

 fgets(line_buffer,180,vfy_1);
 for (n = 0; line_buffer[n] != '\n'; ++n);

 if (n < 180)
  {
   line_buffer[n] = ('\0');
   sprintf(temp_buffer," %8.4f \0",htable->tot_ro_outlet_af);
   strcat(line_buffer,temp_buffer);
  }

 fprintf(temp_fptr,"%s\n",line_buffer);

 fgets(line_buffer,180,vfy_1);
 for (n = 0; line_buffer[n] != '\n'; ++n);

 if (n < 180)
  {
   line_buffer[n] = ('\0');
   sprintf(temp_buffer," %8.4f \0",htable->tot_ro_outlet_in);
   strcat(line_buffer,temp_buffer);
  }

 fprintf(temp_fptr,"%s\n",line_buffer);


/*********************************************** Read Pk Discharge ******/

 fgets(line_buffer,180,vfy_1);
 for (n = 0; line_buffer[n] != '\n'; ++n);

 if (n < 180)
  {
   line_buffer[n] = ('\0');
   sprintf(temp_buffer," %8.2f \0",htable->avg_pk_discharge);
   strcat(line_buffer,temp_buffer);
  }

 fprintf(temp_fptr,"%s\n",line_buffer);


/*********************************************** Read Runoff Volume *****/

 fgets(line_buffer,180,vfy_1);
 for (n = 0; line_buffer[n] != '\n'; ++n);

 if (n < 180)
  {
   line_buffer[n] = ('\0');
   sprintf(temp_buffer," %8.4f \0",htable->avg_ro_volume);
   strcat(line_buffer,temp_buffer);
  }

 fprintf(temp_fptr,"%s\n",line_buffer);


/*********************************************** Read Runoff Pk Volume **/

 fgets(line_buffer,180,vfy_1);
 for (n = 0; line_buffer[n] != '\n'; ++n);

 if (n < 180)
  {
   line_buffer[n] = ('\0');
   sprintf(temp_buffer," %8.4f \0",htable->ro_pk_volume);
   strcat(line_buffer,temp_buffer);
  }

 fprintf(temp_fptr,"%s\n",line_buffer);


/*********************************************** Read Time To Peak *****/

 fgets(line_buffer,180,vfy_1);
 for (n = 0; line_buffer[n] != '\n'; ++n);

 if (n < 180)
  {
   line_buffer[n] = ('\0');
   sprintf(temp_buffer," %8.4f \0",htable->time_to_pk);
   strcat(line_buffer,temp_buffer);
  }

 fprintf(temp_fptr,"%s\n",line_buffer);


/*********************************************** Read Time To Base *****/

 fgets(line_buffer,180,vfy_1);
 for (n = 0; line_buffer[n] != '\n'; ++n);

 if (n < 180)
  {
   line_buffer[n] = ('\0');
   sprintf(temp_buffer," %8.4f \0",htable->time_to_base);
   strcat(line_buffer,temp_buffer);
  }

 fprintf(temp_fptr,"%s\n",line_buffer);

/*********************************************** Read Remainder of File ***/

 while (fgets(line_buffer,180,vfy_1) != NULL)
  fprintf(temp_fptr,"%s",line_buffer);

/*********************************************** Transfer contents ********/


 fseek(vfy_1,0L,SEEK_SET);
 fseek(temp_fptr,0L,SEEK_SET);

 while (fgets(line_buffer,180,temp_fptr) != NULL)
  fprintf(vfy_1,"%s",line_buffer);

/*********************************************** Clean up after myself ****/

 fclose(temp_fptr);
 remove("tvfy1001.swp");

return;
}



#ifdef _UNIX_K_AND_R

void populate_vfy2(vfy_2, stable)

     FILE *vfy_2;
     SED_TABLE_ENTRY *stable;

#else

void populate_vfy2(FILE *vfy_2,SED_TABLE_ENTRY *stable)

#endif


{
 char line_buffer[180];
 char temp_buffer[20];
 FILE *temp_fptr;

 int  i         = 0;
 int  n         = 0;
 int  particle  = 0;
 int  partition = 0;

 fseek(vfy_2,0L,SEEK_SET);

 temp_fptr = fopen("tvfy2002.swp","w+");

/*********************************************** Read File Header *********/

 for (i = 0; i < 5; ++i)
  {
   fgets(line_buffer,180,vfy_2);
   fprintf(temp_fptr,"%s",line_buffer);
  }

/*********************************************** Read Column (Cell) *******/

 fgets(line_buffer,180,vfy_2);
 for (n = 0; line_buffer[n] != '\n'; ++n);

 if (n < 180)
  {
   line_buffer[n] = ('\0');
   sprintf(temp_buffer," %8d \0",stable->column_id);
   strcat(line_buffer,temp_buffer);
  }

 fprintf(temp_fptr,"%s\n",line_buffer);

 for (i = 0; i < 1; ++i)
  {
   fgets(line_buffer,180,vfy_2);
   fprintf(temp_fptr,"%s",line_buffer);
  }


 /********************************************** LOOP By Particle Class ***/

 for (particle = 1; particle <= 6; ++particle)
  {
   fgets(line_buffer,180,vfy_2);
   for (n = 0; line_buffer[n] != '\n'; ++n);

   if (n < 180)
    {
     line_buffer[n] = ('\0');
     sprintf(temp_buffer," %8.2f \0",stable->up_amount[particle]);
     strcat(line_buffer,temp_buffer);
    }

   fprintf(temp_fptr,"%s\n",line_buffer);


   /******************************************** Read Upstream Concen  ***/

   fgets(line_buffer,180,vfy_2);
   for (n = 0; line_buffer[n] != '\n'; ++n);

   if (n < 180)
    {
     line_buffer[n] = ('\0');
     sprintf(temp_buffer," %8.6f \0",stable->up_conc[particle]);
     strcat(line_buffer,temp_buffer);
    }

   fprintf(temp_fptr,"%s\n",line_buffer);


   /******************************************** Read Total in-cell src **/

   fgets(line_buffer,180,vfy_2);
   for (n = 0; line_buffer[n] != '\n'; ++n);

   if (n < 180)
    {
     line_buffer[n] = ('\0');
     sprintf(temp_buffer," %8.2f \0",stable->tot_in_cell_src[particle]);
     strcat(line_buffer,temp_buffer);
    }

   fprintf(temp_fptr,"%s\n",line_buffer);


   /******************************************** Read Down Amount ********/

   fgets(line_buffer,180,vfy_2);
   for (n = 0; line_buffer[n] != '\n'; ++n);

   if (n < 180)
    {
     line_buffer[n] = ('\0');
     sprintf(temp_buffer," %8.2f \0",stable->down_amount[particle]);
     strcat(line_buffer,temp_buffer);
    }

   fprintf(temp_fptr,"%s\n",line_buffer);


   /******************************************** Read Down Concen *******/

   fgets(line_buffer,180,vfy_2);
   for (n = 0; line_buffer[n] != '\n'; ++n);

   if (n < 180)
    {
     line_buffer[n] = ('\0');
     sprintf(temp_buffer," %8.6f \0",stable->down_conc[particle]);
     strcat(line_buffer,temp_buffer);
    }

   fprintf(temp_fptr,"%s\n",line_buffer);

   fgets(line_buffer,180,vfy_2);            /* On to the next Class...  */
   fprintf(temp_fptr,"%s",line_buffer);

  }

 /********************************************** On to the next page... **/

 for (particle = 1; particle < 6; ++particle)
  {
   if (particle == 1)
     for (i = 0; i < 9; ++i)
      {
       fgets(line_buffer,180,vfy_2);
       fprintf(temp_fptr,"%s",line_buffer);
      }
   else
     for (i = 0; i < 6; ++i)
      {
       fgets(line_buffer,180,vfy_2);
       fprintf(temp_fptr,"%s",line_buffer);
      }

   /******************************************** Read Prop Constant ******/

   fgets(line_buffer,180,vfy_2);
   for (n = 0; line_buffer[n] != '\n'; ++n);

   if (n < 180)
    {
     line_buffer[n] = ('\0');
     sprintf(temp_buffer," %8.2f \0",stable->constant_prop[particle]);
     strcat(line_buffer,temp_buffer);
    }

   fprintf(temp_fptr,"%s\n",line_buffer);


   /******************************************** Read Part Fall velocity */

   fgets(line_buffer,180,vfy_2);
   for (n = 0; line_buffer[n] != '\n'; ++n);

   if (n < 180)
    {
     line_buffer[n] = ('\0');
     sprintf(temp_buffer," %8.2e \0",stable->part_fall_vel[particle]);
     strcat(line_buffer,temp_buffer);
    }

   fprintf(temp_fptr,"%s\n",line_buffer);


   /******************************************** Read Stream Seg length **/

   fgets(line_buffer,180,vfy_2);
   for (n = 0; line_buffer[n] != '\n'; ++n);

   if (n < 180)
    {
     line_buffer[n] = ('\0');
     sprintf(temp_buffer," %8.3f \0",stable->str_seg_length[particle]);
     strcat(line_buffer,temp_buffer);
    }

   fprintf(temp_fptr,"%s\n",line_buffer);



   /******************************************** Read Transprt Cap fact **/

   fgets(line_buffer,180,vfy_2);
   for (n = 0; line_buffer[n] != '\n'; ++n);

   if (n < 180)
    {
     line_buffer[n] = ('\0');
     sprintf(temp_buffer," %8.6f \0",stable->trans_cap_factor[particle]);
     strcat(line_buffer,temp_buffer);
    }

   fprintf(temp_fptr,"%s\n",line_buffer);

   fgets(line_buffer,180,vfy_2);
   fprintf(temp_fptr,"%s",line_buffer);

   /******************************************** Read By Partition ******/

   for (partition = 1; partition <= stable->num_partitions; ++partition)
    {
     fgets(line_buffer,180,vfy_2);
     for (n = 0; line_buffer[n] != '\n'; ++n);

     if (n < 180)
      {
       line_buffer[n] = ('\0');
       sprintf(temp_buffer," %8.3f \0",stable->water_discharge[partition][particle]);
       strcat(line_buffer,temp_buffer);
      }

     fprintf(temp_fptr,"%s\n",line_buffer);


     /****************************************** Read Sediment trans cap */

     fgets(line_buffer,180,vfy_2);
     for (n = 0; line_buffer[n] != '\n'; ++n);

     if (n < 180)
      {
       line_buffer[n] = ('\0');
       sprintf(temp_buffer," %8.3f \0",stable->sed_trans_cap[partition][particle]);
       strcat(line_buffer,temp_buffer);
      }

     fprintf(temp_fptr,"%s\n",line_buffer);


     /****************************************** Read Sediment up dischg */

     fgets(line_buffer,180,vfy_2);
     for (n = 0; line_buffer[n] != '\n'; ++n);

     if (n < 180)
      {
       line_buffer[n] = ('\0');
       sprintf(temp_buffer," %8.3f \0",stable->up_sed_discharge[partition][particle]);
       strcat(line_buffer,temp_buffer);
      }

     fprintf(temp_fptr,"%s\n",line_buffer);



     /****************************************** Read Sed down dischg ****/

     fgets(line_buffer,180,vfy_2);
     for (n = 0; line_buffer[n] != '\n'; ++n);

     if (n < 180)
      {
       line_buffer[n] = ('\0');
       sprintf(temp_buffer," %8.3f \0",stable->down_sed_discharge[partition][particle]);
       strcat(line_buffer,temp_buffer);
      }

     fprintf(temp_fptr,"%s\n",line_buffer);



     /****************************************** Read Deposition Number **/

     fgets(line_buffer,180,vfy_2);
     for (n = 0; line_buffer[n] != '\n'; ++n);

     if (n < 180)
      {
       line_buffer[n] = ('\0');
       sprintf(temp_buffer," %8.6f \0",stable->deposition_number[partition][particle]);
       strcat(line_buffer,temp_buffer);
      }

     fprintf(temp_fptr,"%s\n",line_buffer);

     fgets(line_buffer,180,vfy_2);
     fprintf(temp_fptr,"%s",line_buffer);



  }  /* End Partition loop.     */
 }   /* End Particle type loop. */


  /********************************************* Read Remainder of File ***/

  while (fgets(line_buffer,180,vfy_2) != NULL)
    fprintf(temp_fptr,"%s",line_buffer);

  /********************************************* Transfer contents ********/


  fseek(vfy_2,0L,SEEK_SET);
  fseek(temp_fptr,0L,SEEK_SET);

  while (fgets(line_buffer,180,temp_fptr) != NULL)
   fprintf(vfy_2,"%s",line_buffer);

  /********************************************* Clean up after myself ****/

  fclose(temp_fptr);
  remove("tvfy2002.swp");

return;
}




#ifdef _UNIX_K_AND_R

void t3setup_3(ctable)

     CHEM_TABLE_ENTRY *ctable;

#else

void t3setup_3(CHEM_TABLE_ENTRY *ctable)

#endif


{

 ctable->column_id = 0;     /* Column number identification.               */
 ctable->page_num = 0;      /* Area of basic cell.                         */

 ctable->N_dis_decay_factor = 0.0; /* N dissolved decay factor.            */
 ctable->N_dis_into  = 0.0; /* Nitrogen dissolved in from draining cells.  */
 ctable->N_dis_local = 0.0; /* N dissolved generated within local cell.    */
 ctable->N_dis_out   = 0.0; /* Nitrogen dissolved out of cell.             */

 ctable->N_att_into  = 0.0; /* Nitrogen attached in from draining cells.   */
 ctable->N_att_local = 0.0; /* Nitrogen attached generated within local.   */
 ctable->N_att_out   = 0.0; /* Nitrogen attached out of cell.              */

 ctable->N_tot_into  = 0.0; /* Nitrogen total in from draining cells.      */
 ctable->N_tot_local = 0.0; /* Nitrogen total generated within local cell. */
 ctable->N_tot_out   = 0.0; /* Nitrogen total out of cell.                 */
 ctable->N_tot_enrichment = 0.0;  /* N total enrichment ratio.             */


 ctable->P_dis_decay_factor = 0.0;/* P dissolved decay factor.             */
 ctable->P_dis_into  = 0.0; /* Phosphorus dissolved in from draining cells.*/
 ctable->P_dis_local = 0.0; /* P dissolved generated within local cell.    */
 ctable->P_dis_out   = 0.0; /* Phosphorus dissolved out of cell.           */

 ctable->P_att_into  = 0.0; /* Phosphorus attached in from draining cells. */
 ctable->P_att_local = 0.0; /* Phosphorus attached generated within local. */
 ctable->P_att_out   = 0.0; /* Phosphorus attached out of cell.            */

 ctable->P_tot_into  = 0.0; /* Phosphorus total in from draining cells.    */
 ctable->P_tot_local = 0.0; /* Phosphorus total generated in local cell.   */
 ctable->P_tot_out   = 0.0; /* Phosphorus total out of cell.               */
 ctable->P_tot_enrichment = 0.0;  /* P total enrichment ratio.             */


 ctable->Pest_dis_decay_fct = 0.0;/* Pest dissolved decay factor.          */
 ctable->Pest_dis_into  = 0.0;    /* Pest dissolved in from draining cells.*/
 ctable->Pest_dis_local = 0.0;    /* Pest dissolved generated in lcl cell. */
 ctable->Pest_dis_out   = 0.0;    /* Pest dissolved out of cell.           */

 ctable->Pest_att_into  = 0.0;    /* Pest attached in from draining cells. */
 ctable->Pest_att_local = 0.0;    /* Pest attached generated within local. */
 ctable->Pest_att_out   = 0.0;    /* Pest attached out of cell.            */

 ctable->Pest_tot_into  = 0.0;    /* Pest total in from draining cells.    */
 ctable->Pest_tot_local = 0.0;    /* Pest total generated within lcl cell. */
 ctable->Pest_tot_out   = 0.0;    /* Pest total out of cell.               */
 ctable->Pest_tot_enrich= 0.0;    /* Pest total enrichment ratio.          */


 ctable->COD_decay_fct = 0.0;     /* Chem Oxygen Demand decay factor.      */
 ctable->COD_into_ppm  = 0.0;     /* Chem Oxygen Demand in from drain cels.*/
 ctable->COD_into_lbs  = 0.0;     /* Chem Oxygen Demand in from drain cels.*/
 ctable->COD_local_ppm = 0.0;     /* Chem Oxygen Demand in local cell.     */
 ctable->COD_local_lbs = 0.0;     /* Chem Oxygen Demand in local cell.     */
 ctable->COD_out_ppm   = 0.0;     /* Chem Oxygen Demand out of cell.       */
 ctable->COD_out_lbs   = 0.0;     /* Chem Oxygen Demand out of cell.       */

 return;
}




#ifdef _UNIX_K_AND_R

void populate_vfy3(vfy_3, ctable)

     FILE *vfy_3;
     CHEM_TABLE_ENTRY *ctable;

#else

void populate_vfy3(FILE *vfy_3,CHEM_TABLE_ENTRY *ctable)

#endif


{
 char line_buffer[180];
 char temp_buffer[20];
 FILE *temp_fptr;

 int  i = 0;
 int  n = 0;

 fseek(vfy_3,0L,SEEK_SET);

 temp_fptr = fopen("tvfy3001.swp","w+");

/*********************************************** Read File Header *********/

 for (i = 0; i < 6; ++i)
  {
   fgets(line_buffer,180,vfy_3);
   fprintf(temp_fptr,"%s",line_buffer);
  }

/*********************************************** Read Column (Cell) *******/

 fgets(line_buffer,180,vfy_3);
 for (n = 0; line_buffer[n] != '\n'; ++n);

 if (n < 180)
  {
   line_buffer[n] = ('\0');
   sprintf(temp_buffer," %8d \0",ctable->column_id);
   strcat(line_buffer,temp_buffer);
  }

 fprintf(temp_fptr,"%s\n",line_buffer);

 for (i = 0; i < 2; ++i)
  {
   fgets(line_buffer,180,vfy_3);
   fprintf(temp_fptr,"%s",line_buffer);
  }

/*********************************************** Read N Dis decay fact ****/

 fgets(line_buffer,180,vfy_3);
 for (n = 0; line_buffer[n] != '\n'; ++n);

 if (n < 180)
  {
   line_buffer[n] = ('\0');
   sprintf(temp_buffer," %8.3f \0",ctable->N_dis_decay_factor);
   strcat(line_buffer,temp_buffer);
  }

 fprintf(temp_fptr,"%s\n",line_buffer);


/*********************************************** Read N Dis into *********/

 fgets(line_buffer,180,vfy_3);
 for (n = 0; line_buffer[n] != '\n'; ++n);

 if (n < 180)
  {
   line_buffer[n] = ('\0');
   sprintf(temp_buffer," %8.3f \0",ctable->N_dis_into);
   strcat(line_buffer,temp_buffer);
  }

  fprintf(temp_fptr,"%s\n",line_buffer);


/*********************************************** Read N Dis local ********/

 fgets(line_buffer,180,vfy_3);
 for (n = 0; line_buffer[n] != '\n'; ++n);

 if (n < 180)
  {
   line_buffer[n] = ('\0');

   sprintf(temp_buffer," %8.3f \0",ctable->N_dis_local);
   strcat(line_buffer,temp_buffer);
  }

 fprintf(temp_fptr,"%s\n",line_buffer);


/*********************************************** Read N Dis out *********/

 fgets(line_buffer,180,vfy_3);
 for (n = 0; line_buffer[n] != '\n'; ++n);

 if (n < 180)
  {
   line_buffer[n] = ('\0');

   sprintf(temp_buffer," %8.3f \0",ctable->N_dis_out);
   strcat(line_buffer,temp_buffer);
  }

 fprintf(temp_fptr,"%s\n",line_buffer);


/*********************************************** Read N Att into ******/

 fgets(line_buffer,180,vfy_3);
 for (n = 0; line_buffer[n] != '\n'; ++n);

 if (n < 180)
  {
   line_buffer[n] = ('\0');
   sprintf(temp_buffer," %8.3f \0",ctable->N_att_into);
   strcat(line_buffer,temp_buffer);
  }

 fprintf(temp_fptr,"%s\n",line_buffer);


/*********************************************** Read N Att local *****/

 fgets(line_buffer,180,vfy_3);
 for (n = 0; line_buffer[n] != '\n'; ++n);

 if (n < 180)
  {
   line_buffer[n] = ('\0');

   sprintf(temp_buffer," %8.3f \0",ctable->N_att_local);
   strcat(line_buffer,temp_buffer);
  }

 fprintf(temp_fptr,"%s\n",line_buffer);


/*********************************************** Read N Att out *******/

 fgets(line_buffer,180,vfy_3);
 for (n = 0; line_buffer[n] != '\n'; ++n);

 if (n < 180)
  {
   line_buffer[n] = ('\0');

   sprintf(temp_buffer," %8.3f \0",ctable->N_att_out);
   strcat(line_buffer,temp_buffer);
  }

 fprintf(temp_fptr,"%s\n",line_buffer);


/*********************************************** Read N Tot into ***/

 fgets(line_buffer,180,vfy_3);
 for (n = 0; line_buffer[n] != '\n'; ++n);

 if (n < 180)
  {
   line_buffer[n] = ('\0');

   sprintf(temp_buffer," %8.3f \0",ctable->N_tot_into);
   strcat(line_buffer,temp_buffer);
  }
 fprintf(temp_fptr,"%s\n",line_buffer);

/*********************************************** Read N Tot local *****/

 fgets(line_buffer,180,vfy_3);
 for (n = 0; line_buffer[n] != '\n'; ++n);

 if (n < 180)
  {
   line_buffer[n] = ('\0');

   sprintf(temp_buffer," %8.3f \0",ctable->N_tot_local);
   strcat(line_buffer,temp_buffer);
  }

 fprintf(temp_fptr,"%s\n",line_buffer);


/*********************************************** Read N Tot out ******/

 fgets(line_buffer,180,vfy_3);
 for (n = 0; line_buffer[n] != '\n'; ++n);

 if (n < 180)
  {
   line_buffer[n] = ('\0');

   sprintf(temp_buffer," %8.3f \0",ctable->N_tot_out);
   strcat(line_buffer,temp_buffer);
  }

 fprintf(temp_fptr,"%s\n",line_buffer);


/*********************************************** Read N enrichmt *****/

 fgets(line_buffer,180,vfy_3);
 for (n = 0; line_buffer[n] != '\n'; ++n);

 if (n < 180)
  {
   line_buffer[n] = ('\0');

   sprintf(temp_buffer," %8.3f \0",ctable->N_tot_enrichment);
   strcat(line_buffer,temp_buffer);
  }

 fprintf(temp_fptr,"%s\n",line_buffer);

 fgets(line_buffer,180,vfy_3);
 fprintf(temp_fptr,"%s",line_buffer);

 fgets(line_buffer,180,vfy_3);
 fprintf(temp_fptr,"%s",line_buffer);


/*********************************************** Read P Dis decay fact ****/

 fgets(line_buffer,180,vfy_3);
 for (n = 0; line_buffer[n] != '\n'; ++n);

 if (n < 180)
  {
   line_buffer[n] = ('\0');
   sprintf(temp_buffer," %8.3f \0",ctable->P_dis_decay_factor);
   strcat(line_buffer,temp_buffer);
  }

 fprintf(temp_fptr,"%s\n",line_buffer);


/*********************************************** Read P Dis into *********/

 fgets(line_buffer,180,vfy_3);
 for (n = 0; line_buffer[n] != '\n'; ++n);

 if (n < 180)
  {
   line_buffer[n] = ('\0');
   sprintf(temp_buffer," %8.3f \0",ctable->P_dis_into);
   strcat(line_buffer,temp_buffer);
  }

  fprintf(temp_fptr,"%s\n",line_buffer);


/*********************************************** Read P Dis local ********/

 fgets(line_buffer,180,vfy_3);
 for (n = 0; line_buffer[n] != '\n'; ++n);

 if (n < 180)
  {
   line_buffer[n] = ('\0');

   sprintf(temp_buffer," %8.3f \0",ctable->P_dis_local);
   strcat(line_buffer,temp_buffer);
  }

 fprintf(temp_fptr,"%s\n",line_buffer);


/*********************************************** Read P Dis out *********/

 fgets(line_buffer,180,vfy_3);
 for (n = 0; line_buffer[n] != '\n'; ++n);

 if (n < 180)
  {
   line_buffer[n] = ('\0');

   sprintf(temp_buffer," %8.3f \0",ctable->P_dis_out);
   strcat(line_buffer,temp_buffer);
  }

 fprintf(temp_fptr,"%s\n",line_buffer);


/*********************************************** Read P Att into ******/

 fgets(line_buffer,180,vfy_3);
 for (n = 0; line_buffer[n] != '\n'; ++n);

 if (n < 180)
  {
   line_buffer[n] = ('\0');
   sprintf(temp_buffer," %8.3f \0",ctable->P_att_into);
   strcat(line_buffer,temp_buffer);
  }

 fprintf(temp_fptr,"%s\n",line_buffer);


/*********************************************** Read P Att local *****/

 fgets(line_buffer,180,vfy_3);
 for (n = 0; line_buffer[n] != '\n'; ++n);

 if (n < 180)
  {
   line_buffer[n] = ('\0');

   sprintf(temp_buffer," %8.3f \0",ctable->P_att_local);
   strcat(line_buffer,temp_buffer);
  }

 fprintf(temp_fptr,"%s\n",line_buffer);


/*********************************************** Read P Att out *******/

 fgets(line_buffer,180,vfy_3);
 for (n = 0; line_buffer[n] != '\n'; ++n);

 if (n < 180)
  {
   line_buffer[n] = ('\0');

   sprintf(temp_buffer," %8.3f \0",ctable->P_att_out);
   strcat(line_buffer,temp_buffer);
  }

 fprintf(temp_fptr,"%s\n",line_buffer);


/*********************************************** Read P Tot into ***/

 fgets(line_buffer,180,vfy_3);
 for (n = 0; line_buffer[n] != '\n'; ++n);

 if (n < 180)
  {
   line_buffer[n] = ('\0');

   sprintf(temp_buffer," %8.3f \0",ctable->P_tot_into);
   strcat(line_buffer,temp_buffer);
  }
 fprintf(temp_fptr,"%s\n",line_buffer);

/*********************************************** Read P Tot local *****/

 fgets(line_buffer,180,vfy_3);
 for (n = 0; line_buffer[n] != '\n'; ++n);

 if (n < 180)
  {
   line_buffer[n] = ('\0');

   sprintf(temp_buffer," %8.3f \0",ctable->P_tot_local);
   strcat(line_buffer,temp_buffer);
  }

 fprintf(temp_fptr,"%s\n",line_buffer);


/*********************************************** Read P Tot out ******/

 fgets(line_buffer,180,vfy_3);
 for (n = 0; line_buffer[n] != '\n'; ++n);

 if (n < 180)
  {
   line_buffer[n] = ('\0');

   sprintf(temp_buffer," %8.3f \0",ctable->P_tot_out);
   strcat(line_buffer,temp_buffer);
  }

 fprintf(temp_fptr,"%s\n",line_buffer);


/*********************************************** Read P enrichmt *****/

 fgets(line_buffer,180,vfy_3);
 for (n = 0; line_buffer[n] != '\n'; ++n);

 if (n < 180)
  {
   line_buffer[n] = ('\0');

   sprintf(temp_buffer," %8.3f \0",ctable->P_tot_enrichment);
   strcat(line_buffer,temp_buffer);
  }

 fprintf(temp_fptr,"%s\n",line_buffer);

 fgets(line_buffer,180,vfy_3);
 fprintf(temp_fptr,"%s",line_buffer);

 fgets(line_buffer,180,vfy_3);
 fprintf(temp_fptr,"%s",line_buffer);


/*********************************************** Read Pest Dis decay fact */

 fgets(line_buffer,180,vfy_3);
 for (n = 0; line_buffer[n] != '\n'; ++n);

 if (n < 180)
  {
   line_buffer[n] = ('\0');
   sprintf(temp_buffer," %8s \0","   n/a  ");
   strcat(line_buffer,temp_buffer);
  }

 fprintf(temp_fptr,"%s\n",line_buffer);


/*********************************************** Read Pest Dis into *******/

 fgets(line_buffer,180,vfy_3);
 for (n = 0; line_buffer[n] != '\n'; ++n);

 if (n < 180)
  {
   line_buffer[n] = ('\0');
   sprintf(temp_buffer," %8.3f \0",ctable->Pest_dis_into);
   strcat(line_buffer,temp_buffer);
  }

  fprintf(temp_fptr,"%s\n",line_buffer);


/*********************************************** Read Pest Dis local *****/

 fgets(line_buffer,180,vfy_3);
 for (n = 0; line_buffer[n] != '\n'; ++n);

 if (n < 180)
  {
   line_buffer[n] = ('\0');

   sprintf(temp_buffer," %8.3f \0",ctable->Pest_dis_local);
   strcat(line_buffer,temp_buffer);
  }

 fprintf(temp_fptr,"%s\n",line_buffer);


/*********************************************** Read Pest Dis out ******/

 fgets(line_buffer,180,vfy_3);
 for (n = 0; line_buffer[n] != '\n'; ++n);

 if (n < 180)
  {
   line_buffer[n] = ('\0');

   sprintf(temp_buffer," %8.3f \0",ctable->Pest_dis_out);
   strcat(line_buffer,temp_buffer);
  }

 fprintf(temp_fptr,"%s\n",line_buffer);


/*********************************************** Read Pest Att into ***/

 fgets(line_buffer,180,vfy_3);
 for (n = 0; line_buffer[n] != '\n'; ++n);

 if (n < 180)
  {
   line_buffer[n] = ('\0');
   sprintf(temp_buffer," %8.3f \0",ctable->Pest_att_into);
   strcat(line_buffer,temp_buffer);
  }

 fprintf(temp_fptr,"%s\n",line_buffer);


/*********************************************** Read Pest Att local **/

 fgets(line_buffer,180,vfy_3);
 for (n = 0; line_buffer[n] != '\n'; ++n);

 if (n < 180)
  {
   line_buffer[n] = ('\0');

   sprintf(temp_buffer," %8.3f \0",ctable->Pest_att_local);
   strcat(line_buffer,temp_buffer);
  }

 fprintf(temp_fptr,"%s\n",line_buffer);


/*********************************************** Read Pest Att out ****/

 fgets(line_buffer,180,vfy_3);
 for (n = 0; line_buffer[n] != '\n'; ++n);

 if (n < 180)
  {
   line_buffer[n] = ('\0');

   sprintf(temp_buffer," %8.3f \0",ctable->Pest_att_out);
   strcat(line_buffer,temp_buffer);
  }

 fprintf(temp_fptr,"%s\n",line_buffer);


/*********************************************** Read Pest Tot into ***/

 fgets(line_buffer,180,vfy_3);
 for (n = 0; line_buffer[n] != '\n'; ++n);

 if (n < 180)
  {
   line_buffer[n] = ('\0');

   sprintf(temp_buffer," %8.3f \0",ctable->Pest_tot_into);
   strcat(line_buffer,temp_buffer);
  }
 fprintf(temp_fptr,"%s\n",line_buffer);

/*********************************************** Read Pest Tot local **/

 fgets(line_buffer,180,vfy_3);
 for (n = 0; line_buffer[n] != '\n'; ++n);

 if (n < 180)
  {
   line_buffer[n] = ('\0');

   sprintf(temp_buffer," %8.3f \0",ctable->Pest_tot_local);
   strcat(line_buffer,temp_buffer);
  }

 fprintf(temp_fptr,"%s\n",line_buffer);


/*********************************************** Read Pest Tot out ****/

 fgets(line_buffer,180,vfy_3);
 for (n = 0; line_buffer[n] != '\n'; ++n);

 if (n < 180)
  {
   line_buffer[n] = ('\0');

   sprintf(temp_buffer," %8.3f \0",ctable->Pest_tot_out);
   strcat(line_buffer,temp_buffer);
  }

 fprintf(temp_fptr,"%s\n",line_buffer);


/*********************************************** Read Pest enrichmt ***/

 fgets(line_buffer,180,vfy_3);
 for (n = 0; line_buffer[n] != '\n'; ++n);

 if (n < 180)
  {
   line_buffer[n] = ('\0');

   sprintf(temp_buffer," %8.3f \0",ctable->Pest_tot_enrich);
   strcat(line_buffer,temp_buffer);
  }

 fprintf(temp_fptr,"%s\n",line_buffer);

 fgets(line_buffer,180,vfy_3);
 fprintf(temp_fptr,"%s",line_buffer);

 fgets(line_buffer,180,vfy_3);
 fprintf(temp_fptr,"%s",line_buffer);


/*********************************************** Read COD decay fact ******/

 fgets(line_buffer,180,vfy_3);
 for (n = 0; line_buffer[n] != '\n'; ++n);

 if (n < 180)
  {
   line_buffer[n] = ('\0');
   sprintf(temp_buffer," %8.3f \0",ctable->COD_decay_fct);
   strcat(line_buffer,temp_buffer);
  }

 fprintf(temp_fptr,"%s\n",line_buffer);


/*********************************************** Read COD into **********/

 fgets(line_buffer,180,vfy_3);
 for (n = 0; line_buffer[n] != '\n'; ++n);

 if (n < 180)
  {
   line_buffer[n] = ('\0');
   sprintf(temp_buffer," %8.3f \0",ctable->COD_into_ppm);
   strcat(line_buffer,temp_buffer);
  }

  fprintf(temp_fptr,"%s\n",line_buffer);

 fgets(line_buffer,180,vfy_3);
 for (n = 0; line_buffer[n] != '\n'; ++n);

 if (n < 180)
  {
   line_buffer[n] = ('\0');
   sprintf(temp_buffer," %8.0f \0",ctable->COD_into_lbs);
   strcat(line_buffer,temp_buffer);
  }

  fprintf(temp_fptr,"%s\n",line_buffer);


/*********************************************** Read COD local *******/

 fgets(line_buffer,180,vfy_3);
 for (n = 0; line_buffer[n] != '\n'; ++n);

 if (n < 180)
  {
   line_buffer[n] = ('\0');

   sprintf(temp_buffer," %8.3f \0",ctable->COD_local_ppm);
   strcat(line_buffer,temp_buffer);
  }

 fprintf(temp_fptr,"%s\n",line_buffer);

 fgets(line_buffer,180,vfy_3);
 for (n = 0; line_buffer[n] != '\n'; ++n);

 if (n < 180)
  {
   line_buffer[n] = ('\0');

   sprintf(temp_buffer," %8.0f \0",ctable->COD_local_lbs);
   strcat(line_buffer,temp_buffer);
  }

 fprintf(temp_fptr,"%s\n",line_buffer);

/*********************************************** Read COD out ******/

 fgets(line_buffer,180,vfy_3);
 for (n = 0; line_buffer[n] != '\n'; ++n);

 if (n < 180)
  {
   line_buffer[n] = ('\0');

   sprintf(temp_buffer," %8.3f \0",ctable->COD_out_ppm);
   strcat(line_buffer,temp_buffer);
  }

 fprintf(temp_fptr,"%s\n",line_buffer);

 fgets(line_buffer,180,vfy_3);
 for (n = 0; line_buffer[n] != '\n'; ++n);

 if (n < 180)
  {
   line_buffer[n] = ('\0');

   sprintf(temp_buffer," %8.0f \0",ctable->COD_out_lbs);
   strcat(line_buffer,temp_buffer);
  }

 fprintf(temp_fptr,"%s\n",line_buffer);


/*********************************************** Read Remainder of File ***/

 while (fgets(line_buffer,180,vfy_3) != NULL)
  fprintf(temp_fptr,"%s",line_buffer);

/*********************************************** Transfer contents ********/


 fseek(vfy_3,0L,SEEK_SET);
 fseek(temp_fptr,0L,SEEK_SET);

 while (fgets(line_buffer,180,temp_fptr) != NULL)
  fprintf(vfy_3,"%s",line_buffer);

/*********************************************** Clean up after myself ****/




 fclose(temp_fptr);
 remove("tvfy3001.swp");

return;
}

