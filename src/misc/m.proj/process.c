#include "geo.h"
#include "gis.h"
#include "projects.h"

/* retuens 1 if projection info changed, o otherwise */

int process(in,parms,proj_name,proj_title,ellps_name,radius,USED,units) 
 int in;
 char *parms;
 char *proj_name;
 char *proj_title;
 char *ellps_name;
 double radius;
 struct used_opt *USED;
 char *units;
{
 int proj_changed = 0;
 int proj_index;
 int sph, index_prev = -1;
 double a,es;
 double unit_fact;
 int i, npr, stat;
 char io[100];
 char old_units[100];


 sprintf(old_units,"%s",units);
 G_strip(old_units);
 if (in == 1) sprintf(io,"Input");
 else sprintf(io,"Output");


 proj_changed = 0;
 if (strlen(proj_name) == 0 || strncmp(proj_name,"None",4) == 0)
 {
   proj_changed = 1;
   fprintf(stderr,"\n\nInitializing %s projection:\n",io);
   if (G_ask_proj_name(proj_name,proj_title) < 0) exit(0);
 }
 else
 {
   fprintf(stderr,"\n Last %s projection used was \"%s\", do you want to change it (y/[n]) ? ", io, proj_name);
   gets(answer);
   if ((strlen(answer) != 0) && *answer != 'N' && *answer != 'n')
   {
     proj_changed = 1;
     fprintf(stderr,"\n\nInitializing %s projection:\n",io);
     if (G_ask_proj_name(proj_name,proj_title) < 0) exit(0);
   }
 }


 if ((strncmp(proj_name,"stp",3) != 0) && 
       (strncmp(proj_name,"ll",2) != 0))  
 {
   if (strncmp(ellps_name,"None",4) == 0 || strlen(ellps_name) == 0)
   {
     proj_changed = 1;
     fprintf(stderr,"\n\nInitializing %s projection:\n",io);
     sph = G_ask_ellipse_name(ellps_name);
     if (sph  < 0) exit(0); 
     if (sph == 2) { 
       answer[0] = '\0';
       for (;;) {
         fprintf(stderr,"\nEnter radius for the sphere in meters (%.10lf):",RADIUS_DEF);
         while (!G_gets(answer)) 
           fprintf(stderr,"\nEnter radius for the sphere in meters (%.10lf):",RADIUS_DEF);
         if (strlen(answer) == 0) {
           radius = RADIUS_DEF;
           break;
         }
         if(sscanf(answer,"%lf",radius)!=1) fprintf(stderr,"\nInvalid Entry\n");
         else break;
       }
     }
   }
   else
   {
     fprintf(stderr,"\n Last %s spheroid used was \"%s\", do you want to change it (y/[n]) ? ",io, ellps_name);
     gets(answer);
     if ((strlen(answer) != 0) && *answer != 'N' && *answer != 'n')
     {
       proj_changed = 1;
       fprintf(stderr,"\n\nInitializing %s projection:\n",io);
       sph = G_ask_ellipse_name(ellps_name);
       if (sph < 0) exit(0); 
       if (sph == 2) {
         answer[0] = '\0';
         for (;;) {
           fprintf(stderr,"\nEnter radius for the sphere in meters (%.10lf):",RADIUS_DEF);
           while (!G_gets(answer)) 
             fprintf(stderr,"\nEnter radius for the sphere in meters (%.10lf):",RADIUS_DEF);
           if (strlen(answer) == 0) {
             radius = RADIUS_DEF;
             break;
           }
           if(sscanf(answer,"%lf",radius)!=1) fprintf(stderr,"\nInvalid Entry\n");
           else break;
         }
       }
     }
     if (sph == 2) {
       answer[0] = '\0';
       fprintf(stderr,"\nRadius for the sphere was %.10lf, do you want to change it (y/[n]) ? ", radius);
       gets(answer);
       if ((strlen(answer) != 0) && *answer != 'N' && *answer != 'n')
       {
         for (;;) {
           fprintf(stderr,"\nEnter radius for the sphere in meters (%.10lf):",RADIUS_DEF);
           while (!G_gets(answer)) 
             fprintf(stderr,"\nEnter radius for the sphere in meters (%.10lf):",RADIUS_DEF);
           if (strlen(answer) == 0) {
             radius = RADIUS_DEF;
             break;
           }
           if(sscanf(answer,"%lf",radius)!=1) fprintf(stderr,"\nInvalid Entry\n");
           else break;
         } 
       }
     }
   }
 }

 if(index_prev!=-1) index_prev = proj_index;
 proj_index = get_proj_index(proj_name);
 if (proj_index < 0) {
   sprintf(buff,"projection %s is not specified in the table", proj_name);
   G_fatal_error(buff);
 }
 
 if (proj_index == STP)
   sprintf(units,"feet");
 else
   sprintf(units,"meters");

 parms[0] = '\0';
 switch (proj_index) 
 { 
  case STP:
    fprintf(stderr,"\n%s Projection:\n ",io);
    get_stp_proj(parms);
    proj_changed = 1;
    break;
      
  case LL:
    sprintf(parms,"+proj=%s",proj_name);
    break;

  default:  
    if (sph != 2) {
      G_strip(ellps_name); 
      if (G_get_ellipsoid_by_name(ellps_name,&a,&es) == 0) {
        sprintf(buff,"Invalid %s ellipsoid %s", io, ellps_name);
        G_fatal_error(buff);
      }
      sprintf(parms,"+proj=%s +a=%.10lf +es=%.10lf",proj_name,a,es);
    }
    else
      sprintf(parms,"+proj=%s +ellps=sphere +a=%.10lf +es=0.0",proj_name, radius);

    for (i=0; i < NOPTIONS; i++) {
      if (TABLE[proj_index][i].ask == 1) {
        if (i == SOUTH)  {
          fprintf(stderr,"\n%s Projection:  Would you like to use %s (y/[n]) ? ", io, DESC[i]);
          gets(answer);
          proj_changed = 1;
        }
        else {
          if ((USED[i].was == 1)&&(index_prev!=proj_index)) {
            if (i==ZONE) {
              fprintf(stderr,"\n%s Projection:  Last %s used was %d\n\t\tdo you want to change it (y/[n]) ? ", io, DESC[i], (int)(USED[i].val));
            }
            else {
              fprintf(stderr,"\n%s Projection:  Last %s used was %.10lf\n\t\tdo you want to change it (y/[n]) ? ",io, DESC[i], USED[i].val);
            }
            gets(answer);
          }
          else *answer = 'y';
        }
       
        if ((strlen(answer) != 0) &&
		   *answer != 'N' && *answer != 'n')
        { 
          proj_changed = 1;
          if (i==SOUTH) {
            sprintf(buff," +south ");
            strcat(parms,buff);
          }  
          for(;;)
          {
            if ((i==LAT0)||(i==LAT1)||(i==LAT2)||(i==LATTS))
	      if (get_LL_stuff(in,1,proj_index,i)) break;
            if (i==LON0)
	      if (get_LL_stuff(in,0,proj_index,i)) break;
            if (i==ZONE) 
              if (get_zone(in)) break;
            if (i==KFACT)
              if (get_KFACT(in)) break;
            if (i==SOUTH) break;
          }  /* for */
          USED[i].was = 1;
        }
        switch (i) {
          case LAT0:
            sprintf(buff," +lat_0=%.10lf ",USED[i].val);
            strcat(parms,buff);
            break;
          case LAT1:
            sprintf(buff," +lat_1=%.10lf ",USED[i].val);
            strcat(parms,buff);
            break;
          case LAT2:
            sprintf(buff," +lat_2=%.10lf ",USED[i].val);
            strcat(parms,buff);
            break;
          case LATTS:
            sprintf(buff," +lat_ts=%.10lf ",USED[i].val);
            strcat(parms,buff);
            break;
          case LON0:
            sprintf(buff," +lon_0=%.10lf ",USED[i].val);
            strcat(parms,buff);
            break;
          case ZONE:
            sprintf(buff," +zone=%d ",(int)(USED[i].val));
            strcat(parms,buff);
            break;
          case KFACT:
            sprintf(buff," +k=%.10lf ",USED[i].val);
            strcat(parms,buff);
            break;
          default: break;
        }
      } /* ask */ 
    }  /* for */
 }  /* switch index */

 if (proj_index != LL) {
   do {
     fprintf(stderr, "Enter plural for %s units (%s):",io,units);
   } while (!G_gets(answer)) ;
   if (strlen(answer) != 0)  {
     sprintf(units,"%s",answer);
   }
   G_strip(units);
   if (strcmp(old_units,units) != 0)
     proj_changed = 1;
   npr = strlen(units);
   for (i = 0; i < NUNITS; i++) {
     stat = min1(npr,strlen(UNITS[i].units));
     if(strncmp(UNITS[i].units,units,stat) == 0) {
       unit_fact = UNITS[i].fact;
       break;
     }
   }
   if (i >= NUNITS) {
     for (;;) {
       do {
         fprintf(stderr,"Enter conversion factor from %s to meters:",units);
       } while (!G_gets(answer)) ;
       if (strlen(answer) != 0) {
         if(sscanf(answer,"%lf",&unit_fact)!=1) fprintf(stderr,"\nInvalid Entry\n");
         else break;
       }
     }
   }
   sprintf(buff," +unfact=%.10lf ",unit_fact);
   strcat(parms,buff);
 }
 return proj_changed;
}



