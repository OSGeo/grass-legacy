/*======================================================================
			      examine.c
======================================================================*/

#include "landsat.h"

#ifdef _NO_PROTO
static void PrintQuadScene();
static void PrintFast();
#else
static void PrintQuadScene(TMinfo*);
static void PrintFast(TMinfo*);
#endif

/*======================================================================
			     PrintTMinfo
======================================================================*/
void PrintTMinfo(tminfo)
TMinfo *tminfo;
{
   switch(tminfo->type) {
      case QuadrantScene:
      case FullScene:
      	 PrintQuadScene(tminfo); break;
      case FastA:
      case FastB:
      	 PrintFast(tminfo); break;
   }
}



/*======================================================================
			    PrintQuadScene
======================================================================*/
static void PrintQuadScene(tminfo)
TMinfo *tminfo;
{
   printf("File Format:             ");
   switch(tminfo->type) {
      case QuadrantScene:
      	 printf("Quadrant Scene (%s)\n", tminfo->file_format);
      	 printf("Quadrant:                %d\n", tminfo->quadrant);
      	 break;
      case FullScene:
      	 printf("Full Scene (%s)\n", tminfo->file_format); break;
   }
   printf("Rows:                    %d\n", tminfo->rows);
   printf("Columns:                 %d\n", tminfo->cols);
   printf("Full Scene Center:       %s\n",
      SPrintGeo(tminfo->full_scene_geo));
   printf("                         %s\n",
      SPrintPixel(tminfo->full_scene_pixel));

   if( tminfo->type == QuadrantScene) {
      printf("Quadrant Center:         %s\n",
      	 SPrintGeo(tminfo->quad_scene_geo));
      printf("                         %s\n",
      	 SPrintPixel(tminfo->quad_scene_pixel));
   }

   printf("Mission:                 %d\n", tminfo->mission);
   printf("Path/Row: 	      	    %d/%d\n", tminfo->path,
      tminfo->row_number);
   printf("Volume Creation Date:    %s\n",
      SPrintDate(tminfo->vol_creation_date));
   printf("Volume Creation Time:    %s\n", 
      SPrintTime(tminfo->vol_creation_time));
   printf("Product ID:              %s\n", tminfo->product_id);
   printf("Image Date:              %s\n",
      SPrintDate(tminfo->image_date));
   printf("Image Time:              %s\n",
      SPrintTime(tminfo->image_time));
   printf("Scene ID:                %s\n", tminfo->scene_id);
   printf("Interleaving Type:       %s\n", tminfo->interleave);
   printf("Date of Generation:      %s\n",
      SPrintDate(tminfo->generation_date));
}



/*======================================================================
			      PrintFast
======================================================================*/
static void PrintFast(tminfo)
TMinfo *tminfo;
{
   printf("Fast File Format Version %s\n", tminfo->type == FastA ? "A" : "B");
   printf("Rows:                    %d\n", tminfo->rows);
   printf("Columns:                 %d\n", tminfo->cols);
   printf("Northwest Corner:        %s\n", SPrintGeo(tminfo->NWgeo));
   printf("                         %s\n", SPrintUTM(tminfo->NWutm));
   printf("Northeast Corner:        %s\n", SPrintGeo(tminfo->NEgeo));
   printf("                         %s\n", SPrintUTM(tminfo->NEutm));
   printf("Southwest Corner:        %s\n", SPrintGeo(tminfo->SWgeo));
   printf("                         %s\n", SPrintUTM(tminfo->SWutm));
   printf("Southeast Corner:        %s\n", SPrintGeo(tminfo->SEgeo));
   printf("                         %s\n", SPrintUTM(tminfo->SEutm));

   switch(tminfo->type) {
      case FastA:
      if( tminfo->full_scene_utm.east >= 0) {
      	 printf("Scene Center:            lat/lon coordinates not available\n");
      	 printf("                         %s\n",
      	    SPrintUTM(tminfo->full_scene_utm));
      } else
      	 printf("Scene Center:            not available\n");
      printf("Scene ID:                %s\n", tminfo->scene_id);
      break;

      case FastB:
      printf("Scene Center:            %s\n",
      	 SPrintGeo(tminfo->full_scene_geo));
      printf("                         %s\n",
      	 SPrintUTM(tminfo->full_scene_utm));
      printf("                         %s\n",
      	 SPrintPixel(tminfo->full_scene_pixel));
      printf("Product Number           %s\n", tminfo->product_id);
      break;
   }

   printf("Acquisition Date:        %s\n", SPrintDate(tminfo->image_date));
}



char *SPrintGeo(tmgeo)
TMgeo tmgeo;
{
   static char buffer[100];
   G_format_northing(tmgeo.lat, buffer, PROJECTION_LL);
   strcat(buffer, " ");
   G_format_easting(tmgeo.lon, buffer+strlen(buffer), PROJECTION_LL);
   return(buffer);
}

char *SPrintUTM(tmutm)
TMutm tmutm;
{
   static char buffer[100];
   sprintf(buffer, "Easting = %13.3lf Northing = %13.3lf",
      tmutm.east, tmutm.north);
   return(buffer);
}

char *SPrintPixel(tmpixel)
TMpixel tmpixel;
{
   static char buffer[100];
   sprintf(buffer, "Row = %d Column = %d", tmpixel.row, tmpixel.col);
   return(buffer);
}

char *SPrintDate(tmdate)
TMdate tmdate;
{
   static char buffer[100];
   static char *months[] = { "Jan", "Feb", "Mar", "Apr", "May", "Jun",
      "Jul", "Aug", "Sep", "Oct", "Nov", "Dec" };
   sprintf(buffer, "%s %d, %d", months[tmdate.month], tmdate.day,
      tmdate.year);
   return(buffer);
}

char *SPrintTime(tmtime)
TMtime tmtime;
{
   static char buffer[100];
   sprintf(buffer, "%d:%d:%d.%d", tmtime.hours, tmtime.mins, tmtime.secs,
      tmtime.decimal);
   return(buffer);
}


