/*======================================================================
			      examine.c
======================================================================*/

#include <string.h>
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
void 
PrintTMinfo (TMinfo *tminfo)
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
static void PrintQuadScene (TMinfo *tminfo)
{
   fprintf(stderr, "File Format:             ");
   switch(tminfo->type) {
      case QuadrantScene:
      	 fprintf(stderr, "Quadrant Scene (%s)\n", tminfo->file_format);
      	 fprintf(stderr, "Quadrant:                %d\n", tminfo->quadrant);
      	 break;
      case FullScene:
      	 fprintf(stderr, "Full Scene (%s)\n", tminfo->file_format); break;
         break;

      default:
         fprintf(stderr, "PrintQuadScene(): Type=%d not supported\n",tminfo->type);
   }
   fprintf(stderr, "Rows:                    %d\n", tminfo->rows);
   fprintf(stderr, "Columns:                 %d\n", tminfo->cols);
   fprintf(stderr, "Full Scene Center:       %s\n",
      SPrintGeo(tminfo->full_scene_geo));
   fprintf(stderr, "                         %s\n",
      SPrintPixel(tminfo->full_scene_pixel));

   if( tminfo->type == QuadrantScene) {
      fprintf(stderr, "Quadrant Center:         %s\n",
      	 SPrintGeo(tminfo->quad_scene_geo));
      fprintf(stderr, "                         %s\n",
      	 SPrintPixel(tminfo->quad_scene_pixel));
   }

   fprintf(stderr, "Mission:                 %d\n", tminfo->mission);
   fprintf(stderr, "Path/Row: 	      	    %d/%d\n", tminfo->path,
      tminfo->row_number);
   fprintf(stderr, "Volume Creation Date:    %s\n",
      SPrintDate(tminfo->vol_creation_date));
   fprintf(stderr, "Volume Creation Time:    %s\n", 
      SPrintTime(tminfo->vol_creation_time));
   fprintf(stderr, "Product ID:              %s\n", tminfo->product_id);
   fprintf(stderr,"Image Date:              %s\n",
      SPrintDate(tminfo->image_date));
   fprintf(stderr, "Image Time:              %s\n",
      SPrintTime(tminfo->image_time));
   fprintf(stderr, "Scene ID:                %s\n", tminfo->scene_id);
   fprintf(stderr, "Interleaving Type:       %s\n", tminfo->interleave);
   fprintf(stderr, "Date of Generation:      %s\n",
      SPrintDate(tminfo->generation_date));
}



/*======================================================================
			      PrintFast
======================================================================*/
static void PrintFast (TMinfo *tminfo)
{
   fprintf(stderr, "Fast File Format Version %s\n", tminfo->type == FastA ? "A" : "B");
   fprintf(stderr, "Rows:                    %d\n", tminfo->rows);
   fprintf(stderr, "Columns:                 %d\n", tminfo->cols);
   fprintf(stderr, "Northwest Corner:        %s\n", SPrintGeo(tminfo->NWgeo));
   fprintf(stderr, "                         %s\n", SPrintUTM(tminfo->NWutm));
   fprintf(stderr, "Northeast Corner:        %s\n", SPrintGeo(tminfo->NEgeo));
   fprintf(stderr, "                         %s\n", SPrintUTM(tminfo->NEutm));
   fprintf(stderr, "Southwest Corner:        %s\n", SPrintGeo(tminfo->SWgeo));
   fprintf(stderr, "                         %s\n", SPrintUTM(tminfo->SWutm));
   fprintf(stderr, "Southeast Corner:        %s\n", SPrintGeo(tminfo->SEgeo));
   fprintf(stderr, "                         %s\n", SPrintUTM(tminfo->SEutm));

   switch(tminfo->type) {
      case FastA:
      if( tminfo->full_scene_utm.east >= 0) {
      	 fprintf(stderr, "Scene Center:            lat/lon coordinates not available\n");
      	 fprintf(stderr, "                         %s\n",
      	    SPrintUTM(tminfo->full_scene_utm));
      } else
      	 fprintf(stderr, "Scene Center:            not available\n");
      fprintf(stderr, "Scene ID:                %s\n", tminfo->scene_id);
      break;

      case FastB:
      fprintf(stderr, "Scene Center:            %s\n",
      	 SPrintGeo(tminfo->full_scene_geo));
      fprintf(stderr, "                         %s\n",
      	 SPrintUTM(tminfo->full_scene_utm));
      fprintf(stderr, "                         %s\n",
      	 SPrintPixel(tminfo->full_scene_pixel));
      fprintf(stderr, "Product Number           %s\n", tminfo->product_id);
      break;

      default:
        fprintf(stderr, "PrintFast (): Type=%d not supported\n",tminfo->type);
   }

   fprintf(stderr, "Acquisition Date:        %s\n", SPrintDate(tminfo->image_date));
}



char *
SPrintGeo (TMgeo tmgeo)
{
   static char buffer[100];
   G_format_northing(tmgeo.lat, buffer, PROJECTION_LL);
   strcat(buffer, " ");
   G_format_easting(tmgeo.lon, buffer+strlen(buffer), PROJECTION_LL);
   return(buffer);
}

char *
SPrintUTM (TMutm tmutm)
{
   static char buffer[100];
   sprintf(buffer, "Easting = %13.3f Northing = %13.3f",
      tmutm.east, tmutm.north);
   return(buffer);
}

char *
SPrintPixel (TMpixel tmpixel)
{
   static char buffer[100];
   sprintf(buffer, "Row = %d Column = %d", tmpixel.row, tmpixel.col);
   return(buffer);
}

char *
SPrintDate (TMdate tmdate)
{
   static char buffer[100];
   static char *months[] = { "Jan", "Feb", "Mar", "Apr", "May", "Jun",
      "Jul", "Aug", "Sep", "Oct", "Nov", "Dec" };
   sprintf(buffer, "%s %d, %d", months[tmdate.month], tmdate.day,
      tmdate.year);
   return(buffer);
}

char *
SPrintTime (TMtime tmtime)
{
   static char buffer[100];
   sprintf(buffer, "%d:%d:%d.%d", tmtime.hours, tmtime.mins, tmtime.secs,
      tmtime.decimal);
   return(buffer);
}


