#include "gis.h"
#include "digit.h"

whatxy (east, north, Map, Cats)
struct Map_info *Map;
struct Categories *Cats;
double east, north;
{
P_AREA *Area ;
plus_t area ;
int acat ;

  area = dig_point_to_area (Map, east, north);
  if (area != 0)
  {
    Area = &(Map->Area[area]);
    if (Area->att)
    {
      acat = Map->Att[Area->att].cat;
      if (Cats->num > 0)
        printf ("Area - Category %d %s\n", acat, G_get_cat(acat, Cats));
      else
        printf ("Area - Category <not tagged>\n");
    }
  }
}
