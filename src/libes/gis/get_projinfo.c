#include  "gis.h"
#include  "glocale.h"
#include <unistd.h>

#define PERMANENT "PERMANENT"

struct Key_Value *
G_get_projunits() 
{
	int stat;
        struct Key_Value *in_units_keys;
	char path[1024];

         G__file_name (path, "", UNIT_FILE, PERMANENT);
         if (access(path,0) != 0)
         {
           fprintf(stderr,_("%s file not found for location %s\n"),
		 UNIT_FILE, G_location());
           return NULL;
         }
         in_units_keys = G_read_key_value_file(path,&stat);
         if (stat != 0)
         { 
             fprintf(stderr,_("ERROR in reading %s file for location %s\n"),
		UNIT_FILE, G_location());
             return NULL;
         }

         return in_units_keys;

}



struct Key_Value *
G_get_projinfo() 
{
	int stat;
        struct Key_Value *in_proj_keys;
	char path[1024];

         G__file_name (path, "", PROJECTION_FILE, PERMANENT);
         if (access(path,0) != 0)
         {
           fprintf(stderr,_("%s file not found for location %s\n"),
	          PROJECTION_FILE, G_location());
           return NULL;
         }
         in_proj_keys = G_read_key_value_file(path,&stat);
         if (stat != 0)
         { 
             fprintf(stderr,_("ERROR in reading %s file for location %s\n"),
	          PROJECTION_FILE, G_location());
           return NULL;
         }
         return in_proj_keys;
}
