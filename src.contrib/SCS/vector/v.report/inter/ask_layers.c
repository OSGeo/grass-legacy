#include "global.h"
#include <string.h>

int ask_layers (void)
{
    char prompt[80];
    char name[256];
    char *mapset;
    char *map_type;

    G_clear_screen();

    layer = NULL;
    nlayers = 0;

    sprintf (prompt, "Enter vector(digit) map");
    mapset = G_ask_old( prompt, name, "dig", "vector") ;
    if ( ! mapset)
	exit(0);
	 
    for(;;)
      {
      map_type = '\0';
      fprintf(stderr,"\nEnter a the map type, area, line, or site [area] > ");
      fgets(prompt,80,stdin);
      if (strlen(prompt) == 0)
        {
        map_type = "area";
        break;
        }
      if(*prompt == 'a') map_type = "area";
      if(*prompt == 'l') map_type = "line";
      if(*prompt == 's') map_type = "site";
      if (map_type == NULL)
        fprintf(stderr,"Invalid entry, try again\n");
      else break;
      }

    layer = (struct layer *) G_realloc (layer, (nlayers+1) * sizeof(struct layer));
    layer[nlayers].name = G_store(name);
    layer[nlayers].mapset = G_store(mapset);
    layer[nlayers].type = G_store(map_type);
    nlayers++;

    return 0;
}
