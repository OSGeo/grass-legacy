#include <stdio.h>
#include <math.h>
#include "/usr2/grass4.0/src/libes/gis.h"
#define xp 5

main() {
long *pcell;
int x, fd,i;
struct Colors color;
char *mapset, *G_mapset();
  G_gisinit("test");
  G_init_colors(&color);
  mapset = G_mapset();

  fd = G_open_cell_new("junk.file",mapset);
  printf("\nafter open_cell_new");
  pcell = (long *) malloc(sizeof(long)*2*xp);
  printf("\nafter malloc\n");

  for(x=3;x < 2*xp;x++)
   (CELL)pcell[x] = 1;

  for(i=0;i<3;i++)
  G_put_map_row(fd,pcell);

  printf("first put\n");
  G_close_cell(fd);
  G_set_color((CELL)1,255,0,0,&color);
  G_set_color((CELL)0,0,0,0,&color);
  G_write_colors("junk.file",mapset,&color);

  fd = G_open_cell("junk.file",mapset);
  for(i=3;i<6;i++)
  { 
  G_put_map_row(fd,pcell);
  }
  printf("second put\n");

  G_close_cell(fd);
  G_set_color((CELL)1,255,0,0,&color);
  G_set_color((CELL)0,0,0,0,&color);
  G_write_colors("junk.file",mapset,&color);
}
