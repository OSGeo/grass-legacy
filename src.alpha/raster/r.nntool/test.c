#include <stdio.h>
#include "/usr2/grass4.0/src/libes/gis.h"

struct Cell_head *window;

main() {
int a[10],i;

  window = (struct Cell_head *) malloc(sizeof(struct Cell_head));

  G_gisinit("test");
  G_get_window(window);

/*  R_open_driver();
  R_close_driver(); */

  get();
}

get() {
char *mapset, *G_mapset();
struct Cell_head Twindow;
struct junk *ab;
int nrows;

  system("d.rast.zoom");
  system("d.frame -e");
  system("d.rast junk.1");
  G_my_get_window(&Twindow);
  G_set_window(Twindow);

  printf("we are not going to take it!! \n");
}

G_my_get_window(Zoom)
struct Cell_head *Zoom;
{
char *G_location_path(), *path, *junk1, *junk2;
char *mapset, *G_mapset(), *tmpstr;
FILE *fread;

   path = (char *) malloc(sizeof(char)*25);
   tmpstr = (char *) malloc(sizeof(char)*10);
   junk1 = (char *) malloc(sizeof(char)*10);
   junk2 = (char *) malloc(sizeof(char)*10);
   path = G_location_path();

   mapset = G_mapset();
   
   sprintf(tmpstr,"/%s",mapset);
   strcat(path,tmpstr);
   strcat(path,"/WIND");
   fread = fopen(path,"r");
   
   fscanf(fread,"%s %d",junk1,&Zoom->proj);
   fscanf(fread,"%s %d",junk1,&Zoom->zone);
   fscanf(fread,"%s %lf",junk1,&Zoom->north);
   fscanf(fread,"%s %lf",junk1,&Zoom->south);
   fscanf(fread,"%s %lf",junk1,&Zoom->east);
   fscanf(fread,"%s %lf",junk1,&Zoom->west);
   fscanf(fread,"%s %d",junk1,&Zoom->cols);
   fscanf(fread,"%s %d",junk1,&Zoom->rows);
   fscanf(fread,"%s %s %lf",junk1, junk2, &Zoom->ew_res);
   fscanf(fread,"%s %s %lf",junk1, junk2, &Zoom->ns_res);
}
   
sort(array,limit)
int array[], limit;
{ 
 
        int top, search, temp;
 
        for(top=0;top < limit-1;top++) 
                for(search=top+1;search < limit;search++) 
                        if(array[search] < array[top]) { 
                                temp = array[search]; 
                                array[search] = array[top];
                                array[top] = temp; 
                        }
}
