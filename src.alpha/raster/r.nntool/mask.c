#include "globals.h"

mask() 
{
int il, p, xp, k, max, min, edgs, x11, x22, y11, y22, x, y, interX[500];
int *Ymax, *Ymin, pc, ypls1, ymns1, minedg, xmax, ymax,xmin,ymin,maxedg;
int Yscan, elements, i, j, color, xint, left, top;
extern int *CNT, rm[20][100], nxm[20][100], nym[20][100], *Nopc, Noclass;
extern int area[25][50], *ftr, Noedg[25][100], no_tr[3][20][20], nrows, ncols;
float sqrt();
extern long **MAP[10];
extern struct coord polygon[25][50][50];
struct Colors colorg;
struct Categories cats;
extern struct Cell_head cellhd;
char tmpstr[20], *mapset;

    for(il=1;il <= Noclass;il++) {
        for(p=0;CNT[il] <= Noclass && p < CNT[il];p++) {
 
          if(area[il][p] != NULL && area[il][p] == 1) {
            for(k=0;k < no_tr[area[il][p]][il][p];k++) {
              for(y=rm[il][k];y > 0;y--) {
               xp = (nint)(sqrt(rm[il][k]*rm[il][k]*1. - y*y*1.));
               for(x=0;x < xp;x++)
                 MAP[il][nym[il][k]-y][nxm[il][k]+x] = 1;
               for(x=0;x < xp;x++)
                 MAP[il][nym[il][k]-y][nxm[il][k]-x] = 1;
              }
              for(y=0;y <= rm[il][k];y++) {
                xp = (nint)(sqrt(rm[il][k]*rm[il][k]*1. - y*y*1.));
                for(x=0;x < xp;x++)
                  MAP[il][nym[il][k]+y][nxm[il][k]+x] = 1;
                for(x=0;x < xp;x++)
                  MAP[il][nym[il][k]+y][nxm[il][k]-x] = 1;
              }
            }
          }

          else if(area[il][p] != NULL && area[il][p] == 0) {
             for(k=0;k < no_tr[area[il][p]][il][p];k++)
                 MAP[il][nym[il][k]][nxm[il][k]] = 1;
          }
 
          else if(area[il][p] != NULL && area[il][p] == 2) {
            Ymax = (int *) malloc(sizeof(int)*Nopc[il]);
            Ymin = (int *) malloc(sizeof(int)*Nopc[il]);
            for(pc=1;pc <= Nopc[il];pc++) {
              max = 0;    min = 100000;
              for(edgs=1;edgs <= Noedg[il][pc];edgs++) {
                if(polygon[il][pc][edgs].y1 > max) {
                  max = polygon[il][pc][edgs].y1;
                  xmax = polygon[il][pc][edgs].x1;
                  maxedg = edgs;
                }
                if(polygon[il][pc][edgs].y1 < min) {
                  min = polygon[il][pc][edgs].y1;
                  xmin = polygon[il][pc][edgs].x1;
                  minedg = edgs;
                }
              }   
              Ymax[pc-1] = max;
              Ymin[pc-1] = min;
            }
 
            for(Yscan=0;Yscan < nrows;Yscan++) {
               elements = 0;
               for(pc=1;pc <= Nopc[il];pc++)
                 for(edgs=1;edgs <= Noedg[il][pc];edgs++) {
                  y11 = polygon[il][pc][edgs].y1;
                  y22 = polygon[il][pc][edgs].y2;
                  x11 = polygon[il][pc][edgs].x1;
                  x22 = polygon[il][pc][edgs].x2;

                  if(Yscan==Ymin[pc-1]) {
                    if(minedg > 1 && minedg <= Noedg[il][pc]) {
                      ypls1 = polygon[il][pc][minedg].y2;
                      ymns1 = polygon[il][pc][minedg-1].y1;
                    }
                    else if(minedg == 1) {
                      ypls1 = polygon[il][pc][minedg].y2;
                      ymns1 = polygon[il][pc][Noedg[il][pc]].y1;
                    }

                    if(ypls1 > Yscan && ymns1 > Yscan) {
                      interX[elements] = xmin; interX[elements+1] = xmin;
                      elements += 2;
                      break;
                    }
                  }
                  else if(Yscan==Ymax[pc-1]) {
                    if(maxedg > 1 && maxedg <= Noedg[il][pc]) {
                      ypls1 = polygon[il][pc][maxedg].y2;
                      ymns1 = polygon[il][pc][maxedg-1].y1;
                    }
                    else if(maxedg == 1) {
                      ypls1 = polygon[il][pc][maxedg].y2;
                      ymns1 = polygon[il][pc][Noedg[il][pc]].y1;
                    }

                    if(ypls1 < Yscan && ymns1 < Yscan) {
                      interX[elements] = xmax; interX[elements+1] = xmax;
                      elements += 2;
                      break;
                    }
                  }

                  if(y11!=y22 && ((y11>=Yscan && y22<Yscan) ||
                                        (y11<Yscan && y22>=Yscan))) {
                    if(Yscan < y22)
                      xint=x22 - ((y22 - Yscan)*1./(y22 - y11))*(x22 - x11);
                    else
                      xint=x22 + ((Yscan - y22)*1./(y11 - y22))*(x11 - x22);
                    interX[elements] = xint;
                    elements++;
                  }
                 }

               if(elements) {
                 sort(interX,elements);
                 for(j=0;j < elements;j+=2) {
                   if(interX[j+1] != interX[j]) {
                     for(x=0;x < interX[j+1] - interX[j];x++)
                       MAP[il][Yscan-1][interX[j]+x] = 1;
                   }
                   else
                       MAP[il][Yscan-1][interX[j]] = 1;
                 }
                 for(j=0;j < elements;j++) interX[j] = 0;
               }
             }
          }     
        }
    }

    G_set_window(&cellhd);
    for(k=1;k <= Noclass;k++) {
	sprintf(tmpstr,"train.cls%d",k);
        ftr[k] = G_open_cell_new(tmpstr);
        Menu_msg("Creating training cell files");
        for(i=0;i < nrows;i++)
          G_put_map_row(ftr[k],MAP[k][i]);
    }
 
    for(il=1;il <= Noclass;il++) {
      G_init_colors(&colorg);
      G_close_cell(ftr[il]);
      G_init_colors(&colorg);
      color = 255 - (nint)((il*1./Noclass)*255.);
      G_set_color((CELL)1,color,color,color,&colorg);
      G_set_color((CELL)0,255,255,255,&colorg);
      sprintf(tmpstr,"train.cls%d",il);
      mapset = G_find_cell(tmpstr,"");
      G_write_colors(tmpstr,mapset,&colorg);
      G_free_colors(&colorg);
      G_init_cats((CELL)0,"",&cats);
      G_set_cat((CELL)0,"unmasked area",&cats);
      G_set_cat((CELL)1,"masked area",&cats);

      sprintf(tmpstr,"train.cls%d",il);
      G_write_cats(tmpstr,&cats);
      G_free_cats(&cats);
    }
    use_mouse_msg();
    return(0);
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
