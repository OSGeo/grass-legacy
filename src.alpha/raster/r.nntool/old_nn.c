/*************************************************************************
This program is called nntool.c.  It was written by Ranjan S. Muttiah
while he was a student at Purdue University and while working for Texas A&M
at the Blackland Research Center on the HUMUS project.  
email: muttiah@brcsun1.tamu.edu
*****************************************************************************/
#include <stdio.h>
#include <math.h>
#include "/usr2/grass4.0/src/libes/gis.h"
#define YES 1
#define NO 0
#define QP 0
#define BP 1
#define LIN 2
#define skip 1 /* # of rows to skip for sampling */
#define BC_DISP "red"
#define TC_DISP "yellow"
#define DC_DISP "white"


int nrows,ncols,numfiles,nxm[20][100],nym[20][100],*ftr, Noedg[25][100];
int DIVROWS,DIVCOLS,propout=0,rm[20][100],no_tr[3][20][20],NO_INPUT=0;
int numtrain=0, Type_net, numtest, AVG, RANDOM, maxcategory=0, *Nopc;
int area[25][50], thermo, Noclass, ask, nexttime, *attr, *CNT;
int **FLAG, resolve;
char outname[50],*mapset, cellfile[20];
struct Cell_head window;
struct coord {
	int x1, y1, x2, y2;
	};
struct coord polygon[25][50][50];
long **MAP[10], **INPUT[4];
float **mean, **OUT, **kINPUT[4];

/* Note: This is a little demented!: There are DIVROWS number of _cells_
that are averaged row-wise, and there are DIVCOLS number of _divisions_
column-wise, if using the averaging option of nntool. */

main() {
CELL cell, *G_allocate_cell_buf();
struct Cell_head *new_window;
int x, y, nxi, nyi, button, ans, num=1, k=0, i=0, xp,D_popup(), Yscan;
int bot[1], top[1], left[1], rite[1], num2=2, **lumpdata,xmax,xmin;
int topw, botw, leftw, ritew, il, kk, irow, jcol, **temparray,addon;
int polyx[8], polyy[8], nxe, nye, move=0, class, **compress, addcls, redo;
int scr_t_w, scr_b_w, scr_l_w, scr_r_w, color, max, min, ypls1, ymns1;
int x11, y11, x22, y22, elements, pc, edgs, interX[500], *Ymax, *Ymin;
unsigned char red, green, blu;
int bc, tc, dc, topscr=100, leftscr=100, size=3, flump, flumped,maxedg,classify;
int t, b, l, r, localvalue, lumpcol, lumprow, j, ii, jl, jj, p,minedg;
static char *opt[] = {
		"    NEURAL NET TOOL",
		" LUMP DATA FOR LAYER",
		"  GET TRAINING AREA",
		" DONE SELECTION",
		" DISPLAY SITES ",
		" CONFIGURE NETWORK",
		"  SAMPLE FROM SITES ",
		"  REORDER TRAINING DATA",
		" TRAIN NETWORK",
		" PROPAGATE INPUT",
		" DISPLAY NN OUTPUT",
                " LINEAR CLASSIFIER",
		"        QUIT ",
		NULL,
		};
struct Colors colorg, colors;
struct Categories cats;
char *name1, *G_mapset(), tmpstr[20];
char lumpfile[20], lumpedfile[20];
float radius, pow(), sqrt(), xint;
double D_d_to_u_row(), D_d_to_u_col(), D_d_to_a_col();
double D_get_d_west(), D_get_d_south(), D_d_to_a_row();
CELL *pcellm, *pcell;

 G_gisinit("nntool");
 G_get_window(&window);
 R_open_driver();

 bc = D_translate_color(BC_DISP);
 tc = D_translate_color(TC_DISP);
 dc = D_translate_color(DC_DISP);
 R_font("romant");

 mapset = G_mapset();
 nrows = G_window_rows();
 ncols = G_window_cols();
 if (D_get_screen_window(&t, &b, &l, &r))
    G_fatal_error("Getting screen window");
 if(D_do_conversions(&window,t,b,l,r))
    G_fatal_error("Error in calculating conversions"); 

 nexttime=0;
 while(1) {
  ans = D_popup (
		bc,
		tc,
		dc,
		topscr,
		leftscr,
		size,
		opt
		);
  switch(ans) {
    case 1:
      G_ask_cell_old("Enter the map layer for lumping",lumpfile); 
      printf("What do you want to call the new cell file ? ");
      scanf("%s",lumpedfile);
      flump = G_open_cell_old(lumpfile, mapset);
      G_read_colors(lumpfile,mapset,&colors);

      nrows = G_window_rows();
      ncols = G_window_cols();
      printf("How many cells ( > 0!) do you want to lump row-wise ? ");
      scanf("%d",&lumprow);
      printf("How many cells ( > 0!) do you want to lump column-wise ? ");
      scanf("%d",&lumpcol);
      getchar();
      lumpdata = (int **) malloc(sizeof(int)*nrows);
      temparray = (int **) malloc(sizeof(int)*lumprow);

      for(i=0;i < lumprow;i++)
	temparray[i] = (int *) malloc(sizeof(int)*lumpcol);

      maxcategory = 0;
      for(i=0;i < nrows;i++) {
	lumpdata[i] = (int *) malloc(sizeof(int)*ncols);
        pcell = G_allocate_cell_buf();
	G_get_map_row(flump,pcell,i);
	for(j=0;j < ncols;j++) {
          lumpdata[i][j] = pcell[j];
          if(lumpdata[i][j] > maxcategory)
		maxcategory = lumpdata[i][j];
        }
      }
      attr = (int *) malloc(sizeof(int)*(maxcategory+2));

      compress = (int **)malloc(sizeof(int)*((nint)(nrows*1./lumprow)+2));
      for(i=0,k=0;i < nrows - lumprow;i+=lumprow,k++) {
	compress[k]=(int *)malloc(sizeof(int)*((nint)(ncols*1./lumpcol)+2));
	for(j=0,jcol=0;j < ncols - lumpcol;j+=lumpcol,jcol++) {
	  for(ii=i,il=0;ii < i+lumprow;ii++,il++)
	    for(jj=j,jl=0;jj < j+lumpcol;jj++,jl++)
	      temparray[il][jl] = lumpdata[ii][jj];
	  compress[k][jcol] = checkcategory(lumprow,lumpcol,temparray);
	}
	for(il=0;il < lumprow;il++)
	  for(jl=0;jl < lumpcol;jl++)
	    temparray[il][jl] = 0;

	for(ii=i,il=0;ii < i+lumprow;ii++,il++)
	  for(j=ncols - lumpcol,jl=0;j < ncols;j++,jl++)
	    temparray[il][jl] = lumpdata[ii][j];
        compress[k][jcol] = checkcategory(il,jl,temparray);
      }

      compress[k]=(int *) malloc(sizeof(int)*((nint)(ncols*1./lumpcol)+2));
      for(j=0,jcol=0;j < ncols - lumpcol;j+=lumpcol,jcol++)
        for(jj=j,jl=0;jj < j+lumpcol;jj++,jl++)
          for(i=nrows-lumprow,il=0;i < nrows;i++,il++) {
              temparray[il][jl] = lumpdata[i][jj];
          compress[k][jcol] = checkcategory(lumprow,lumpcol,temparray);
        }

      for(il=0;il < lumprow;il++)
        for(jl=0;jl < lumpcol;jl++)
          temparray[il][jl] = 0; 
 
      for(j=ncols - lumpcol,jl=0;j < ncols;j++,jl++)
        for(i=nrows-lumprow,il=0;i < nrows;i++,il++)
          temparray[il][jl] = lumpdata[i][j]; 
      compress[k][jcol] = checkcategory(il,jl,temparray); 
      G_close_cell(flump);
 
      new_window = (struct Cell_head *) malloc(sizeof(struct Cell_head));
      new_window->format = window.format;
      new_window->compressed = window.compressed;
      new_window->rows = (nint)((nrows*1.)/lumprow);
      new_window->cols = (nint)((ncols*1.)/lumpcol);
      new_window->proj = window.proj;
      new_window->zone = window.zone;
      new_window->ew_res = window.ew_res*lumpcol;
      new_window->ns_res = window.ns_res*lumprow;
      new_window->north = window.north;
      new_window->south = window.north - new_window->rows*new_window->ns_res;
      new_window->west = window.west;
      new_window->east = window.west + new_window->cols*new_window->ew_res;
      G_put_window(new_window);
      G_set_window(new_window);
      flumped = G_open_cell_new(lumpedfile, mapset);
      nrows = G_window_rows();
      ncols = G_window_cols();

      for(i=0;i < nrows;i++)
        G_put_map_row(flumped,compress[i]);

      G_close_cell(flumped);
      G_write_colors(lumpedfile,mapset,&colors);      
      Dcell(lumpedfile,mapset,1);

      resolve=G_yes("Want to keep the new updated resolution ?",1);
      if(resolve == NO) {
	G_put_window(&window);
	G_set_window(&window);
	nrows = G_window_rows(); ncols = G_window_cols();
        Dcell(lumpfile,mapset,1);
      }
      else {
        if(D_get_screen_window(&t, &b, &l, &r))
          G_fatal_error("Getting screen window");
        if(D_do_conversions(&new_window,t,b,l,r))
          G_fatal_error("Error in calculating conversions");
      }
      break;
    case 2:
      if(nexttime == 0) {
	G_ask_cell_old("Enter name of the output cell file", cellfile);
 	G_clear_screen();
	Dcell(cellfile,mapset,1);
        printf("How many classes do you have for output ? -->: ");
        scanf("%d",&Noclass);
        getchar();
        ftr = (int *) malloc(sizeof(int)*(Noclass+10));
	CNT = (int *) malloc(sizeof(int)*(Noclass+10));
	Nopc = (int *) malloc(sizeof(int)*(Noclass+10));
        for(k=1;k <= Noclass;k++) {
            Nopc[k] = 0;
            sprintf(tmpstr,"train.cls%d",k);
            ftr[k] = G_open_cell_new(tmpstr);
	    MAP[k] = (long **) malloc(sizeof(long)*nrows);
            for(i=0;i < nrows;i++) {
	      MAP[k][i] = (long *) malloc(sizeof(long)*ncols);
	      for(j=0;j < ncols;j++)
		MAP[k][i][j] = 0;
	    }
        }
        printf("ok\n");
      
        for(ask=1;ask <= Noclass;ask++) {
	  printf("SELECT TRAINING AREA FOR CLASS # %d\n",ask);
          get();
	}
      }
      else {
          printf("BOING!!, GRASS won't let you do this! -- RSM 11/5/92\n");
          break;
      }

      G_clear_screen();
      RANDOM = NO;
     
      break;
    case 3:
      if(nexttime == 1) {addon = ask - 1;}
      else addon = 0;
      for(il=1+addon;il <= Noclass+addon;il++) {
        printf("Masking file #%d \n",il);
        for(p=0;p < CNT[il];p++) {

          if(area[il][p] == 1) {
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

          else if(area[il][p] == 0) {
             for(k=0;k < no_tr[area[il][p]][il][p];k++)
	         MAP[il][nym[il][k]][nxm[il][k]] = 1;
          }

	  else if(area[il][p] == 2) {
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
		  min= polygon[il][pc][edgs].y1;
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
		    interX[elements] = (nint)(xint);
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
      printf("Wait ... \n");

      for(k=1;k <= Noclass;k++) {
        printf("Creating cell file %d --> ", k);
	for(i=0;i < nrows;i++) {
          G_put_map_row(ftr[k],MAP[k][i]);
	  G_percent(i,nrows,1);
	}
	printf("\n");
      }
      G_clear_screen();

      G_init_colors(&colorg);
      for(k=1;k <= Noclass;k++) {    /* will have to redo this part!!! */
        G_close_cell(ftr[k]);
	G_init_colors(&colorg);
	color = 255 - (nint)((k*1./Noclass)*255.);
	G_set_color((CELL)1,color,color,color,&colorg);
        G_set_color((CELL)0,255,255,255,&colorg); 
	sprintf(tmpstr,"train.cls%d",k);
	G_write_colors(tmpstr,mapset,&colorg);
      }
      G_free_colors(&colorg);

      G_init_cats((CELL)0,"",&cats);
      G_set_cat((CELL)0,"unmasked area",&cats);
      G_set_cat((CELL)1,"masked area",&cats);
      for(k=1;k <= Noclass;k++) {
        sprintf(tmpstr,"train.cls%d",k);
        G_write_cats(tmpstr,&cats);
      }
      G_free_cats(&cats);

/*      addcls = G_yes("Do you want to add another class ? ");
      if(addcls) {
        printf("The number of classes is %d\n", Noclass);
        printf("The program will select %d as the next class\n",Noclass+1);
        sprintf(tmpstr,"train.cls%d",++Noclass); 
        ftr[Noclass] = G_open_cell_new(tmpstr);
	get();
        ask++; nexttime = 1;
      } */

    break;

  case 4:
    printf("Which class do you want displayed ? --> : ");
    scanf("%d",&class);
    getchar();
    printf("Displaying training sites for class %d\n", class);
    sprintf(tmpstr,"train.cls%d",class);
    Dcell(tmpstr,"workspace",1);
    redo = G_yes("Is this OK ? Or do want to delete from the class ?");
    if(redo) /* clean(class); */ printf("Work in progress\n");
    break;

  case 5:
    goto net;
netb: break;

  case 6:
    if(Noclass == 0) {
      printf("How many output classes do you have ? -->: ");
      scanf("%d",&Noclass);
      getchar();
    }
    nntool();
    G_clear_screen();
    break;

  case 7:
    goto random;
ranb:    break;

  case 8:
    goto train;
trainb:	break;

  case 9:
    goto prop;
propb:  break;

  case 10:
    goto display;
disb:  break;

  case 11:
     if(Noclass == 2) {
       printf("Do you want Bayes or Nearest means ? Bayes-0 Means-1 ");
       scanf("%d",&classify);
       getchar();
       if(classify)
         kmeans();
/*       else 
         bayes(); */
     }
     else kmeans();
     break;

  case 12:
    R_close_driver();
    exit(0);
  }
 }
 random: {               /* Start a new block */
   float gen;
   int ran, linenum=0, LINE, no_tr_cols, l,NUMTRAIN,READ=0;
   short *seed48();
   long lrand();
   FILE *ft, *ftnew, *fopen(), *fclose();
   char c, *str, tran[10000][500];

   RANDOM = YES;

   ft = fopen("o_train","r");
   ftnew = fopen("train","w");

   for(i=0;i < 10000;i++)
     for(l=0;l < 500;l++)
       tran[i][l] = 0;

   i = 0; if(numtrain == 0) READ=1;
   while((c = getc(ft)) != EOF) {
     tran[linenum][i++] = c;
     if(c == '\n') {
       if(linenum == 0) no_tr_cols = i;
       linenum++; if(READ) numtrain++;
       i = 0;
     }
   }
   fclose(ft);
 
   for(i=0;i < 100;i++)
     lrand48();

   printf("There are %d number of training samples\n\n",numtrain);

   printf("The # of data points you want randomized -->: ");
   scanf("%d",&NUMTRAIN);
   getchar();
 
   printf("Sampling from training data files -->: ");

   for(i=0;i < NUMTRAIN;i++) {
     ran = lrand48();
     gen = ran*1./pow(2.,31.);
     LINE = gen*linenum;

/*     str = (char *) malloc(sizeof(char)*no_tr_cols);
     for(l=0;l < no_tr_cols;l++)
       str[l] = tran[LINE][l]; */

     fprintf(ftnew,"%s",tran[LINE]);
/*     free(str); */

     G_percent(i,NUMTRAIN,1);
   }
   printf("\n");
   fclose(ftnew);
   goto ranb;
 }
 train: {
     cnet(numfiles,Type_net);
     if(Type_net == 1)
	netmain();
     else quickprop();
     goto trainb;
 }
 net:    {
        printf("What type of network do you want (qp=0,nets=1) ? ");
        scanf("%d",&Type_net);
	getchar();
	goto netb;
 }
 prop: {
     if(Type_net == BP) {
	propout = 1;
	netmain();
	propout=0;
	goto propb;
     }
     else
       printf("For quick prop, the propagation has already been done!\n");
     goto propb;
 }
 display: {
     char dispname[50];

    if(Type_net == QP  || Type_net == LIN) {
      if(numtrain == 0) {
       R_erase();
       printf("Enter the name of the file to display -->: ");
       scanf("%s",dispname);
       getchar();
       Dcell(dispname,"workspace",1);
      }
      else
        draw(numtest);
    }
    else {
      R_erase();
      Dcell(outname,"workspace",1);
    }
    goto disb;
 }
} /* main() */

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


/* checkcategory goes through a local neighborhood to find the
"dominant" category value.  Right now, an attribute array indexed
all the way to maxcategory value is used to count the number of
cells of each category in the neighborhood.  A better scheme exists!! */

checkcategory(row,column,array)
int row, column, **array;
{
int k,l, localvalue, localnum;

	for(k=0;k < maxcategory+2;k++)
	  attr[k] = 0;

	for(k=0;k < row;k++)
	  for(l=0;l < column;l++)
	    attr[array[k][l]]++;

	localnum = 0;
	for(k=0;k < maxcategory;k++)
	  if(attr[k] > localnum) {
	    localnum = attr[k];
	    localvalue = k;
	   }

	return(localvalue);
}

kmeans()
{
int row, col, i, j, cl, lclass;
float mindist, dist;

  OUT = (float **) malloc(sizeof(float)*nrows*ncols);
  for(row=0,j=0;row < nrows;row++) {
    for(col=0;col < ncols;col++,j++) {
      OUT[j] = (float *) malloc(sizeof(float)*(Noclass+1));
      mindist = 10000.; lclass=0;
      for(cl=1;cl <= Noclass;cl++) {
        OUT[j][cl] = 0.; dist = 0.;
        for(i=0;i < numfiles;i++)
	  dist += (kINPUT[i][row][col] - mean[cl][i])*
				(kINPUT[i][row][col] - mean[cl][i]);
        if(mindist > dist) {
          mindist = dist;
          lclass = cl;
        }
      }
      OUT[j][lclass] = 1.0;
    }
  }
  draw(j);
  Type_net = LIN; numtrain=0;
}

nntool() {

/*********************************************************************
This is the gut of the data extraction part of nntool.  Right
now I haven't checked whether bugs exist for averaging (as they are
bound to).  Use that option at your own risk. 
**********************************************************************/

int fd[10], row, col, getrow,i, newi, *fm, q;
int j, qstrt, qend, loop, k, outt, OK, getin, divcnt; /* blnk[400][350];*/
int rcnt, GOOD, BAD, rt, getavg, getmask, trip=0, bit; /*trip = class*/
CELL *pcell, *G_allocate_cell_buf(), *pcellin, *pmask;
CELL *pread[4], *savg[10][25], *pavg;
struct Cell_head *window;
char name[20], *lname[10], *sprintf(), *cellname, outstr[20];
char *strcat(), *str1, *str, *estr, tmpstr[40], **check, *strcheck;
FILE *fopen(), *fclose(), *ft, *fin;
float output, fabs(), tmp, out_trip, out, *kclass;
int max[10], *maxin, min[10], *minin, clnt, ck;
float sum[10][40], T_area[9][150][350], S_A, S_Aprime;
int T_A[20], ii, FOUND, I_A[20], WEIGHT, junk, *num_cl;
long **DATA[25];
struct ar {
	int num;
	int cnt;
	};
struct ar AREA_T[20][25], AREA_I[20][25];

  estr = (char *) malloc(sizeof(char)*5);
  fm = (int *) malloc(sizeof(int)*(Noclass+1));
  AVG = G_yes("Do you want to average out area ?",1);
  if(AVG == YES) {
    DIVROWS = 40;
    DIVCOLS = 9;
  }
  else {
    DIVROWS = nrows;
    DIVCOLS = 1;
  }
  WEIGHT = G_yes("Do you want to weight the categories ?",1);
  thermo = G_yes("Do you want thermometer coding ?",1);
  printf("How many files do you have as input ? ");
  scanf("%d",&numfiles);
  getchar();
  for(i=0;i < numfiles;i++) {
    lname[i] = (char *) malloc(sizeof(char)*20);
    sprintf(tmpstr, "Enter input cell file # %d",i+1);
    mapset = G_ask_cell_old(tmpstr, lname[i]);
    if(mapset == NULL) {
      printf("Trouble reading the mapset! \n");
      exit(0);
    }
    fd[i] = G_open_cell_old(lname[i],mapset);
  }

  for(i=0;i < numfiles;i++) {
    DATA[i] = (long **) malloc(sizeof(long)*nrows);
    for(row=0;row < nrows;row++) {
      DATA[i][row] = (long *) malloc(sizeof(long)*ncols);
      G_get_map_row_nomask(fd[i],DATA[i][row],row);
    }
  }

  FLAG = (int **) malloc(sizeof(int)*nrows);
  for(row=0;row < nrows;row++) {
    FLAG[row] = (int *) malloc(sizeof(int)*ncols);
    for(col=0;col < ncols;col++)
      FLAG[row][col] = 1;
  }
  maxin = (int *) malloc(sizeof(int)*numfiles);
  minin = (int *) malloc(sizeof(int)*numfiles);
  for(i=0;i < numfiles;i++) {
     minin[i] = 10000;  maxin[i] = -10000;
     printf("Reading row data for file #%d -->: ",i+1);
     for(row=0;row < nrows;row++) {
       for(col=0;col < ncols;col++) {
         if(DATA[i][row][col] != 0) bit = 1;
         else bit = 0;
         FLAG[row][col] = FLAG[row][col] * bit;
         if(minin[i] > DATA[i][row][col])
           minin[i] = DATA[i][row][col];
         if(maxin[i] < DATA[i][row][col])
           maxin[i] = DATA[i][row][col];
       }
       G_percent(row,nrows,1);
     }
     printf("\n");
  }

  str = (char *) malloc(sizeof(char)*numfiles*20);
  str1 = (char *) malloc(sizeof(char)*numfiles*ncols*20);
  ft = fopen("o_train","w");
  fin = fopen("input","w");

  mean = (float **) malloc(sizeof(float)*(Noclass+1));
  kclass = (float *) malloc(sizeof(float)*numfiles);
  num_cl = (int *) malloc(sizeof(int)*(Noclass+1));
  for(trip=1;trip <= Noclass;trip++) {  /* this is the mother of all loops */
    mean[trip] = (float *) malloc(sizeof(float)*numfiles);
    num_cl[trip] = 0;
    sprintf(tmpstr,"train.cls%d",trip);
    mapset = G_find_cell(tmpstr,"");
    fm[trip] = G_open_cell_old(tmpstr,mapset);
    pmask = G_allocate_cell_buf();

    sprintf(outstr,"");
    for(k=1;k <= Noclass;k++) {
      if(k == trip) 
        strcat(outstr, "0.900 ");
      else strcat(outstr, "0.100 ");
    }

    for(i=0;i < numfiles;i++) {
      mean[trip][i] = 0.;
      T_A[i] = 0;
      if(WEIGHT == YES) {
        for(k=0;k < 25;k++) {
          AREA_T[i][k].num = 0;
          AREA_T[i][k].cnt = 0;
        }
      }

     for(rcnt=0,row=0;row < nrows && row%skip == 0;row++) {
       loop = NO;

       G_get_map_row(fm[trip],pmask,row);
      
       for(j=0;j < ncols;j++) {
         T_area[i][rcnt][j] = 0.;
         if(pmask[j] > 0) {
	   loop = YES;
	   break;
         }
       }
       if(loop == YES) {
	   for(col=0;col < ncols;col++) {
             if(pmask[col] > 0) {
	        T_area[i][rcnt][col] = (float)DATA[i][row][col];
                if(WEIGHT == YES) {
                  ii=0; FOUND = NO;
	          while(AREA_T[i][ii].num != 0) {
/* Contribution of each category towards the whole training area */
                    if(AREA_T[i][ii].num == DATA[i][row][col]) {
                       AREA_T[i][ii].cnt += 1;
                       FOUND = YES;
                    }
                    ii++;
                  } /* while AREA_T */
		  if(FOUND == NO) {
                    AREA_T[i][ii].num = DATA[i][row][col];
                    AREA_T[i][ii].cnt += 1;
		  }
	        } /* if WEIGHT */
             } /* if pmask[] */
	   } /* for col */
	   rcnt++;
           if(trip == 1) {
            for(col=0;col < ncols;col++) {
	       ii=0; FOUND = NO;
               if(WEIGHT == YES) {
/* Contribution of each category toward the input */
	         while(AREA_I[i][ii].num != 0) {
	           if(AREA_I[i][ii].num == DATA[i][row][col]) {
	             AREA_I[i][ii].cnt += 1;
	             FOUND = YES;
	           }
	           ii++;
	         }
	         if(FOUND == NO) {
	           AREA_I[i][ii].num = DATA[i][row][col];
	           AREA_I[i][ii].cnt += 1;
	         }
               } /* if WEIGHT */
            } /* for col */
           } /* if trip */
        k=0; T_A[i] = 0;
        if(WEIGHT == YES) {
          while(AREA_T[i][k].num != 0) {
            T_A[i] += AREA_T[i][k].cnt; /* Total area */
            k++;
          }
          k=0; I_A[i] = 0;
          while(AREA_I[i][k].num != 0) {
            I_A[i] += AREA_I[i][k].cnt;
            k++;
          }
        } /* if WEIGHT */
      } /* if loop */
      G_percent(row,nrows,1);
    } /* for rcnt */
    printf("\n");
   } /* for(i = 0 -> numfiles) */

/* Find max of all the attribute values in the mask and divide by it to
get scaled values ! For fuzzy numbers (good,bad etc) use 0, 0.5 1.0 etc
Note that the training data are scaled by the input max and min values */

    if(trip == 1) {
       numtrain=0;
       check = (char **) malloc(sizeof(char)*numfiles*nrows*ncols);
       strcheck = (char *) malloc(sizeof(char)*numfiles*30);
    }
    for(row=0;row < rcnt;row++) {
         strcpy(str1,"");
         for(col=0;col < ncols;col++) {
             if(FLAG[row][col] > 0) {
               for(i=0;i < numfiles && T_area[i][row][col] != 0.0;i++) {
  		 if(i == 0)
                   check[numtrain]=(char *) malloc(sizeof(char)*numfiles*30);

                 if(WEIGHT == YES) {
                     q = 0;
                     while(AREA_T[i][q].num != (int)T_area[i][row][col])
                       q++;
 
/* Category contribution in % */
                      S_Aprime = (float)AREA_T[i][q].cnt/(float)T_A[i];
		      S_A = S_Aprime;	
                  } /* if WEIGHT */
                  else S_A = 1.0;

/*   instead of out = S_A*(minin[i] - T_are....)/(.) */

		  out=S_A*fabs((T_area[i][row][col]-minin[i])/
					(double)(maxin[i]-minin[i]));

                   if(thermo == YES) {
		     switch(nint(out*10.)) {
		       case 0:
		sprintf(str," 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 ");
			break;
		       case 1:
		sprintf(str," 0.9 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 ");
			break;
		       case 2:
		sprintf(str," 0.9 0.9 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 ");
			break;
		       case 3:
		sprintf(str," 0.9 0.9 0.9 0.1 0.1 0.1 0.1 0.1 0.1 0.1 ");
			break;
                       case 4:
                sprintf(str," 0.9 0.9 0.9 0.9 0.1 0.1 0.1 0.1 0.1 0.1 ");
                        break;
                       case 5:
                sprintf(str," 0.9 0.9 0.9 0.9 0.9 0.1 0.1 0.1 0.1 0.1 ");
                        break; 
	               case 6:
                sprintf(str," 0.9 0.9 0.9 0.9 0.9 0.9 0.1 0.1 0.1 0.1 ");
                        break;
                       case 7:
                sprintf(str," 0.9 0.9 0.9 0.9 0.9 0.9 0.9 0.1 0.1 0.1 ");
                        break;
                       case 8:
                sprintf(str," 0.9 0.9 0.9 0.9 0.9 0.9 0.9 0.9 0.1 0.1 ");
                        break;
                       case 9:
                sprintf(str," 0.9 0.9 0.9 0.9 0.9 0.9 0.9 0.9 0.9 0.1 ");
                        break;
                       case 10:
                sprintf(str," 0.9 0.9 0.9 0.9 0.9 0.9 0.9 0.9 0.9 0.9 ");
                        break;	
		    } /* switch */
                  } /* thermo */
                  else 
                   sprintf(str,"%4.3f ",out);

                  if(i == 0) {
		    if(Type_net == BP)
		      strcpy(strcheck,"(");
		    else strcpy(strcheck,"");
		  }
                  strcat(strcheck,str);
	          kclass[i] = out;	  
              } /* for i -> numfiles */

              if(strcmp(strcheck,"") != 0) {
                if(Type_net == BP)
                  sprintf(str,"%s)\n",outstr);
		else
		  sprintf(str,"%s\n",outstr);

	/* Check to see whether it's already in the training file! */
                for(ck=0;ck < numtrain;ck++) {
                   if(strcmp(check[ck],strcheck) == 0)
		     break;
	        }
	        if(ck == numtrain) {
                  for(i=0;i < numfiles;i++)
                    mean[trip][i] += kclass[i];
		  strcpy(check[ck],strcheck);
	          strcat(str1,strcheck);
                  strcat(str1,str);
		  num_cl[trip]++; 
                  numtrain++;
		}
              } /* if strcmp */
 	      strcpy(strcheck,"");
           }  /* if FLAG[][] */
        } /* col */
        if(strcmp(str1,"") != 0)
          fprintf(ft,"%s",str1);
    } /* row */
    if(trip == 1 && AVG == NO) {
       for(i=0;i < numfiles;i++) {
         pread[i] = G_allocate_cell_buf();
         kINPUT[i] = (float **) malloc(sizeof(float)*nrows);
	 INPUT[i] = (long **) malloc(sizeof(long)*nrows);
       }
       NO_INPUT=0; numtest=0;
       printf("Writing input data --->: "); /* This is for file input */
       for(row=0;row < nrows;row++) {
         strcpy(str1,"");
         for(i=0;i < numfiles;i++) {
           INPUT[i][row] = (long *) malloc(sizeof(long)*ncols);
	   kINPUT[i][row] = (float *) malloc(sizeof(float)*ncols);
           G_zero_cell_buf(pread[i]);
           getin = G_get_map_row_nomask(fd[i],pread[i],row);
           if(getin == -1) {
             printf("error\n");
             exit(0);
           }
           for(col=0;col < ncols;col++)
 	     INPUT[i][row][col] = pread[i][col];
         }
         for(col=0;col < ncols;col++) {
           if(FLAG[row][col] == YES) {
             numtest++;
             if(Type_net == BP)
               strcat(str1,"( ");
             for(i=0;i < numfiles;i++) {
                if(WEIGHT == YES) {
                  q = 0;
                  while(AREA_I[i][q].num != pread[i][col] && pread[i][col]>0)
                    q++;
                  if(pread[i][col] > 0)
                    S_Aprime = (float)AREA_I[i][q].cnt/(float)I_A[i];
                    else S_Aprime = 0.;
		  S_A = S_Aprime;   /* redundant variable ! */
                } /* WEIGHT */
                else S_A = 1.0;

/* instead of minin[i]-S_A .... */

		out = S_A*fabs((minin[i]-pread[i][col])/
				(double)(maxin[i] - minin[i]));

                kINPUT[i][row][col] = out;
                NO_INPUT++;

		if(thermo == YES) {
		  switch(nint(out*10.)) {
		   case 0:
		sprintf(str," 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 ");
		     break;
		   case 1:
		sprintf(str," 0.9 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 ");
		     break;
		   case 2:
		sprintf(str," 0.9 0.9 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 ");
		     break;
		   case 3:
		sprintf(str," 0.9 0.9 0.9 0.1 0.1 0.1 0.1 0.1 0.1 0.1 ");
		     break;
	 	   case 4:
		sprintf(str," 0.9 0.9 0.9 0.9 0.1 0.1 0.1 0.1 0.1 0.1 ");
		     break;
		   case 5:
		sprintf(str," 0.9 0.9 0.9 0.9 0.9 0.1 0.1 0.1 0.1 0.1 ");
		     break;
		   case 6:
		sprintf(str," 0.9 0.9 0.9 0.9 0.9 0.9 0.1 0.1 0.1 0.1 ");
		     break;
		   case 7:
		sprintf(str," 0.9 0.9 0.9 0.9 0.9 0.9 0.9 0.1 0.1 0.1 ");
		     break;
		   case 8:
		sprintf(str," 0.9 0.9 0.9 0.9 0.9 0.9 0.9 0.9 0.1 0.1 ");
		     break;
		   case 9:
		sprintf(str," 0.9 0.9 0.9 0.9 0.9 0.9 0.9 0.9 0.9 0.1 ");
		     break;
		   case 10:
		sprintf(str," 0.9 0.9 0.9 0.9 0.9 0.9 0.9 0.9 0.9 0.9 ");
		     break;
		  } /* switch */
                } /* thermo */
	        else	
                  sprintf(str,"%6.5f ",out);
                strcat(str1,str);
             } /* for i: numfiles */

	     if(Type_net == BP)
               strcat(str1,")\n");
	     else
               strcat(str1,"\n");
           } /* if FLAG[][] */
         } /* for col */
         if(strcmp(str1,"") != 0) fprintf(fin,"%s",str1);
	 G_percent(row,nrows,1);
       } /* for row */
       printf("\n");
       fclose(fin);
    } /* if trip */
    else if(trip == 1 && AVG == YES) {
     NO_INPUT = 0;
     printf("Writing input data --->: ");
     for(row=0,rcnt=0;row < nrows;rcnt++,row++) {
      G_percent(row,nrows,1);
      if(row != 0 && row % 10 == 0 || row == nrows - 1) {
        for(i=0; i < numfiles;i++)
          for(rt=0;rt < 37;rt++)   /* 37 = ncols/9 */
            sum[i][rt] = 0.;

        for(i=0;i < numfiles;i++) {
          for(rt=0;rt < rcnt;rt++) {
            for(col=0,clnt=0;col < ncols;col++) {
                if(col != 0 && col % DIVCOLS == 0) clnt++;
                if(maxin[i] == 1 || maxin[i] == 2) {
                  switch(DATA[i][rt][col]) {
                      case 0:
                        sum[i][clnt] = 0.0;
                        break;
                      case 1:
                        sum[i][clnt] = 0.9 + sum[i][clnt];
                        break;
                      case 2:
                       sum[i][clnt] = 0.1 + sum[i][clnt];
                        break;
                      default:
                        break;
                  }
                } /* if maxin */
                else {
                  if(WEIGHT == YES) {
                    q = 0;
                    while(AREA_I[i][q].num != DATA[i][rt][col]) q++;
                    S_Aprime = (float)AREA_I[i][q].cnt/(float)I_A[i];
 
                    S_A = S_Aprime;
                  }
                  else S_A = 1.0;

                  sum[i][clnt]=S_A*fabs((double)(DATA[i][rt][col] - minin[i])/
                            (double)(maxin[i] - minin[i])) + sum[i][clnt];
                } /* else */
            } /* col */
          } /* rt */
        } /* i -> numfiles */

        for(col=0;col <= clnt;col++) {
          NO_INPUT++;

	  strcpy(estr,"");
          for(i=0;i < numfiles;i++) {
            if((tmp = sum[i][col]/(DIVROWS*DIVCOLS)) > 1.0) tmp = 1.0;
	    sprintf(tmpstr, "%4.3f ",tmp);
	    strcat(estr,tmpstr);
          } /* i: numfiles */
          if(Type_net == BP)
            fprintf(fin," ( %s )", estr);
          else
             fprintf(fin," %s\n",estr);
        } /* col */
        rcnt = 0;
      } /* if row */
    } /* for row */
    fclose(fin);
   } /* else if trip */
   printf("\n");
   for(i=0;i < numfiles;i++)
     mean[trip][i] = mean[trip][i]/num_cl[trip];
 } /* for trip=1 -> Noclass */
 fclose(ft);
 for(i=0;i < numfiles;i++)
   G_close_cell(fd[i]);
 free(lname);
}

cnet(num,Type_net)
int num, Type_net; {
FILE *fopen(), *fclose(), *fnet;
int Nhidden;

  if(num == 0) {
    printf("Is input data in thermometer coding ? [0/1] ");
    scanf("%d", &thermo);
    getchar();
    printf("How many input files are there ? ");
    scanf("%d", &num);
    getchar();
  }
  if(AVG == NO) {
    DIVROWS = nrows;
    DIVCOLS = 1;
  }

  fnet = fopen("site.net","w");
  if(Type_net == 1)  { /* nets */

    if(Noclass == 0) {
      printf("How many output classes are there ? -->: ");
      scanf("%d",&Noclass);
    }

    printf("How many hidden units do you want ? ");
    scanf("%d",&Nhidden);
    getchar();

    fprintf(fnet,"LAYER : 0\n");
    fprintf(fnet,"NODES : %d\n",(thermo == YES)?num*10:num);
    fprintf(fnet," TARGET : 2\n");

    fprintf(fnet,"LAYER : 1\n");
    fprintf(fnet,"NODES : %d\n",Noclass);

    fprintf(fnet,"LAYER : 2\n");
    fprintf(fnet,"NODES : %d\n",(thermo == YES)?Nhidden*10:num*2);
    fprintf(fnet," TARGET : 1\n");

    fclose(fnet);
  }
  else {
    if(numtrain == 0) {
      printf("Enter the number of training data -->: ");
      scanf("%d",&numtrain);
      getchar();
    }
    if(numtest == 0 && AVG == NO) numtest = ncols*nrows;
    
    if(Noclass == 0) {
      printf("How many classes do you have for output ? -->: ");
      scanf("%d",&Noclass);
    }
    printf("How many hidden units do you want ? ");
    scanf("%d",&Nhidden);

    fprintf(fnet,"#\n");
    fprintf(fnet,"Ninputs %d     Nhidden %d     Noutputs %d\n",
           (thermo==YES)?num*10:num,Nhidden,Noclass);
    fprintf(fnet,"UnitType 2\n");
    fprintf(fnet,"Connectcalls 2\n");
    fprintf(fnet,"1 %d      ",num);
    fprintf(fnet,"%d %d\n",num+1,num+Nhidden);
    fprintf(fnet,"%d %d     ",num+1,num+Nhidden);
    fprintf(fnet,"%d %d\n",num+Nhidden+1,num+Nhidden+Noclass);
    fprintf(fnet,"NTrainingPatterns %d\n",numtrain);
    fprintf(fnet,"NTestPatterns %d\n",numtest);
    fclose(fnet);
  }
}

/* This program asks the user to put in the coordinates for the good
training area and stores the coordinates in a file called GOODAREA. 
Note that overlapping polygons are not being disentangled.  To consider
this see Watkins algorithms etc. in any standard graphics book. */


get()
{
FILE *fopen(), *fd, *fclose();
int endpnt1x[10], endpnt2x, endpnt1y[10], endpnt2y, nxz, nyz;
int nxi, nyi, button, radx, rady, radbut,k, x=0, y=1, edg, ZOOM;
int t, b, l, r, i=0, px, py, pc, cnt=0, MORE=NO, endx, endy, firstx, firsty;
float radius, pow(), sqrt();
double D_d_to_u_row(), D_d_to_u_col(), D_d_to_a_col();
double D_get_d_west(), D_get_d_south(), D_d_to_a_row();
CELL *pcellm, *G_allocate_cell_buf();
struct Cell_head Twindow;
char *tmpstr;

       pc = 1; /* polygon count */
       tmpstr = (char *) malloc(sizeof(char)*20);

label:{
        ZOOM = G_yes("Do you want to zoom ? ",1);/*zooms within zoom needed*/
        if(ZOOM) {
	  printf("Click any button at the UPPER LEFT CORNER of zoom\n");
	  R_get_location_with_pointer(&nxz,&nyz,&button);
	  printf("Close the zoom box\n");
	  R_get_location_with_box(nxz,nyz,&endx,&endy,&button);

	  Twindow.format = window.format;
	  Twindow.compressed = window.compressed;
          if(nyz < endy) {
            Twindow.north = D_d_to_u_row((double)nyz);
	    Twindow.south = D_d_to_u_row((double)endy);
	  }
	  else {
	    Twindow.north = D_d_to_u_row((double)endy);
	    Twindow.south = D_d_to_u_row((double)nyz);
	  }
	  if(nxz < endx) {
	    Twindow.west = D_d_to_u_col((double)nxz);
	    Twindow.east = D_d_to_u_col((double)endx);
	  }
	  else {
	    Twindow.west = D_d_to_u_row((double)endx);
	    Twindow.east = D_d_to_u_row((double)nxz);
	  }
	  Twindow.proj = window.proj;
	  Twindow.zone = window.zone;
	  Twindow.ew_res = window.ew_res;
	  Twindow.ns_res = window.ns_res;
	  Twindow.rows = (nint)((Twindow.north-Twindow.south)/Twindow.ns_res);
	  Twindow.cols = (nint)((Twindow.east-Twindow.west)/Twindow.ew_res);

	  nxz = (nint)(D_d_to_a_col((double)nxz));
	  nyz = (nint)(D_d_to_a_row((double)nyz));

	  G_put_window(&Twindow);	
	  G_set_window(&Twindow);

	  R_close_driver();
	  system("d.frame -e");
	  R_open_driver();

	  Dcell(cellfile,mapset,1);
          if(D_get_screen_window(&t, &b, &l, &r))
             G_fatal_error("Getting screen window");
          if(D_do_conversions(&Twindow,t,b,l,r))
             G_fatal_error("Error in calculating conversions");
        }
	else { nxz = 0; nyz = 0; }
 
        printf("Press -1 to quit \n");
        printf("Is this for point[0], circular[1], or polygonal[2] areas ? ");
        scanf("%d",&area[ask][cnt]); /* if breaking out need to have more 
				   than 1-d array!! */
        if(area[ask][cnt] == -1) return;

	printf("press the right mouse button when done\n");

	nxi = (nint)(D_get_d_west());
	nyi = (nint)(D_get_d_south());
	R_get_location_with_pointer(&nxi,&nyi,&button);	

        if(area[ask][cnt] == 1) { /* Circular */
	  printf("press the left mouse button at the training site\n");
	  if(MORE == NO)  /* Draw a circle in white !!! */
	    i = 0;
	  while(button != 3) {
	    printf("press any button once radius is specified\n");
	    R_get_location_with_line(nxi,nyi,&radx,&rady,&radbut);

	    nxi = (nint)(D_d_to_a_col((double)nxi)) + nxz;
	    nyi = (nint)(D_d_to_a_row((double)nyi)) + nyz;
	    radx = (nint)(D_d_to_a_col((double)radx)) + nxz;
	    rady = (nint)(D_d_to_a_row((double)rady)) + nyz;

	    nxm[ask][no_tr[area[ask][cnt]][ask][cnt]+i] = /*nxz +*/ nxi;   
            nym[ask][no_tr[area[ask][cnt]][ask][cnt]+i] = /*nyz +*/ nyi;
	    rm[ask][no_tr[area[ask][cnt]][ask][cnt]+i] = 
		(nint)(sqrt(pow((radx-nxi)*1.,2.)+pow((rady - nyi)*1.,2.)));

            printf("press the left mouse button at the training site\n");
	    R_get_location_with_pointer(&nxi,&nyi,&button);
	    i++;
	  }
          no_tr[area[ask][cnt]][ask][cnt] = i;
          if(ZOOM) {
	    G_put_window(&window);
	    G_set_window(&window);

	    R_close_driver();
	    system("d.frame -e");
	    R_open_driver();

	    Dcell(cellfile,mapset,1);
            if(D_get_screen_window(&t, &b, &l, &r))
               G_fatal_error("Getting screen window");
            if(D_do_conversions(&window,t,b,l,r))
               G_fatal_error("Error in calculating conversions");
          }
        }
        else if (area[ask][cnt] == 0) { /* Point */
	  printf("press the left mouse button at the training site\n");
          if(MORE == NO)
             i = 0;
          while(button != 3) {
            nxi = (nint)(D_d_to_a_col((double)nxi)) + nxz;
            nyi = (nint)(D_d_to_a_row((double)nyi)) + nyz;  
            nxm[ask][no_tr[area[ask][cnt]][ask][cnt]+i] = nxi;
	    nym[ask][no_tr[area[ask][cnt]][ask][cnt]+i] = nyi;

            printf("press the left mouse button at training site\n");
            R_get_location_with_pointer(&nxi,&nyi,&button);
            i++;
          }
	  no_tr[area[ask][cnt]][ask][cnt] = i;
          if(ZOOM) {
            G_put_window(&window);
	    G_set_window(&window);
            R_close_driver();
	    system("d.frame -e");
	    R_open_driver();
	    Dcell(cellfile,mapset,1);
            if(D_get_screen_window(&t, &b, &l, &r))
               G_fatal_error("Getting screen window");
            if(D_do_conversions(&window,t,b,l,r))
               G_fatal_error("Error in calculating conversions");
          }
        }
	else if(area[ask][cnt] == 2) { /* polygon */
	  printf("Press the left button at the corners\n");
	  edg = 1;  Noedg[ask][pc] = edg;

	  printf("Press the middle button to end polygon\n");

	  while(button != 3) {
	    R_get_location_with_line(nxi,nyi,&endx,&endy,&button);
	    R_move_abs(nxi,nyi);
	    R_cont_abs(endx,endy);
            if(edg == 1) {
              firstx = nxi;	firsty = nyi; /* The 1st location */
            }
	    px = (nint)(D_d_to_a_col((double)nxi));
	    py = (nint)(D_d_to_a_row((double)nyi));
	    polygon[ask][pc][edg].x1 = px + nxz;
	    polygon[ask][pc][edg].y1 = py + nyz;

	    px = (nint)(D_d_to_a_col((double)endx)); 
	    py = (nint)(D_d_to_a_row((double)endy));
	    polygon[ask][pc][edg].x2 = px + nxz; 
            polygon[ask][pc][edg].y2 = py + nyz;
            ++edg;

	    nxi = endx;		nyi = endy;
	    if(button == 2) {
              R_move_abs(nxi,nyi);
	      R_cont_abs(firstx,firsty);

	      polygon[ask][pc][edg].x1 = px + nxz;
              polygon[ask][pc][edg].y1 = py + nyz;
	      polygon[ask][pc][edg].x2 = polygon[ask][pc][1].x1;
	      polygon[ask][pc][edg].y2 = polygon[ask][pc][1].y1;

	      printf("Left button at new location; Right to quit\n");
	      Noedg[ask][pc] = edg; Nopc[ask]++;
	      pc++;
	      R_get_location_with_pointer(&nxi,&nyi,&button);
              edg = 1;
	    }
          }
          if(ZOOM) {
	    G_put_window(&window);
	    G_set_window(&window);

	    R_close_driver();
	    system("d.frame -e");
	    R_open_driver();

	    Dcell(cellfile,mapset,1);
            if(D_get_screen_window(&t, &b, &l, &r))
               G_fatal_error("Getting screen window");
            if(D_do_conversions(&window,t,b,l,r))
               G_fatal_error("Error in calculating conversions");
          }
        }
	  
	printf("You want any other type areas for class %d [0/1] ",ask);
	scanf("%d",&MORE);
        getchar();
       
        if(MORE == YES) {
          cnt++; 
          CNT[ask] = cnt;
	  goto label;
        } /*I know what you CS types are thinking!*/
	else {
	  cnt++;
	  CNT[ask] = cnt;
	}
     }
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

clean(cl) 
int cl;
{
int nx, ny, button, B, mapx, mapy, i;
int dx, dy, numx, numy, succeed, query;
char tmpstr[20];
double D_d_to_a_col(), D_d_to_a_row();

la:	printf("Press at the center of the area you want deleted");
	R_get_location_with_pointer(&nx,&ny,&button);

	mapx = (nint)(D_d_to_a_col((double)nx));
	mapy = (nint)(D_d_to_a_row((double)ny));
   
	numx = numy = 0; B =0;
	while(B == 0) {
		numx++; numy++;
		succeed = 0;
    		for(dy=-numy;dy < numy;dy++)
			for(dx=-numx;dx < numx;dx++)
				if(MAP[cl][nx+dx][ny+dy] != 0) {
					succeed++;
					MAP[cl][nx+dx][ny+dy] = 0;
				}
		if(succeed == 0)
			B = 1;
	}

	query = G_yes("Do you want to delete any more ?");
	if(query == 1)
		goto la;
	else {
		for(i=0;i < nrows;i++)
			G_put_map_row(ftr[cl],MAP[cl][i]);
	}

	sprintf(tmpstr,"train.cls%d",cl);
    	Dcell(tmpstr,"workspace",1);
}
