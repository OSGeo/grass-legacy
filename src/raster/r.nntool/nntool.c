/*************************************************************************
This program called r.nntool was written by Ranjan S. Muttiah
while he was a student at Purdue University and while working for Texas A&M
at the Blackland Research Center on the HUMUS project sponsored
by the SPA division of the Soil Conservation Service.  All errors in this
program are due to the author and are NOT the reponsibility of Purdue 
University, Texas A&M University, or the Soil Conservation Service.

The neural network programs used are called QUICKPROP and NETS.  Quickprop
was developed by Scott Fahlman at Carnegie Mellon University, and NETS was
developed by Paul Baffes at the Johnson Space Center of NASA.

If you use this program and report results, the author would appreciate
mention of his program.  Report any problems, bugs, or any successful 
applications to the author.

email: muttiah@brcsun1.tamu.edu
*****************************************************************************/
#include <stdio.h>
#include <math.h>
#define YES 1
#define NO 0
#define QP 0
#define BP 1
#define LIN 2
#define skip 1 /* # of rows to skip for sampling */
#define MAIN
#define GLOBAL
#include "globals.h"

int propout=0,rm[20][100],no_tr[3][20][20],NO_INPUT=0;
int numtrain=0, Type_net, numtest, RANDOM, maxcategory=0, *store;
int area[25][50], thermo, Noclass, ask, nexttime, *attr, *actual;
int nxz, nyz, Noedg[25][100], *ftr, numfiles, nxm[20][100], cnt, pc;
int **FLAG, resolve, nym[20][100], *CNT, *Nopc, *num_cl, DELETE, DEL_TRAIN;
int nrows, ncols, numfiles, *CNT, error(), Nhidden, Ninput, Noutput;
char outname[50], cellfile[20], *lname[20], *Gmapset[20], *Omapset, group[10];
struct Cell_head window, cellhd;
struct coord polygon[25][50][50];
float **OUT, **kINPUT[10];
long **MAP[10], **INPUT[10];

main()
{
CELL cell, *G_allocate_cell_buf();
struct Cell_head *new_window;
int x, y, nxi, nyi, button, ans, num=1, k=0, i=0, xp, Yscan;
int bot[1], top[1], left[1], rite[1], num2=2, xmax, xmin;
int topw, botw, leftw, ritew, il, kk, irow, jcol, addon;
int polyx[8], polyy[8], nxe, nye, class, addcls, redo;
int scr_t_w, scr_b_w, scr_l_w, scr_r_w, color, max, min, ypls1, ymns1;
int x11, y11, x22, y22, elements, pc, edgs, *Ymax, *Ymin;
unsigned char red, green, blu;
int bc, tc, dc, topscr=100, leftscr=100, size=3;
int t, b, l, r, localvalue, lumpcol, lumprow, j, ii, jl, jj, p,minedg;
char *name1, tmpstr[50];
char lumpfile[20], lumpedfile[20], *tempfile;
float radius, xint;
double D_d_to_u_row(), D_d_to_u_col(), D_d_to_a_col();
double D_get_d_west(), D_get_d_south(), D_d_to_a_row();
CELL *pcellm, *pcell;

 	G_gisinit("nntool");
 	R_open_driver();

	/* remove any old mask */
	remove_mask();
	
	/* initialize the region */
	init_region();

	tempfile = G_tempfile();
	interrupt_char = G_intr_char();

	/* initialize the graphics */
	g_init();

	/* set up signal handling */
	set_signals();

	/* put out a title */
	display_title(VIEW_MAP1);

	Omapset = (char *) malloc(sizeof(char)*2);
	Omapset=G_ask_cell_old("Enter name of the output cell file", cellfile);
	if(Omapset == NULL) exit(0);
        if(G_get_cellhd(cellfile,Omapset,&cellhd) != 0)
		G_fatal_error("Did not find input cell map");
	
	G_adjust_window_to_box(&cellhd,&VIEW_MAP1->cell.head, VIEW_MAP1->nrows,
				VIEW_MAP1->ncols);
	Configure_view(VIEW_MAP1, cellfile, Omapset, cellhd.ns_res, 
			cellhd.ew_res);		
	G_adjust_window_to_box(&cellhd,&VIEW_MASK1->cell.head, 
			VIEW_MASK1->nrows, VIEW_MASK1->ncols);
	Configure_view(VIEW_MASK1, "MASK", G_mapset(), cellhd.ns_res, 
			cellhd.ew_res);
	draw_cell(VIEW_MAP1, OVER_WRITE);

	G_set_window(&cellhd);
        window = cellhd;
        if (D_get_screen_window(&t, &b, &l, &r))
                G_fatal_error("Getting screen window");
        if(D_do_conversions(&window,t,b,l,r))
                G_fatal_error("Error in calculating conversions");

	nrows = cellhd.rows;	ncols = cellhd.cols;

        printf("How many classes do you have for output ? ");
	scanf("%d",&Noclass);
	getchar();

	store = (int *) malloc(sizeof(int)*(Noclass+1));
	actual = (int *) malloc(sizeof(int)*(Noclass+1));

	for(i=1;i <= Noclass;i++) {
		store[i] = 0;
		printf("What is the attr # for class %d ? -->: ",i);
		scanf("%d",&actual[i]);
	}

	printf("How many files do you have as input ? ");
	scanf("%d",&numfiles);
	getchar();

  	for(i=0;i < numfiles;i++) {
    		lname[i] = (char *) malloc(sizeof(char)*20);
    		sprintf(tmpstr, "Enter input cell file # %d",i+1);
    		Gmapset[i] = G_ask_cell_old(tmpstr, lname[i]);
    		if(Gmapset[i] == NULL) {
      			printf("Trouble reading the mapset! \n");
      			exit(0);
    		}
	}

	printf("Do you have imagery the group set up ? ");
	scanf("%c",group);
	getchar();
	if(group[0] == 'y' || group[0] == 'Y') {
		ask_sig();
		if(numfiles != Sigs.nbands) {
			printf("Incompatible number of Sig files!\n");
			exit(0);
		}
	}
	
	Begin_curses();
	Curses_clear_window (PROMPT_WINDOW);
	G_set_error_routine(error);

        ftr = (int *) malloc(sizeof(int)*(Noclass+10));
	CNT = (int *) malloc(sizeof(int)*(Noclass+10));
	Nopc = (int *) malloc(sizeof(int)*(Noclass+10));
        for(k=1;k <= Noclass;k++) {
            Nopc[k] = 0;
	    MAP[k] = (long **) malloc(sizeof(long)*nrows);
            for(i=0;i < nrows;i++) {
	      MAP[k][i] = (long *) malloc(sizeof(long)*ncols);
	      for(j=0;j < ncols;j++)
		MAP[k][i][j] = 0;
	    }
        }

	SA=0;	/* The number of training areas */
	driver();
	quit();
}
      
linear()
{
int classify;
char tmpstr[10];

     if(Noclass == 2) {
       Curses_prompt_gets("(Bayes = 0) (Nearest means = 1) ? ",tmpstr);
       classify = atoi(tmpstr);
       if(classify)
         kmeans();
/*       else 
         bayes(); */
     }
     else kmeans();

     driver();
}

random() 
{
   float gen;
   extern int DELETE;
   int ran, linenum=0, LINE, no_tr_cols, l,NUMTRAIN,READ=0,i;
   FILE *ft, *ftnew, *fopen();
   char c, *str, tran[10000][500], tmpstr[30];

   RANDOM = YES;

   if(DELETE == YES)
	ft = fopen("d_train","r");
   else
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

   if(DELETE == YES)
   	sprintf(tmpstr,"Training Samples: %d",DEL_TRAIN);
   else
   	sprintf(tmpstr,"Training Samples: %d",numtrain);
   Menu_msg(tmpstr);

/*   Curses_write_window(PROMPT_WINDOW, 1, 1, tmpstr); */

   Curses_prompt_gets("The # of data points you want randomized : ",tmpstr);
   NUMTRAIN = atoi(tmpstr);
 
   Menu_msg("Sampling from training data files ");

   for(i=0;i < NUMTRAIN;i++) {
     ran = lrand48();
     gen = ran*1./pow(2.,31.);
     LINE = gen*linenum;

     fprintf(ftnew,"%s",tran[LINE]);
   }
   fclose(ftnew);
   return(0);
}

kmeans()
{
int row, col, i, j, cl, lclass;
float mindist, dist=0.;

  OUT = (float **) malloc(sizeof(float)*nrows*ncols);
  Menu_msg("");
  Menu_msg("Standby: classifying data");
  for(row=0,j=0;row < nrows;row++) {
    for(col=0;col < ncols;col++) {
      if(FLAG[row][col] > 0) {
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
      	OUT[j][lclass] = 1.0; j++;
      }
    }
    G_percent(row,nrows,1);
  }
  draw(j);
  Type_net = LIN; numtrain=0;
}

nntool() 
{
/*********************************************************************
This is the guts of the data extraction part of nntool.
**********************************************************************/

int row, col, getrow, i, newi, *fm, q, fd[10];
int j, loop, k, getin, left, top;
int rcnt, GOOD, BAD, rt, getavg, getmask, trip=0, bit; /*trip = class*/
CELL *pcell, *G_allocate_cell_buf(), *pcellin, *pmask;
CELL *pread[10], *savg[10][25], *pavg;
char name[20], *cellname, outstr[100], *mapset;
char *strcat(), *str1, *str, *estr, tmpstr[50], **check, *strcheck;
FILE *fopen(), *ft, *fin;
float output, tmp, out_trip, out, *kclass;
int max[10], *maxin, min[10], *minin, clnt, ck, foutf, srow[500];
float sum[10][40], T_area[5][500][500], S_A, S_Aprime;
int T_A[20], ii, FOUND, I_A[20], WEIGHT, junk;
long **DATA[10], **OUTPUT;
struct ar {
	int num;
	int cnt;
	};
struct ar AREA_T[20][500], AREA_I[20][500];

  estr = (char *) malloc(sizeof(char)*5);
  fm = (int *) malloc(sizeof(int)*(Noclass+1));

/*  Curses_prompt_gets("Do you want to weight the categories [0/1] ? ", name);
  WEIGHT = atoi(name);
  Curses_prompt_gets("Do you want thermometer coding [0/1] ? ", name);
  thermo = atoi(name); */
  WEIGHT = 0; thermo = 0;

  foutf = G_open_cell_old(cellfile,Omapset);
  G_set_window(&cellhd);  /* &window */
  OUTPUT = (long **) malloc(sizeof(long)*nrows);
  for(row=0;row < nrows;row++) {
        OUTPUT[row] = (long *) malloc(sizeof(long)*ncols);
	G_get_map_row_nomask(foutf,OUTPUT[row],row);
  }

  for(i=0;i < numfiles;i++) {
    fd[i] = G_open_cell_old(lname[i], Gmapset[i]);
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
     sprintf(tmpstr,"Reading row data for file #%d ",i+1);
     Menu_msg(tmpstr);
     for(row=0;row < nrows;row++) {
       for(col=0;col < ncols;col++) {
         if(DATA[i][row][col] != 0 && OUTPUT[row][col] != 0)
		bit = 1;
         else 
		bit = 0;
         FLAG[row][col] = FLAG[row][col] * bit;
         if(minin[i] > DATA[i][row][col])
           minin[i] = DATA[i][row][col];
         if(maxin[i] < DATA[i][row][col])
           maxin[i] = DATA[i][row][col];
       }
     }
  }

  if(WEIGHT == YES)
    for(i=0;i < numfiles;i++) {
	I_A[i] = 0;
  	for(row=0;row < nrows;row++) {
		for(col=0;col < ncols;col++) {
		   if(FLAG[row][col] == YES) {
			ii=0; FOUND = NO;
			while(AREA_I[i][ii].num != 0) {
				if(AREA_I[i][ii].num == DATA[i][row][col]) {
					AREA_I[i][ii].cnt += 1;
					FOUND = YES;
					I_A[i]++;
				}
				ii++;
			}
			if(FOUND == NO && DATA[i][row][col] != 0) {
				AREA_I[i][ii].num = DATA[i][row][col];
				AREA_I[i][ii].cnt += 1;
				I_A[i]++;
			}
		   } /* FLAG */
		} /* ncols */
	} /* nrows */
    } /* numfiles */

  str = (char *) malloc(sizeof(char)*numfiles*20);
  str1 = (char *) malloc(sizeof(char)*numfiles*ncols*20);
  ft = fopen("o_train","w");
/*  fin = fopen("input","w"); */

  mean = (float **) malloc(sizeof(float)*(Noclass+1));
  kclass = (float *) malloc(sizeof(float)*numfiles);
  num_cl = (int *) malloc(sizeof(int)*(Noclass+1));
  NCLS[0] = (float **) malloc(sizeof(float)*(Noclass+1));
  NCLS[1] = (float **) malloc(sizeof(float)*(Noclass+1));
  for(trip=1;trip <= Noclass;trip++) {  /* this is the mother of all loops */
   mean[trip] = (float *) malloc(sizeof(float)*numfiles);
   num_cl[trip] = 0;
   sprintf(tmpstr,"train.cls%d",trip);
/*   mapset = G_find_cell(tmpstr,""); */
   fm[trip] = G_open_cell_old(tmpstr,"workspace"); /* training sites */
   if(fm[trip] == 0) {
		G_warning("No training map layer!");
		driver();
   }
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
        for(k=0;k < 500;k++) {
          AREA_T[i][k].num = 0;
          AREA_T[i][k].cnt = 0;
        }
     }

     for(rcnt=0,row=0;row < nrows && row%skip == 0;row++) {
       loop = NO;
       G_get_map_row(fm[trip],pmask,row);
       for(j=0;j < ncols;j++)
	  T_area[i][rcnt][j] = 0.;
       for(j=0;j < ncols;j++) {
         if(pmask[j] > 0) {
	   loop = YES;
	   break;
         }
       }
       if(loop == YES) {
	   srow[rcnt] = row;
	   for(col=0;col < ncols;col++) {
             if(pmask[col] > 0 && DATA[i][row][col] != 0) {
	        T_area[i][rcnt][col] = (float)(DATA[i][row][col]);
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
		  if(FOUND == NO && DATA[i][row][col] != 0) {
                    AREA_T[i][ii].num = DATA[i][row][col];
                    AREA_T[i][ii].cnt += 1;
		  }
	        } /* if WEIGHT */
             } /* if pmask[] */
	   } /* for col */
	rcnt++;
        k=0; T_A[i] = 0;
        if(WEIGHT == YES) {
          while(AREA_T[i][k].num != 0) {
            T_A[i] += AREA_T[i][k].cnt; /* Total area */
            k++;
          }
        } /* if WEIGHT */
       } /* if loop */
     } /* for rcnt */
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
             if(FLAG[srow[row]][col] > 0) {
               for(i=0;i < numfiles && T_area[i][row][col] != 0.;i++) {
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

		  out=S_A*(T_area[i][row][col] - minin[i])/
				(double)(maxin[i] - minin[i]);

                  if(thermo == YES) {
		     switch((int)(out*10.)) {
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
                  for(i=0;i < numfiles;i++) {
                    mean[trip][i] += kclass[i];
		    CLASS[i][trip][num_cl[trip]] = kclass[i];
		  }
		  strcpy(check[ck],strcheck);
	          strcat(str1,strcheck);
                  strcat(str1,str);
		  num_cl[trip]++; 	/* # of samples in training class */
                  numtrain++;
		}
              } /* if strcmp */
 	      strcpy(strcheck,"");
           }  /* if FLAG[][] */
        } /* col */
        if(strcmp(str1,"") != 0)
          fprintf(ft,"%s",str1);
   } /* row */
   if(trip == 1) {
       for(i=0;i < numfiles;i++) {
         pread[i] = G_allocate_cell_buf();
         kINPUT[i] = (float **) malloc(sizeof(float)*nrows);
	 INPUT[i] = (long **) malloc(sizeof(long)*nrows);
       }
       NO_INPUT=0; numtest=0;
       Menu_msg("Writing input data "); /* This is for file input */
       for(row=0;row < nrows;row++) {
         strcpy(str1,"");
         for(i=0;i < numfiles;i++) {
           INPUT[i][row] = (long *) malloc(sizeof(long)*ncols);
	   kINPUT[i][row] = (float *) malloc(sizeof(float)*ncols);
           G_zero_cell_buf(pread[i]);
           getin = G_get_map_row_nomask(fd[i],pread[i],row);
           if(getin == -1) {
             G_warning("error");
             exit(0);
           }
           for(col=0;col < ncols;col++)
 	     INPUT[i][row][col] = pread[i][col];
         }
         for(col=0;col < ncols;col++) {
           if(FLAG[row][col] == YES) {
             numtest++;
             for(i=0;i < numfiles;i++) {
                if(WEIGHT == YES) {
                  q = 0;
                  while(AREA_I[i][q].num != pread[i][col]&&pread[i][col] != 0)
                    q++;
                  if(pread[i][col] != 0)
                    S_Aprime = (float)AREA_I[i][q].cnt/(float)I_A[i];
                    else S_Aprime = 0.;
		  S_A = S_Aprime;   /* redundant variable ! */
                } /* WEIGHT */
                else S_A = 1.0;

/* instead of minin[i]-S_A .... */

		out = S_A*(pread[i][col] - minin[i])/
			(double)(maxin[i] - minin[i]);

                kINPUT[i][row][col] = out;
                NO_INPUT++;

		if(thermo == YES) {
		  switch((nint)(out*10.)) {
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
             strcat(str1,"\n");
           } /* if FLAG[][] */
         } /* for col */
         /* if(strcmp(str1,"") != 0) fprintf(fin,"%s",str1); */
	 G_percent(row,nrows,1);
       } /* for row */
/*       fclose(fin); */
   } /* if trip */
   for(i=0;i < numfiles;i++)
     mean[trip][i] = mean[trip][i]/num_cl[trip];

   NCLS[0][trip] = (float *) malloc(sizeof(float)*num_cl[trip]);
   NCLS[1][trip] = (float *) malloc(sizeof(float)*num_cl[trip]);
   for(i=0;i < num_cl[trip];i++)
	NCLS[0][trip][i] = NCLS[1][trip][i] = 0.;

  } /* for trip=1 -> Noclass */
  fclose(ft);
  for(i=0;i < numfiles;i++)
    G_close_cell(fd[i]);
  free(lname); free(OUTPUT); free(DATA[0]); free(DATA[1]);
}

error(msg, fatal)
char *msg;
{
  char buf[200];
  int x,y,button;
 
  Curses_clear_window (PROMPT_WINDOW);
  Curses_write_window (PROMPT_WINDOW,1,1, "LOCATION:\n");
  Curses_write_window (PROMPT_WINDOW,1,12,G_location());
  Curses_write_window (PROMPT_WINDOW,2,1, "MAPSET:\n");
  Curses_write_window (PROMPT_WINDOW,2,12,G_location());
  Beep();
  if (fatal)
    sprintf (buf, "ERROR: %s", msg);
  else
    sprintf (buf, "WARNING: %s (click mouse to continue)", msg);
  Menu_msg (buf);
    
  if (fatal) {
    quit();
  }
  Mouse_pointer (&x, &y, &button);
  Curses_clear_window (PROMPT_WINDOW);
}

quit()
{
extern char group[10];
 
    if(group[0] == 'Y' || group[0] == 'y')
       write_signatures();
 
    End_curses();
    R_close_driver();
}
