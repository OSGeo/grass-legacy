#include "globals.h"
#define NUM_CHARS 30
#define BORDER 30
#define TEXT_HEIGHT (width/NUM_CHARS + 2)

static int use = 1, TEMP=NO, count=0;
extern float CLASS[10][10][1000], **NCLS[2];
float temp[10][10][1000];
int d_top, d_left, d_right, height, width, d_bottom; 

class() {
int class_dist(),recall(),new(),class_done(),class_del(),driver();

	static Objects objects[] =
	{
		INFO("Class:",&use),
		MENU(" Show ", class_dist, &use),
		MENU(" Delete ", class_del, &use),
		MENU(" Done ", class_done, &use),
		MENU(" Main ", driver, &use),
		{0}
	};

	Input_pointer(objects);
	Menu_msg("");
}

class_dist() 
{
int DELx, DELy, x1, y1, x2, y2, cl, i, red, green, blue, num;
extern int Noclass, *num_cl, SAMPLED, DELETE, numfiles, thermo,numtrain;
extern int DEL_TRAIN;
View *VIEW_DIST;
char msg[20], sdel[10];

	if(SAMPLED == NO) {
		G_warning("Sample training classes first!");
		driver();
	}
	if(numfiles > 2) {
		G_warning("Greater than 2 input files!");
		driver();
	}
	if(thermo == YES) {
		G_warning("Thermometer coding!");
		driver();
	}

	VIEW_DIST = VIEW_HISTO;

        Erase_view(VIEW_DIST);
	Outline_box(VIEW_DIST->top, VIEW_DIST->bottom, VIEW_DIST->left,
			VIEW_DIST->right);
	R_set_window(VIEW_DIST->top, VIEW_DIST->bottom, VIEW_DIST->left, 
                        VIEW_DIST->right);

	width = VIEW_DIST->ncols - 50;
	height = VIEW_DIST->nrows - 150;

	R_text_size(3*TEXT_HEIGHT/4, TEXT_HEIGHT);

	d_top = VIEW_DIST->top + BORDER+ 10;
	d_left = VIEW_DIST->left + BORDER;
	d_right = d_left + width - 1; 
	d_bottom = d_top + height - 1;

	R_standard_color(GREEN);
	sprintf(msg,"%s","CLASSES");
	R_move_abs(VIEW_DIST->left + 100, VIEW_DIST->top + TEXT_HEIGHT);
	R_text(msg);

        if(count) {
	  sprintf(msg,"%s %d","#samples: ",DEL_TRAIN);	
	  R_move_abs(VIEW_DIST->left+200, VIEW_DIST->top + TEXT_HEIGHT);
	  R_text(msg);
	}

	x1 = d_left; y1 = y2 = d_bottom;
	x2 = d_right;

	R_move_abs(x1 - 10, y1 + 10);
	R_text("0.0");
	R_move_abs(x1, y1);  /* draw horizontal axis */
	R_cont_abs(x2, y2); R_text("1.0");
	R_move_abs(x2 - 50, y2 + 20);
	R_text("X1");

	x2 = d_left; y2 = d_top;
	R_move_abs(x1, y1); /* draw vertical axis */
	R_cont_abs(x2, y2); R_text("1.0");
	R_move_abs(x2 - 20, y2 - 20);
	R_text("X2");

	TEMP = YES;

	if(DELETE && TEMP)
		Curses_prompt_gets("Display (Old = O) (Delete = D) ?", sdel);

        for(cl=0;cl < 10;cl++)
		for(i=0;i < 1000;i++)
			temp[0][cl][i] = temp[1][cl][i] = 0.;

	DELx = 2;	DELy = 2;
	for(cl=1;cl <= Noclass;cl++) {
		for(i=0;i < num_cl[cl];i++) {
			red = (nint)((cl*1.0)/Noclass*255);
			green = (nint)((cl*1.0)/Noclass*255);
			blue = (nint)((cl*1.0)/Noclass*255);
			R_reset_color(red, green, blue, num);
			R_color(num);
			if(sdel[0] == 'O' || sdel[0] == 'o' || DELETE == 0) {
				x1 = d_left + (nint)(CLASS[0][cl][i]*width);
				y1 = d_bottom - (nint)(CLASS[1][cl][i]*height);
				temp[0][cl][i] = CLASS[0][cl][i];
				temp[1][cl][i] = CLASS[1][cl][i];
			}
			else if (sdel[0] == 'D' || sdel[0] == 'd') {
			   if(NCLS[0][cl][i]>0. && NCLS[1][cl][i]>0.) {
				x1 = d_left + (nint)(NCLS[0][cl][i]*width);
				y1 = d_bottom - (nint)(NCLS[1][cl][i]*height);
				temp[0][cl][i] = NCLS[0][cl][i];
				temp[1][cl][i] = NCLS[1][cl][i];
			   }
			}
			R_move_abs(x1, y1);
			R_box_rel(DELx, DELy);
		}
		R_flush();
	}
	R_standard_color(WHITE);
	R_text_size(3*NORMAL_TEXT_SIZE/4,NORMAL_TEXT_SIZE);
	R_set_window (SCREEN_TOP, SCREEN_BOTTOM, SCREEN_LEFT, SCREEN_RIGHT);
	TEMP = NO;
	use_mouse_msg();
	class();
}

class_del ()
{
int ncl, cl, j, k, i=0, d;
extern int Noclass, *num_cl;
int butt0, butt1, butt2, x0, y0, x1, y1, x2, y2;
struct xy { float x1, y1, x2, y2};
struct xy cl_del[200];

  count = 0;
  if(SAMPLED == NO) {
                G_warning("Read training classes first!");
                driver();
  }

  use_mouse_msg();

  for(cl=1;cl <= Noclass;cl++)
	for(k=0;k < num_cl[cl];k++) {
			NCLS[0][cl][k] = temp[0][cl][k];
			NCLS[1][cl][k] = temp[1][cl][k];
	}

  R_standard_color(YELLOW);
  while(butt0 != 3) {
     Menu_msg("(Left = NW corner) (Left/Middle = Complete) (Right = Quit)");
     Mouse_pointer(&x1, &y1, &butt1);
     if(butt1 == 3) break;
     Mouse_box_anchored(&x1, &y1, &x2, &y2, &butt2);
     Outline_box(y1,y2,x1,x2);
     Menu_msg("(Middle = ACCEPT) (Left/Right = REJECT)");
     Mouse_pointer(&x0, &y0, &butt0);
     if(butt0 == 2) {
        cl_del[i].x1 = (x1 - d_left)*1.0/width; 
	cl_del[i].x2 = (x2 - d_left)*1.0/width;
        cl_del[i].y1 = (d_bottom - y1)*1.0/height;
	cl_del[i].y2 = (d_bottom - y2)*1.0/height;
	i++;
     }
  }
  for(k=0;k < i;k++)
    for(cl=1;cl <= Noclass;cl++)
     for(d=0;d < num_cl[cl];d++) {
       if(temp[0][cl][d] > cl_del[k].x1 && temp[0][cl][d] < cl_del[k].x2 
       && temp[1][cl][d] > cl_del[k].y2 && temp[1][cl][d] < cl_del[k].y1) {
		NCLS[0][cl][d] = NCLS[1][cl][d] = 0.;
       		count++;
	}
     }
  TEMP = YES;
  count = numtrain - count;
  class();
}

class_done() {
FILE *fnewt;
char ssave[10], *outstr;
int save, cl, k, d;
extern int DELETE, DEL_TRAIN;

  if(SAMPLED == NO) {
	G_warning("Read training classes first!");
        driver();
  }

  fnewt = fopen("d_train","w");
  outstr = (char *) malloc(sizeof(char)*Noclass*10);
  Curses_prompt_gets("Save changes ? [Y/N] ", ssave); 
  if(ssave[0] == 'Y' || ssave[0] == 'y') {
    for(cl=1,DEL_TRAIN=1;cl <= Noclass;cl++) {
      strcpy(outstr,"");
      for(k=1;k <= Noclass;k++) {
	if(k == cl)
	  strcat(outstr,"0.900 ");	
	else strcat(outstr,"0.100 ");
      }
      for(d=0;d < num_cl[cl];d++)
	  if(NCLS[0][cl][d] > 0. && NCLS[1][cl][d] > 0.) {
		fprintf(fnewt,"%5.4f %5.4f %s\n",NCLS[0][cl][d],NCLS[1][cl][d],
				outstr);
		DEL_TRAIN++;
	  }
    }
    DELETE = YES; 
  }
  fclose(fnewt);
  class();
}
