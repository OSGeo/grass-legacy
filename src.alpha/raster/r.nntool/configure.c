#include "globals.h"

static int TESTED, TRAINED, use=1, Type_net;

configure()
{
int train(), test(), raster(), cmain(), random();
extern int numfiles, numtest, ncols, nrows, DELETE, DEL_TRAIN;
char temp[20];

static Objects objects[] =
	{
	INFO("NN:",&use),
        MENU(" Random ",random,&use),
	MENU(" Train ",train,&use),
	MENU(" Test ",test,&use),
   	MENU(" Raster ",raster,&use),
	MENU(" Main ",cmain,&use),
	{0}
	};

	Curses_prompt_gets("What type network ? (qp=0) (nets=1) ", temp);
	Type_net = atoi(temp);
	cnet(numfiles,Type_net);
	TRAINED = TESTED = NO;

	Input_pointer(objects);
	Menu_msg("");
}

cmain()
{
	driver();
}

train()
{
	if(SAMPLED == NO) {
		G_warning("You need to sample data first!");
		return(0);
	}

        if(Type_net == 1)
	  netmain();
        else quickprop();

	TRAINED = YES;
	use_mouse_msg();
	return(0);
}


test()
{
extern int numtest, ncols, nrows, TEST(), propout;

	if(TRAINED == NO) {
		Curses_write_window(PROMPT_WINDOW, 1, 1, "");
		Curses_write_window(PROMPT_WINDOW, 1, 1,
					"Need to train network first");
		G_warning("Need to train network first");
		return(0);
	}

	if(numtest == 0) numtest = ncols*nrows;

	if(Type_net == 1) {
		propout = 1;
		netmain();
		propout = 0;
		TESTED = YES;
	}
	else {
		TEST();
		TESTED = YES;
	}

	use_mouse_msg();
	return(0);
}

raster()
{
	if(TESTED == YES)
		draw(numtest);
	else {
		Curses_write_window(PROMPT_WINDOW, 1, 1, "");
		Curses_write_window(PROMPT_WINDOW, 1, 1,
					"Need to test network first");
	}
	use_mouse_msg();
	return(0);
}

cnet(num, Type_net)
int num, Type_net; 
{
FILE *fopen(), *fnet;
extern int thermo, numtrain, Noclass, ncols, nrows;
extern int  numtest, Ninput,Nhidden,Noutput;
char temp[20];

  if(num == 0) {
    Curses_prompt_gets("Input data in thermometer coding ? [0/1] ", temp);
    thermo = atoi(temp);
    Curses_prompt_gets("How many input files are there ? ", temp);
    num = atoi(temp);
  }

  fnet = fopen("site.net","w");
  if(Type_net == 1)  { /* nets */
 
    if(Noclass == 0) {
      Curses_prompt_gets("How many output classes ? ", temp);
      Noclass = atoi(temp);
    }
      
    Curses_prompt_gets("How many hidden units do you want ? ", temp);
    Nhidden = atoi(temp);

    if(thermo == YES) {Ninput = num*10; Nhidden=Nhidden*10;}
    else Ninput = num;
    Noutput = Noclass;

    use_mouse_msg();

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
    if(numtest == 0 || numtrain == 0) {
	Curses_prompt_gets("Enter the number of training data : ", temp);
	numtrain = atoi(temp);

	Curses_prompt_gets("Take default #input ? (Yes=1) (No=0) ",temp);
	if(atoi(temp))
	   numtest = ncols*nrows;
	else {
	  Curses_prompt_gets("Enter the number of input pairs : ", temp);
	  numtest = atoi(temp);
	}
    }
    if(Noclass == 0) {
      Curses_prompt_gets("How many classes do you have for output ? ",temp);
      Noclass = atoi(temp);
    }
    Curses_prompt_gets("How many hidden units do you want ? ",temp);
    Nhidden = atoi(temp);

    if(thermo == YES) {Ninput = num*10; Nhidden=Nhidden*10;}
    else Ninput = num;
    Noutput = Noclass;

    use_mouse_msg();
 
    fprintf(fnet,"#\n");
    fprintf(fnet,"Ninputs %d     Nhidden %d     Noutputs %d\n",
           (thermo==YES)?num*10:num,Nhidden,Noclass);
    fprintf(fnet,"UnitType 2\n");
    fprintf(fnet,"Connectcalls 2\n");
    fprintf(fnet,"1 %d      ",num);
    fprintf(fnet,"%d %d\n",num+1,num+Nhidden);
    fprintf(fnet,"%d %d     ",num+1,num+Nhidden);
    fprintf(fnet,"%d %d\n",num+Nhidden+1,num+Nhidden+Noclass);
    fprintf(fnet,"NTrainingPatterns %d\n",(DELETE==NO)?numtrain:DEL_TRAIN);
    fprintf(fnet,"NTestPatterns %d\n",numtest);
    fclose(fnet);
  }
}
