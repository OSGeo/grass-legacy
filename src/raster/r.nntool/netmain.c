/*=============================*/
/*           NETS              */
/*                             */
/* a product of the AI Section */
/* NASA, Johnson Space Center  */
/*                             */
/* principal author:           */
/*       Paul Baffes           */
/*                             */
/* contributing authors:       */
/*      Bryan Dulock           */
/*      Chris Ortiz            */
/*=============================*/


/*
----------------------------------------------------------------------
  This is the main module for the neural net.  All of the main code   
  plus documentation on the system features lives here.

  A note on nomenclature: Most of the routine calls which you will 
  see in the code throughout these files will be "prefixed" by some 
  letters and an underscore.  That is one of my conventions for indicating
  which file contains the code for the subroutine in question. Because
  I have several files, this makes tracing problems and debugging
  files easier to accomplish.  

  This file is one of 14 source files which make up the back
  propagation code. These files are the following:

      activate.c              semi-linear activation function
      buildnet.c
      compile.c
      convert.c               conversion routines, conversion routines
      layer.c                 layer manipulation and creation
      lnrate.c
      net.c                   net manipulation, learning, propagation
      netio.c                 I/O routines, file handlers
      netmain.c               main routines, menus, user interface
      pairs.c
      prop.c
      shownet.c
      teach.c
      weights.c               weights manipulation and creation

  All of these are covered in detail in the respective files. The prefix
  codes for each of the files are as follows:

      activate.c              "A_"
      buildnet.c              "B_"
      compile.c               "CC_"
      convert.c               "C_"
      dribble.c               "D_"
      net.c                   "N_"
      netio.c                 "IO_"
      netmain.c               *NONE*
      pairs.c                 "PA_"
      parser.c                "PS_"
      prop.c                  "P_"
      layer.c                 "L_"
      lnrate.c                "LR_"
      shownet.c               "S_"
      teach.c                 "T_"
      weights.c               "W_"
      sysdep.c                "sys_"    (system dependent code)

  The rest of this file is organized into the folloing groups:

  (1) include files
  (2) externed functions
  (3) global variables
  (4) subroutines
----------------------------------------------------------------------
*/


/*
----------------------------------------------------------------------
  INCLUDE FILES
----------------------------------------------------------------------
*/
#include  "common.h"
#include  "netio.h"
#include  "weights.h"
#include  "layer.h"
#include  "net.h"
#include "gis.h"
#define YES 1
#define NO 0


/*
----------------------------------------------------------------------
  EXTERNED FUNCTIONS
----------------------------------------------------------------------
*/
extern Net    *B_create_net();
extern Net    *B_free_net();

extern void   N_query_net();
extern int    N_reset_wts();
extern void   N_save_wts();
extern Layer  *N_get_layer();

extern void   T_teach_net();

extern void   S_show_weights();
extern void   S_show_biases();
extern void   S_show_net();

extern float  IO_my_get_float();
extern int    IO_my_get_int();
extern int    IO_get_default_int();
extern int    IO_get_num_cycles();
extern void   IO_my_get_string();
extern void   IO_set_filenames();
extern void   IO_get_io_name();
extern void   IO_get_wts_name();
extern void   IO_print();

extern void   PA_initialize();
extern void   PA_setup_iopairs();
extern void   PA_reset_iopairs();
extern void   PA_randomize_file();

extern void   D_initialize();
extern void   D_dribble_status(); 

extern Sint   C_float_to_Sint();
extern void   L_modify_learning();
extern void   sys_init_rand();
extern void   CC_create_delivery();


/*
----------------------------------------------------------------------
  GLOBAL VARIABLES
----------------------------------------------------------------------
 Next come the global variables, declared in other routines, which    
  need to be referenced here.  In general I tried to keep the number  
  of globals to a minimum since they can be messy, but many of the    
  io functions needed to keep "state" variables for the lifetime of   
  the program execution.  Examples are the default file names which   
  are used when prompting the user.  These names are read here, and   
  referenced in  net.c as well as netio.c, and thus I needed to be    
  able to pass them around.  I could have simply left them as global  
  values and referenced them as needed, but instead I passed them     
  explicitly to the non-io routines that needed them.  I could just   
  as easily have declared them here and made them external to the     
  io package, but since file names and IO are so intimately tied, I   
  thought it more logical to declare these variables with the other   
  IO code.                                                            
----------------------------------------------------------------------
*/
extern char  net_config[];                 /* these three from netio.c  */
extern char  net_iop[];
extern char  net_fwt[];
extern char  net_pwt[];
extern char  IO_str[MAX_LINE_SIZE];

static Net  *the_net;          /* variable which holds ptr to the net   */
                               /* (global to this netmain routine only) */


/*
======================================================================
  ROUTINES IN NETMAIN.C
======================================================================
  The routines in this file are grouped below by function.  NO ROUTINE
  ARE PREFIXED IN THIS FILE. 

  The main routine is just below.  It basically amounts to a read-eval-
  print loop, much like an interpreter. After initialization, this code
  loops forever, calling "print_menu", "read_choice", and then 
  "evaluate".  The only other routine is a "check_net_ptr" routine 
  used during evaluate to verify that a valid net exists before 
  attempting some operation.
======================================================================
*/

/*int fout;*/
extern char outname[];
extern int propout;
char *G_ask_cell_new();
char *mapset, tmpstr[30];

netmain()
BEGIN
   void  initialize(), evaluate(), cleanup();
   char  c;
   int tt;
      

   if(propout == 0 ) {
	initialize();

	c = 'c';
	evaluate(c);

	c = 'i';
	evaluate(c);

l:	c = 't';
	evaluate(c);
/*	Curses_prompt_gets("Do you want to continue training ? ",tmpstr);
	if(atoi(tmpstr)) goto l; */
   }
   else {
	c = 'p';
	evaluate(c);
	cleanup();
   }

END /* main */

void initialize()
/*
-----------------------------------------------------------------
 This is the initialization routine for the main program. Much   
  of what the program is able to do is assumed here. For example 
  I have assumed (for now) that we will be dealing with only one 
  net at a time (even though I have made allowances in the NET   
  structure for multiple nets).  Thus, this guy only initializes 
  the one global 'the_net' variable.  This would need changing   
  in the future if more nets were added.                         
 Note also that the system random number generator is setup here 
  This is significant since IT MAY NOT BE PORTABLE.              
-----------------------------------------------------------------
*/
BEGIN
   the_net = NULL;
   sys_init_rand();
   PA_initialize();
   D_initialize();
END /* initialize */

void  evaluate(input)
char  input;
/*
----------------------------------------------------------------------
 Evaluates the input command and calls the appropriate function to    
  carry out the actions                                               
----------------------------------------------------------------------
*/
BEGIN
   int    t1, t2, check_net_ptr();
   float  tf1;
   char   filename[MAX_WORD_SIZE], file2[MAX_WORD_SIZE], 
          tmp_str[MAX_WORD_SIZE];
   FILE   *fp; /* for query from file */
   extern int NO_INPUT;

   switch (input) BEGIN
      case 'b': BEGIN
         if (check_net_ptr(the_net) == OKAY) BEGIN
            if (the_net->use_biases == FALSEE) BEGIN
               sprintf(IO_str, "\n*** network has no biases");
		Menu_msg(IO_str);
/*               IO_print(0);  */
            ENDIF
            else BEGIN
               sprintf(IO_str, "   layer number? ");
	       Curses_prompt_gets(IO_str,tmp_str);
		t1 = atoi(tmp_str);

/*               IO_print(0);
               t1 = IO_my_get_int(); */

               S_show_biases(the_net, t1);
            ENDELSE
         ENDIF
         break;
      ENDCASE
      case 'c': BEGIN
         the_net = B_free_net(the_net);
         IO_set_filenames();
         the_net = B_create_net(1, net_config);
         break;
      ENDCASE
      case 'd': BEGIN
         D_dribble_status();
         break;
      ENDCASE
      case 'g': BEGIN
         if (check_net_ptr(the_net) == OKAY) BEGIN
            /*--------------------------------*/
            /* use the net_config filename as */
            /* the configuration file.        */
            /*--------------------------------*/
             strcpy(filename, net_config);
            /*-----------------------------*/
            /* prompt for the weights file */
            /*-----------------------------*/
            sprintf(IO_str, "\n   Enter filename with PORTABLE weight values");
            IO_print(0);
            sprintf(IO_str, " (default=%s): ", net_pwt);
            IO_print(0);
            IO_my_get_string(file2);
            if (file2[0] == ENDSTRING) 
               strcpy(file2, net_pwt);

            /*-------------------------*/
            /* call the create routine */
            /*-------------------------*/
            CC_create_delivery(the_net, filename, file2);
         ENDIF
         break;
      ENDCASE
      case 'i': BEGIN
         if (check_net_ptr(the_net) == OKAY) BEGIN
/*            IO_get_io_name(); */
            PA_setup_iopairs(the_net, net_iop);
         ENDIF
         break;
      ENDCASE
      case 'j': BEGIN
         if (check_net_ptr(the_net) == OKAY) BEGIN
            sprintf(IO_str, "\n   Resetting I/O pairs to 'workfile.net'");
            IO_print(0);
            PA_reset_iopairs(the_net, "workfile.net");
         ENDIF
         break;
      ENDCASE
      case 'l': BEGIN
         if (check_net_ptr(the_net) == OKAY) BEGIN
            sprintf(IO_str, "\n   Layer number: ");
            IO_print(0);
            t1 = IO_my_get_int();
            L_modify_learning(t1, N_get_layer(the_net, t1));
         ENDIF
         break;
      ENDCASE
      case 'n': BEGIN
         if(check_net_ptr(the_net) == OKAY)
            S_show_net(the_net);
         break;
      ENDCASE
      case 'o': BEGIN
         if(check_net_ptr(the_net) == OKAY)
            if (the_net->num_io_pairs <= 0) BEGIN
               sprintf(IO_str, "\n\n*** no valid set of io pairs to randomize ***\n");
               IO_print(0);
            ENDIF
            else BEGIN
               sprintf(IO_str, "\n   Enter filename for reordered IO pairs: ");
               IO_print(0);
               IO_my_get_string(filename);
               while (filename[0] == ENDSTRING) BEGIN
                  sprintf(IO_str, "\n      try again: ");
                  IO_print(0);
                  IO_my_get_string(filename);
               ENDWHILE
               PA_randomize_file(filename, "workfile.net", the_net->num_io_pairs,
                                 the_net->num_inputs + the_net->num_outputs);
            ENDELSE

         break;
      ENDCASE
      case 'p': BEGIN
         if (check_net_ptr(the_net) == OKAY) BEGIN
            N_query_net(the_net, NO_INPUT);
         ENDIF
         break;
      ENDCASE
      case 'r': BEGIN
         if (check_net_ptr(the_net) == OKAY) BEGIN
            sprintf(IO_str,"\n   Load weights from FAST or PORTABLE format");
            IO_print(0);
            sprintf(IO_str, "(f/p, default=f)? ");
            IO_print(0);
            IO_my_get_string(tmp_str);
            if ((tmp_str[0] == 'p') || (tmp_str[0] == 'P'))
               t1 = PORTABLE_FORMAT;
            else t1 = FAST_FORMAT;
            sprintf(IO_str, "\n   Enter name of file with weight values");
            IO_print(0);
            IO_get_wts_name(t1);
            t1 = ( (t1 == FAST_FORMAT) 
                   ? N_reset_wts(the_net, net_fwt, t1)
                   : N_reset_wts(the_net, net_pwt, t1) );
            if (t1 == ERROR) BEGIN
               sprintf(IO_str, "\n*** weight resetting was incomplete ***");
               IO_print(0);
            ENDIF
         ENDIF
         break;
      ENDCASE
      case 's': BEGIN
         if (check_net_ptr(the_net) == OKAY) BEGIN
            sprintf(IO_str,"\n   Save weights in FAST or PORTABLE format");
            IO_print(0);
            sprintf(IO_str, "(f/p, default=f)? ");
            IO_print(0);
            IO_my_get_string(tmp_str);
            if ((tmp_str[0] == 'p') || (tmp_str[0] == 'P'))
               t1 = PORTABLE_FORMAT;
            else t1 = FAST_FORMAT;
            sprintf(IO_str, "\n   Enter file name for storing weights");
            IO_print(0);
            IO_get_wts_name(t1);
            if (t1 == FAST_FORMAT)
               N_save_wts(the_net, net_fwt, t1);
            else N_save_wts(the_net, net_pwt, t1);
         ENDIF
         break;
      ENDCASE
      case 't': BEGIN
         if(check_net_ptr(the_net) == OKAY) BEGIN
            if (the_net->num_io_pairs <= 0) BEGIN
               sprintf(IO_str, "*** no valid set of io pairs to teach ***");
		Menu_msg(IO_str);
/*               IO_print(0);  */
            ENDIF
            else BEGIN
               sprintf(IO_str, "  Enter constraint error: ");
               Curses_prompt_gets("Enter constraint error: ",tmpstr); 
	       tf1 = atof(tmpstr);
               sprintf(IO_str, "   Enter max number of cycles(default=%d): ", MAX_CYCLES);
		Curses_prompt_gets(IO_str,tmp_str);
		t1 = atoi(tmp_str);
		if(t1 == 0) t1 = MAX_CYCLES;

/*               IO_print(0);
               t1  = IO_get_num_cycles(); */

          sprintf(IO_str,"Enter cycle increment for showing errors(default=1) ");
		Curses_prompt_gets(IO_str,tmp_str);
		t2 = atoi(tmp_str);
		if(t2 == 0) t2 = 1;

/*               IO_print(0);
               t2  = IO_get_default_int(1); */

               T_teach_net(the_net, C_float_to_Sint(tf1), t1, t2);
            ENDELSE
         ENDIF
         break;
      ENDCASE
      case 'w': BEGIN
         if (check_net_ptr(the_net) == OKAY) BEGIN
            sprintf(IO_str, "   source layer? ");
	    Curses_prompt_gets(IO_str,tmp_str);
	    t1 = atoi(tmp_str);
/*            IO_print(0);
            t1 = IO_my_get_int(); */

            sprintf(IO_str, "   target layer? ");
	    Curses_prompt_gets(IO_str,tmp_str);
	    t2 = atoi(tmp_str);

/*            IO_print(0);
            t2 = IO_my_get_int(); */
            S_show_weights(the_net, t1, t2);
         ENDIF
         break;
      ENDCASE
      default :
         break;
   ENDSWITCH

END /* evaluate */


int  check_net_ptr(ptr_net)
Net  *ptr_net;
/*
----------------------------------------------------------------------
 This small routine is simply a check to see whether or not the arg   
  passed in is actually a valid Net pointer.  If a net has not been   
  created, or if there was an error in its creation, then no          
  operations ought to be allowed using the net pointer.               
 Returns 0 if the ptr is INvalid, otherwise returns a 1.  Note that   
  a net which has an error will return a non-NULL pointer, but the ID 
  of such a net will be negative. (see return values of B_create_net) 
----------------------------------------------------------------------
*/
BEGIN
   if (ptr_net == NULL) BEGIN
      sprintf(IO_str, "\n\n*** no valid net exists ***\n");
      IO_print(0);
      return(0);
   ENDIF
   if (ptr_net->ID == ERROR) BEGIN
      sprintf(IO_str, "\n\n*** no valid net exists ***\n");
      IO_print(0);
      return(0);
   ENDIF
   return(OKAY);

END /* check_net_ptr */


void  cleanup()
/*
----------------------------------------------------------------------
   Before quitting the program entirely, this routine ensures that all
   the loose ends are accounted for. Right now, that consists of freeing
   all the memory currently used for the network.
----------------------------------------------------------------------
*/
BEGIN

   the_net = B_free_net(the_net);

/*   G_close_cell(fout); */
   
END /* cleanup */
