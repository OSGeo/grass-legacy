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
  Code For Production of Delivery Code (Prefix = CC_)
----------------------------------------------------------------------
  This code is divided into 3 major sections:

  (1) include files
  (2) externed functions
  (3) subroutines
 
  Each section is further explained below.
----------------------------------------------------------------------
*/


/*
----------------------------------------------------------------------
  INCLUDE FILES
----------------------------------------------------------------------
*/
#include  "common.h"
#include  "weights.h"
#include  "layer.h"
#include  "net.h"
#include  "netio.h"


/*
----------------------------------------------------------------------
  EXTERNED FUNCTIONS AND GLOBALS
----------------------------------------------------------------------
  Below are the functions defined in other files which are used by the
  code here. They are organized by section.
----------------------------------------------------------------------
*/
extern int    N_reset_wts();
extern int    N_save_wts();
extern void   IO_my_get_string();
extern void   IO_print();
extern void   sys_delete_file();
extern int    PA_check_signature();

extern char   IO_str[MAX_LINE_SIZE];


/*
======================================================================
  ROUTINES IN DELIVER.C                                                   
======================================================================
  The routines in this file are grouped below by function. Each routine
  is prefixed by the string "CC_" indicating that it is defined in the 
  "deliver.c" file.  The types returned by the routines are also shown 
  so that cross checking is more easily done between these functions
  and the other files which intern them.


  Type Returned                 Routine                                 
  -------------                 -------                                 
                                                                     
  CREATION ROUTINES                                                    
    void                        CC_create_delivery
    void                        CC_setup_header
    void                        CC_create_initialize
    void                        CC_create_prop
    void                        CC_create_cleanup
    void                        CC_create_main
======================================================================
*/


void  CC_create_delivery(ptr_net, net_file, wts_file)
Net   *ptr_net;
char  *net_file, *wts_file;
/*
----------------------------------------------------------------------
  Using the network as a guide, this routine creates a deliverable
  file which is used simply to execute the network.
----------------------------------------------------------------------
*/
BEGIN
   FILE  *fp;
   void  CC_setup_header(), CC_create_initialize(), CC_create_prop(),
         CC_create_cleanup(), CC_create_main(), CC_set_name();
   char  cc_file[MAX_WORD_SIZE], net_name[MAX_WORD_SIZE];

   /*-----------------------------------------------------*/
   /* if the weights file is not in portable format, quit */
   /*-----------------------------------------------------*/
   if (PA_check_signature(wts_file, PORTABLE_FORMAT) == ERROR)
      return;
   
   /*-------------------------------------*/
   /* first, prompt for delivery file and */
   /* check that the file can be opened   */
   /*-------------------------------------*/
   sprintf(IO_str, "\n   Enter name of delivery file: ");
   IO_print(0);
   IO_my_get_string(cc_file);
   while (cc_file[0] == ENDSTRING) BEGIN
      sprintf(IO_str, "\n   try again: ");
      IO_print(0);
      IO_my_get_string(cc_file);
   ENDWHILE   
   if( (fp = fopen(cc_file, "wt")) == NULL) BEGIN
      sprintf(IO_str, "\n*** can't open %s file ***\n", cc_file);
      IO_print(0);
      return;
   ENDIF

   /*------------------------------------*/
   /* once the prompting is done, we can */
   /* begin. First,though, save off the  */
   /* current network weights since they */
   /* will be trashed in the process of  */
   /* making the delivery file.          */
   /*------------------------------------*/
   N_save_wts(ptr_net, "temp.wts", FAST_FORMAT);

   /*---------------------------------------------*/
   /* Next, check the weights to see that they    */
   /* are compatible with the net's configuration */
   /* and if not, print out message and abort.    */
   /*---------------------------------------------*/
   if (N_reset_wts(ptr_net, wts_file, PORTABLE_FORMAT) == ERROR) BEGIN
      sprintf(IO_str, "\n*** weights not compatible with network");
      IO_print(0);
   ENDIF
   
   else BEGIN   
      /*-------------------------------------------*/
      /* setup the net_name variable using the net */
      /* ID so that this code will be unique.      */
      /*-------------------------------------------*/
      CC_set_name(net_name, net_file);
   
      /*------------------------------------------------*/
      /* Create the headers and routines which are used */
      /* to initialize, propagate, and free a network.  */
      /*------------------------------------------------*/
      CC_setup_header(fp, ptr_net, net_name);
      CC_create_main(fp, net_name);
      CC_create_initialize(fp, ptr_net, net_file, wts_file, net_name);
      CC_create_prop(fp, net_name);
      CC_create_cleanup(fp, net_name);
   ENDELSE
   
   /*-------------------------------------------*/
   /* reset the weights of the network to their */
   /* former values, throw away the temporary   */
   /* file, and close the delivery file.        */
   /*-------------------------------------------*/
   N_reset_wts(ptr_net, "temp.wts", FAST_FORMAT);
   sys_delete_file("temp.wts");
   fclose(fp);

END /* CC_create_delivery */


void  CC_set_name(net_name, net_file)
char  *net_name, *net_file;
/*
----------------------------------------------------------------------
   Using the name of the network specification file (stored in the
   "net_file" parameter) this routine sets up the string which will be
   used as a precursor to all variable and routine names in the delivery
   file. The idea is to read until a "." or the end of the string is
   encountered.
----------------------------------------------------------------------
*/
BEGIN
   int  i = 0;
   
   while ((net_file[i] != '.') && (net_file[i] != ENDSTRING)) BEGIN
      net_name[i] = net_file[i];      
      /*-------------------------------------------------*/
      /* I know this is stupid, but the stupid masscomp  */
      /* C compiler choked on an autoincrement of "i"    */
      /* within the array reference. So I had to come    */
      /* down a peg to the brute force approach.         */
      /*-------------------------------------------------*/
      i++;
   ENDWHILE
   net_name[i++] = '_';
   net_name[i] = ENDSTRING;

END /* CC_set_name */


void  CC_setup_header(fp, ptr_net, name)
FILE  *fp;
Net   *ptr_net;
char  *name;
/*
----------------------------------------------------------------------
  This routine prints out the initial includes and defines for the 
  delivery file. Also, two arrays of floating point numbers are 
  declared as global variables; these will be the arrays which are
  used to put inputs into the network.
----------------------------------------------------------------------
*/
BEGIN   
   fprintf(fp, "\n/*=============================*/");
   fprintf(fp, "\n/* NETS Network Delivery File  */");
   fprintf(fp, "\n/*                             */");
   fprintf(fp, "\n/* a product of the AI Section */");
   fprintf(fp, "\n/* NASA, Johnson Space Center  */");
   fprintf(fp, "\n/*                             */");
   fprintf(fp, "\n/* principal author:           */");
   fprintf(fp, "\n/*       Paul Baffes           */");
   fprintf(fp, "\n/*                             */");
   fprintf(fp, "\n/* contributing authors:       */");
   fprintf(fp, "\n/*      Brian Dulock           */");
   fprintf(fp, "\n/*      Chris Ortiz            */");
   fprintf(fp, "\n/*=============================*/");
   
   fprintf(fp, "\n\n#include \"common.h\"");
   fprintf(fp, "\n#include \"weights.h\"");
   fprintf(fp, "\n#include \"layer.h\"");
   fprintf(fp, "\n#include \"net.h\"");
   fprintf(fp, "\n#include \"netio.h\"");
   fprintf(fp, "\n#define  INPUT_SIZE   %d", 
               ptr_net->input_layer->num_nodes);
   fprintf(fp, "\n#define  OUTPUT_SIZE  %d",
               ptr_net->output_layer->num_nodes);
   
   fprintf(fp, "\n\nextern Net   *B_create_net();");
   fprintf(fp, "\nextern Net   *B_free_net();");
   fprintf(fp, "\nextern int    N_reset_wts();");
   fprintf(fp, "\nextern void   P_prop_input();");
   fprintf(fp, "\nextern void   PA_initialize();");
   fprintf(fp, "\nextern void   D_initialize();");
   fprintf(fp, "\nextern Sint   C_float_to_Sint();");
   fprintf(fp, "\nextern float  C_Sint_to_float();");
   fprintf(fp, "\nextern void   sys_init_rand();");
   
   fprintf(fp, "\n\n\n/*------------------*/");
   fprintf(fp, "\n/* Global Variables */");
   fprintf(fp, "\n/*------------------*/");
   fprintf(fp, "\nfloat  %sInputs[INPUT_SIZE];", name);
   fprintf(fp, "\nfloat  %sOutputs[OUTPUT_SIZE];", name);
   fprintf(fp, "\nNet   *%sNetPtr;", name);
     
END /* CC_setup_header */


void  CC_create_main(fp, name)
FILE  *fp;
char  *name;
/*
----------------------------------------------------------------------
   This code creates an example main program for the user. All that is
   done here is to fill up the Inputs array once, propagate it through
   the network, and print out the results.
----------------------------------------------------------------------
*/
BEGIN

   fprintf(fp, "\n\n\n/*-------------------------------------------------------------*/");
   fprintf(fp, "\n/* Here is an example of a main routine. Note that the network */");
   fprintf(fp, "\n/* is initialized once BEFORE before the propagate routine is  */");
   fprintf(fp, "\n/* called and cleaned up once AFTER ALL calls to propagate are */");
   fprintf(fp, "\n/* completed. That is, you only need to call initialize once   */");
   fprintf(fp, "\n/* to build the network and once to throw it away.  Note also  */");
   fprintf(fp, "\n/* that the inputs and outputs are communicated via the two    */");
   fprintf(fp, "\n/* global arrays defined at the top of the file.               */");
   fprintf(fp, "\n/*                                                             */");
   fprintf(fp, "\n/* This routine should be replaced with your own routines(s)   */");
   fprintf(fp, "\n/* designed for your application.                              */");
   fprintf(fp, "\n/*-------------------------------------------------------------*/");
   fprintf(fp, "\nmain()");
   fprintf(fp, "\n{");
   fprintf(fp, "\n   int   i;");
   fprintf(fp, "\n   void  %sinitialize();", name);
   fprintf(fp, "\n   void  %spropagate();", name);
   fprintf(fp, "\n   void  %scleanup();", name);
   fprintf(fp, "\n\n   %sinitialize();", name);
   fprintf(fp, "\n\n   for (i = 0; i < INPUT_SIZE; i++)");
   fprintf(fp, "\n      %sInputs[i] = .9;", name);
   fprintf(fp, "\n   %spropagate();", name);
   fprintf(fp, "\n   for (i = 0; i < OUTPUT_SIZE; i++)");
#if  USE_SCALED_INTS
   fprintf(fp, "\n      printf(\"\\n output %%d = %%7.3f\", i, %sOutputs[i]);", name);
#else
   fprintf(fp, "\n      printf(\"\\n output %%d = %%10.6f\", i, %sOutputs[i]);", name);
#endif
   fprintf(fp, "\n\n   %scleanup();", name);
   fprintf(fp, "\n\n} /* example main program */");

END /* CC_create_main */


void  CC_create_initialize(fp, ptr_net, net_file, wts_file, name)
FILE  *fp;
Net   *ptr_net;
char  *net_file, *wts_file, *name;
/*
----------------------------------------------------------------------
  This routine creates an initialization file for the network. Mostly 
  this involves setting up the net and its weights, although it also
  involves some of the same initialization calls made in the "initialize"
  routine of netmain.c. Note I duplicate those calls here so that the
  user does not have to include that file in his compile (which we 
  DONT want included since it has a "main" function).
----------------------------------------------------------------------
*/
BEGIN   
   fprintf(fp, "\n\n\n/*---------------------------------------------*/");
   fprintf(fp, "\n/* call this routine once to setup the network */");
   fprintf(fp, "\n/*---------------------------------------------*/");
   fprintf(fp, "\nvoid  %sinitialize()", name);
   fprintf(fp, "\n{");
   fprintf(fp, "\n   int  i;");
   
   /*-----------------------------------------------*/
   /* write code for calling initializationroutines */
   /*-----------------------------------------------*/
   fprintf(fp, "\n\n   /*--------------------------*/");
   fprintf(fp, "\n   /* call initialization code */");
   fprintf(fp, "\n   /*--------------------------*/");
   fprintf(fp, "\n   %sNetPtr = NULL;", name);
   fprintf(fp, "\n   sys_init_rand();");
   fprintf(fp, "\n   PA_initialize();");
   fprintf(fp, "\n   D_initialize();");
   
   /*--------------------------------------------*/
   /* code for creating network and setting bias */
   /*--------------------------------------------*/
   fprintf(fp, "\n\n   /*----------------*/");
   fprintf(fp, "\n   /* create network */");
   fprintf(fp, "\n   /*----------------*/");
   fprintf(fp, "\n   %sNetPtr = B_create_net(1, \"%s\");", name, net_file);
   fprintf(fp, "\n   %sNetPtr->use_biases = %s;", name,
               ((ptr_net->use_biases == TRUEE) ? "TRUEE" : "FALSEE"));
   fprintf(fp, "\n   %sNetPtr->num_inputs  = INPUT_SIZE;", name);
   fprintf(fp, "\n   %sNetPtr->num_outputs = OUTPUT_SIZE;", name);
      
   fprintf(fp, "\n\n   /*--------------------------------------------*/");
   fprintf(fp, "\n   /* reset weights and the input, output arrays */");
   fprintf(fp, "\n   /*--------------------------------------------*/");
   fprintf(fp, "\n   N_reset_wts(%sNetPtr, \"%s\", PORTABLE_FORMAT);", name, wts_file);
   fprintf(fp, "\n   for (i = 0; i < INPUT_SIZE; i++)");
   fprintf(fp, "\n      %sInputs[i] = 0.0;", name);
   fprintf(fp, "\n   for (i = 0; i < OUTPUT_SIZE; i++)");
   fprintf(fp, "\n      %sOutputs[i] = 0.0;", name);
   fprintf(fp, "\n\n} /* %sinitialize */", name);
   
END /* CC_create_initialize */


void  CC_create_prop(fp, name)
FILE  *fp;
char  *name;
/*
----------------------------------------------------------------------
  This guy creates the propagate routine for pushing info through a 
  net. It assumes that the calling routine has loaded the Inputs array
  from which it will read its inputs. After propagating it puts the 
  outputs on the Outputs arrray. Again, the caller may do with these
  arrays as it likes, since they are global. However, if these conventions
  are not followed then there is no guaranty on the behavior of the 
  network.
----------------------------------------------------------------------
*/
BEGIN
   fprintf(fp, "\n\n\n/*------------------------------------------------*/");
   fprintf(fp, "\n/* call this routine every time you want to query */");
   fprintf(fp, "\n/* the network. Note that it assumes the \"Input\"  */");
   fprintf(fp, "\n/* array is already loaded with input values      */");
   fprintf(fp, "\n/*------------------------------------------------*/");
   fprintf(fp, "\nvoid  %spropagate()", name);
   fprintf(fp, "\n{");
   fprintf(fp, "\n   int  i;");
   fprintf(fp, "\n   Layer  *input, *output;");
   
   fprintf(fp, "\n\n   /*------------------------------------------*/");
   fprintf(fp, "\n   /* get pointers to network input and output */");
   fprintf(fp, "\n   /*------------------------------------------*/");
   fprintf(fp, "\n   input  = %sNetPtr->input_layer;", name);
   fprintf(fp, "\n   output = %sNetPtr->output_layer;", name);
   fprintf(fp, "\n\n   /*--------------------------------------*/");
   fprintf(fp, "\n   /* load input values; propagate network */");
   fprintf(fp, "\n   /*--------------------------------------*/");
   fprintf(fp, "\n   for (i = 0; i < INPUT_SIZE; i++)");
   fprintf(fp, "\n      input->node_outputs[i] = C_float_to_Sint");
   fprintf(fp, "(%sInputs[i]);", name);
   fprintf(fp, "\n   P_prop_input(%sNetPtr);", name);
   
   fprintf(fp, "\n\n   /*---------------------*/");
   fprintf(fp, "\n   /* setup output values */");
   fprintf(fp, "\n   /*---------------------*/");
   fprintf(fp, "\n   for (i = 0; i < OUTPUT_SIZE; i++)");
   fprintf(fp, "\n      %sOutputs[i] = C_Sint_to_float", name);
   fprintf(fp, "(output->node_outputs[i]);");
   fprintf(fp, "\n\n} /* %spropagate */", name);

END /* CC_create_prop */


void  CC_create_cleanup(fp, name)
FILE  *fp;
char  *name;
/*
----------------------------------------------------------------------
  This routine is provided as a means for removing the memory assigned
  to the network during its execution. 
----------------------------------------------------------------------
*/
BEGIN
   fprintf(fp, "\n\n\n/*------------------------------------------*/");
   fprintf(fp, "\n/* call this routine once to free the space */");
   fprintf(fp, "\n/* used by the network.                     */");
   fprintf(fp, "\n/*------------------------------------------*/");
   fprintf(fp, "\nvoid  %scleanup()", name);
   fprintf(fp, "\n{");
   fprintf(fp, "\n   B_free_net(%sNetPtr);", name);
   fprintf(fp, "\n\n} /* %scleanup */", name);

END /* CC_create_prop */
