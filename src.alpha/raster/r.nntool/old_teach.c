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
  Code For Teaching Networks (Prefix = T_)
----------------------------------------------------------------------
  This code is divided into 4 major sections:

  (1) include files
  (2) externed functions
  (3) externed global variables
  (4) subroutines
 
  Each section is further explained below.
----------------------------------------------------------------------
*/


/*
----------------------------------------------------------------------
  INCLUDE FILES
----------------------------------------------------------------------
*/
#include "common.h"
#include "weights.h"
#include "layer.h"
#include "net.h"


/*
----------------------------------------------------------------------
  EXTERNED FUNCTIONS
----------------------------------------------------------------------
  Below are the functions defined in other files which are used by the
  code here. They are organized by section.
----------------------------------------------------------------------
*/
extern  void         L_update_learn_rates();

extern  void         PA_rewind_workfile();
extern  void         PA_done_with_workfile();
extern  Sint         PA_retrieve();

extern  Sint         C_float_to_Sint();
extern  float        C_Sint_to_float();

extern  void         P_prop_input();
extern  void         P_prop_error();
extern  Sint         P_calc_delta();
extern  void         P_change_biases();

extern  void         D_dribble();
extern  float        sys_get_time();
extern  void         IO_print();
extern  void         IO_insert_format();



/*
----------------------------------------------------------------------
  EXTERNED GLOBALS
----------------------------------------------------------------------
  Finally, some globals imported from the NETMAIN.C code which drive
  the dribble option of NETS. This is a fairly new option (3-89) which
  allows you to save the errors, weights, or the results of a test 
  input while training a network.
  
  All that I need to know here is which of the flags for dribbling (if
  any) are set. There are three of these. If any are set, then I simply
  call the D_dribble routine to handle the rest.
----------------------------------------------------------------------
*/
extern int    SAVE_MAXERR;   /* all below are from the IO file */
extern int    SAVE_RMSERR;
extern int    SAVE_WEIGHTS;
extern int    SAVE_TESTS;
extern char   IO_str[MAX_LINE_SIZE];
extern char   IO_wkstr[MAX_LINE_SIZE];


/*
======================================================================
  ROUTINES IN TEACH.C                                                   
======================================================================
  The routines in this file are grouped below by function.  Each routine
  is prefixed by the string "T_" indicating that it is defined in the 
  "teach.c" file.  The types returned by the routines are also shown here
  so that cross checking is more easily done between these functions
  and the other files which intern them.


  Type Returned                 Routine                                 
  -------------                 -------                                 
                                                                     
  CREATION ROUTINES                                                    
    void                        T_teach_net                             
    Sint                        T_process_iopairs                       
    void                        T_setup_inputs                          
    Sint                        T_setup_errors     
    void                        T_change_weights 
======================================================================
*/


void  T_teach_net(ptr_net, desired_err, stop_cycle, print_mod)
Net   *ptr_net;
Sint  desired_err;
int   stop_cycle;
int   print_mod;
/*
----------------------------------------------------------------------
 'teach_net' is really the guts of this entire program, since most of 
  the time spent running this code will be spent here and since the   
  success or failure of the code rests here.  Because so much running 
  time will be spent in this routine, it is imperative that it be     
  efficient and much of the design of the data structures for the net 
  went into making this possible.                                     
 The basic idea is to follow the algorithm set down in the Rummelhart 
  et. al. paper on the generalized delta approach to neural nets.     
  Here, the idea is to take an input, propagate it through the net,   
  measure the subsequent output against your desired output, and then 
  propagate the error back through the net.  This process is repeated 
  for each IO pair in turn until such time as ALL the io pairs have   
  been learned within an acceptable error.                            
----------------------------------------------------------------------
 4-7-89  I added another parameter called "print_mod" which gets passed
  in from netmain.c to be used when printing out the Max error/RMS error
  message to the screen. The print_mod indicates how many training 
  cycles should elapse between printouts to the screen. Note that this
  printout statement used to be in the T_process_iopairs routine and 
  was moved here because this routine keeps the cycle number. Thus, all
  we need do is check the num_cycles against the print_mod to see if 
  the errors should be printed out.
----------------------------------------------------------------------
*/
BEGIN
   Sint   cur_error, T_process_iopairs();
   int    num_cycles, scroll_cycle=0, QUIT=0;
   float  t1, t2, grad_err;
   char cycv;

   cur_error  = desired_err * 2;
   num_cycles = 0;
   sprintf(IO_str, "\n*** Learning; please wait ***\n");
   IO_print(0);

   t1 = sys_get_time();                             /* record starting time */
   while (cur_error > desired_err && QUIT == 0) BEGIN
      num_cycles++; scroll_cycle++;

      if(scroll_cycle == 20) {
		printf("Enter c if you want to continue; q to stop --->:");
                scanf("%c",&cycv);
                getchar(); scroll_cycle = 0;

		if(cycv == 'q') {
			QUIT = 1;
			break;
		}
      }
      if (num_cycles > stop_cycle && cycv != 'q') BEGIN
         sprintf(IO_str, "\n*** Timeout failure ***\n");
         IO_print(0);
         sprintf(IO_str, "*** Net could not learn after %d tries ***\n", 
                stop_cycle);
         IO_print(0);
         break;
      ENDIF
      cur_error = T_process_iopairs(ptr_net, &grad_err);

      /*------------------------*/
      /* print errors to screen */
      /*------------------------*/
      if (print_mod != 0)
         if ((num_cycles % print_mod) == 0) BEGIN
            sprintf(IO_str, "\n Cycle : %d", num_cycles);
            IO_print(0);
            sprintf(IO_wkstr, "   Max error:%%.f    RMS error:%%.f \n");
            IO_insert_format(IO_wkstr);
            sprintf(IO_str, IO_wkstr, C_Sint_to_float(cur_error), grad_err);
            IO_print(0);
         ENDIF
      
      /*-----------------------------------------*/
      /* if dribble parameters set, then dribble */
      /*-----------------------------------------*/
      if (SAVE_MAXERR == TRUE || SAVE_RMSERR == TRUE 
          || SAVE_WEIGHTS == TRUE || SAVE_TESTS == TRUE)
         D_dribble(ptr_net, num_cycles, cur_error, grad_err);
   ENDWHILE
   
   t2 = sys_get_time();                             /* record ending time   */
   PA_done_with_workfile();                       /* make sure file closed*/
   if (num_cycles <= stop_cycle) BEGIN
      sprintf(IO_str, "\nNet learned after %d cycles\n", num_cycles);
      IO_print(0);
      sprintf(IO_str, "Learning time: %7.1f seconds\n", (t2 - t1));
      IO_print(0);
   ENDIF

END /* T_teach_net */


Sint  T_process_iopairs(ptr_net, grad_err)
Net    *ptr_net;
float  *grad_err;
/*
----------------------------------------------------------------------
 Steps through each of the io pairs once, propagating the input, then 
  propagating back the error.  At each point, a maximum error is      
  determined for the eventual return value from this routine.         
 There may be some confusion over the terminology concerning 'errors' 
  returned from this routine.  Actually, there are two types of error 
  values we are concerned with.  One is used as a measure of when to  
  to stop the processing, and the other is a normalized, overall error
  of how well the net currently knows the entire set of IO pairs. The 
  second number is more for diagnostic purposes than anything else,   
  but it is significant in that the whole theory behind the Rummelhart
  et. al. paper is that this algorithm is guaranteed to minimize the  
  second measure of error.                                            
 To keep things straight, I will call the two errors "stop_err" and   
  "grad_err" since the first measures when the process should stop and
  the second is keeping track of what ought to be a gradient descent. 
----------------------------------------------------------------------
 I just added (3-16-89) another variable called "avg_err" which is used
  as a pointer to a Sint which will be used to hold the average error
  for A SINGLE IO PAIR. The idea is to keep the average error for one
  input/output pair and then use this average to reset the learning rate
  for that particular IO pair. A guy named Don Woods (MacDonnel Doug.) 
  came up with the idea which he tested on an XOR net. I am simply 
  extending the idea to the general case (ie, multiple outputs) in an
  attempt to help the learning. A problem seems to arise with NETS 
  due to the fact that the deltas calculated can only get so small 
  before no weight changes are made. That is:
  
     delta weight = learn_rate * delta(j) * output(i) 
     
  for a weight connecting node i to output node j. If the learn_rate is
  small (say .1) and the output if i is average (say .5) then the delta
  must be .02 to generate a weight change of .001 (our minimum precision).
  Now:
  
     delta(j) = (t - o) * o(1-o)
     
  for the output deltas. With a desired delta of .02, we either have
  to have large (t-o) values, or large o values. That means NETS will
  stop learning (using a learn_rate of .1) when it starts to get close!
  The use of the avg err should help fix that.
 The avg_err is passed to T_change_weights so that each layer may 
  calculate its own learning rate based on the average error.
----------------------------------------------------------------------
 4-7-89 Note that I have moved the printout statement for the errors 
  to the T_teach_net routine so that a modulus argument could be used
  for specification of how often the errors should be printed.
----------------------------------------------------------------------
*/
BEGIN
   void    T_setup_inputs(), T_change_weights();
   Sint    stop_err, temp_s_err, avg_err, T_setup_errors();
   int     i;

   stop_err = 0;
   *grad_err = 0;
   PA_rewind_workfile();
   for (i = 0; i < ptr_net->num_io_pairs; i++) BEGIN
      T_setup_inputs(ptr_net);
      P_prop_input(ptr_net);
      temp_s_err = T_setup_errors(ptr_net, grad_err, &avg_err);
      if (temp_s_err > stop_err)
         stop_err = temp_s_err;
      
      P_prop_error(ptr_net);
      L_update_learn_rates(ptr_net->hidden_front, avg_err);
      T_change_weights(ptr_net);
      if (ptr_net->use_biases == TRUE)
         P_change_biases(ptr_net->hidden_front);
   ENDFOR
   
   *grad_err = sqrt( (*grad_err / ((float)ptr_net->output_layer->num_nodes
                                  * (float)ptr_net->num_io_pairs)) );
   return(stop_err);

END /* T_process_iopairs */


void  T_setup_inputs(ptr_net)
Net  *ptr_net;
/*
----------------------------------------------------------------------
 Reads in the correct number of inputs from the temporary file setup  
  by the 'PA_parse_iopairs' routine above, and places these values as  
  the outputs of the INPUT layer of the net (pointed to by 'ptr_net') 
  Note that it can be assumed that this intermediate file is in the   
  proper format since the 'PA_parse_iopairs' routine must be run       
  successfully prior to this routine.                                 
----------------------------------------------------------------------
*/
BEGIN
   int  i;

   for (i = 0; i < ptr_net->input_layer->num_nodes; i++)
      ptr_net->input_layer->node_outputs[i] = PA_retrieve();
   
END /* T_setup_inputs */


Sint  T_setup_errors(ptr_net, ptr_grad_err, ptr_avg_err)
Net    *ptr_net;
float  *ptr_grad_err;
Sint   *ptr_avg_err;
/*
----------------------------------------------------------------------
 This routine is very similar to the 'T_setup_inputs' routine above,  
  differing in that it works with the outputs rather than the inputs. 
  Here, the process involves looking at what the net generated, called
  the observed outputs, vs. what you wanted the net to generate,      
  called the target outputs.  Of course, the target outputs are just  
  the output part of the IO pair.  Once these two values are obtained 
  our error, called new_error, becomes the difference between the two 
  values.  The motivation behind calculating  this error (and then the
  delta for the node) is explained in the Rummelhart et. al. paper.   
 Besides calculating the errors and deltas for each output node, this 
  routine also has to send back a message indicating whether or not   
  the error was greater than the desired error.  Actually, this guy   
  just sends back whatever the maximum error was, leaving the job of  
  determining the maximum to the 'T_process_iopairs' routine in net.c 
  Note, however, that since we are doing a simple subtraction to find 
  our error we can get both positive and negative errors.  In order   
  to make the job of determining the maximum easier, this routine     
  always sends back the ABSOLUTE VALUE of the calculated error.       
 Referring back to the T_process_iopairs routine, note that there are 
  two different measures of error, the sort mentioned above and a     
  second type to track gradient descent.  Since this routine can only 
  return one type, the second sort of error is communicated via the   
  pointer to the floating point value passed to this routine.         
----------------------------------------------------------------------
*/
BEGIN
   Sint   result, target, observed, new_error;
   float  t_float;
   int    i;

   /*------------------------------------*/
   /* clear the result and average error */
   /*------------------------------------*/
   result = 0;
   *ptr_avg_err = 0;
   
   /*---------------------------*/
   /* loop for all output nodes */
   /*---------------------------*/
   for (i = 0; i < ptr_net->output_layer->num_nodes; i++) BEGIN
   
      /*----------------------------------*/
      /* get the desired output; subtract */
      /* what you got to set new_error    */
      /*----------------------------------*/
      target = PA_retrieve();
      observed = ptr_net->output_layer->node_outputs[i];
      new_error = target - observed;
      
      /*--------------------------------*/
      /* increment the avg error by the */
      /* new error (ie, running total)  */
      /*--------------------------------*/
#if  USE_SCALED_INTS
      *ptr_avg_err += abs(new_error);
#else
      *ptr_avg_err += fabs(new_error);
#endif
      
      /*-----------------------------------*/
      /* add the square of the error to    */
      /* the grad_err variable. These will */
      /* be summed, divided, then sqrt by  */
      /* the T_process_iopairs routine     */
      /*-----------------------------------*/
      t_float = C_Sint_to_float(new_error);
      *ptr_grad_err += (t_float * t_float);
      ptr_net->output_layer->node_deltas[i] =
                                P_calc_delta( (D_Sint)new_error, observed );
                                
      /*---------------------------------------------*/
      /* save new error as max if larger than result */
      /*---------------------------------------------*/
#if  USE_SCALED_INTS
      if (abs(new_error) > result)
         result = abs(new_error);
#else
      if (fabs(new_error) > result)
         result = fabs(new_error);
#endif
   ENDFOR
   
   /*-------------------------------------*/
   /* don't forget to divide avg error by */
   /* number of outputs which is equal to */
   /* the "i" value after the loop!!      */
   /*-------------------------------------*/
   *ptr_avg_err = *ptr_avg_err / ((Sint) i);
   return(result);

END /* T_setup_errors */


void  T_change_weights(ptr_net)
Net     *ptr_net;
/*
----------------------------------------------------------------------
 Propagates back through the net, changing the weights to reflect the 
  newest changes in the delta values.                                 
 Changing the weights in the net is similar to the two propagate      
  functions above in that all of the layers must be visited. However, 
  since our weights are connected to both their source and target     
  layers, some care has to be taken to ensure that all the weights are
  visited ONLY ONCE.  To do this, you can either propagate forward or 
  backward through the net, and I just happened to choose backward as 
  the way to go.  Thus, I start at the second to last layer (layer 1, 
  the output, is the last layer) and work my way back to layer 0.     
  Each layer is considered a SOURCE as it is visited, thus only the   
  out_weights to its TARGETS are updated for the layer.  This keeps a 
  set of weights from being updated twice.                            
 Once a set of weights is found, then the 'w_update' routine is     
  called to do the work of changing the weight values.
 I have added another parameter to this routine called "avg_err" which
  holds the average error value for ONE IO PAIR. This value is used by
  this routine to determine the learning rate for this particular IO
  pair. The idea (from Don Woods) is to use the cosecant (1/sin) to
  boost the learning rate for both high and low errors. The theory is
  that high errors need lots of change (because they're high) and low
  errors need lots of change (because they're close). I don't know how
  sound that really is; however, NETS is having problems learning when
  using low learn_rates (see T_setup_errors). Hopefully, this change 
  will enable NETS to overcome its precision problems. The change is
  
    new_learn = learn_rate * csc(avg_err * pi)
----------------------------------------------------------------------
*/
BEGIN
   Layer_lst    *cur_layer;
   Weights_lst  *cur_weight;
   Layer        *target, *source;
   Weights      *the_weights;
     
   cur_layer = ptr_net->hidden_back;
   while (cur_layer != NULL) BEGIN
      source     = cur_layer->value;
      cur_weight = cur_layer->value->out_weights;
      while (cur_weight != NULL) BEGIN
         target      = cur_weight->value->target_layer;
         the_weights = cur_weight->value;
         (*the_weights->w_update) (source, target, the_weights);
         cur_weight = cur_weight->next;
      ENDWHILE
      cur_layer = cur_layer->prev;
   ENDWHILE

END /* T_change_weights */
