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
  Code For Showing Network Structures (Prefix = S_)
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
extern  Layer        *N_get_layer();
extern  void         L_show_biases();
extern  void         L_show_learn_rates();
extern  Weights      *L_get_weights();
extern  void         W_show_weights();
extern  void         IO_print();
extern  void         IO_reset_more();
extern  int          IO_more();

extern  char         IO_str[MAX_LINE_SIZE];


/*
======================================================================
  ROUTINES IN SHOWNET.C                                                   
======================================================================
  The routines in this file are grouped below by function.  Each routine
  is prefixed by the string "S_" indicating that it is defined in the 
  "show.c" file.  The types returned by the routines are also shown 
  here so that cross checking is more easily done between these functions
  and the other files which intern them.


  Type Returned                 Routine                                 
  -------------                 -------                                 
    void                        S_show_net
    void                        S_show_wts
    void                        S_show_weights
    void                        S_show_biases
======================================================================
*/


void  S_show_net(ptr_net)
Net  *ptr_net;
/*
----------------------------------------------------------------------
 This routine prints out the configuration of the net passed in.  The 
  process involves printing out the net ID, then all the layers. The  
  layers are printed out in rows, starting with the input layer, then 
  the output layer, then any hidden layers. Each row has the layer id,
  then the size of the layer (in nodes), then a list of all of its    
  outputs, then a list of all of its inputs.                          
----------------------------------------------------------------------
*/
BEGIN
   void       S_show_wts();
   Layer_lst  *current;

   sprintf(IO_str, "\n\nnet ID:        %d", ptr_net->ID);
   IO_print(0);
   sprintf(IO_str, "\nbiases in use: %s\n",
           (ptr_net->use_biases == TRUEE) ? "TRUEE" : "FALSEE");
   IO_print(0);

   /*----------------------------------------*/
   /* show the learning rates for the layers */
   /*----------------------------------------*/
   current = ptr_net->hidden_front;
   while (current != NULL) BEGIN
      L_show_learn_rates(current->value);
      current = current->next;
   ENDWHILE
   
   sprintf(IO_str, "\nlayer %d size: %d; ", ptr_net->input_layer->ID,
          ptr_net->input_layer->num_nodes);
   IO_print(0);
   S_show_wts(ptr_net->input_layer);
   sprintf(IO_str, "layer %d size: %d; ", ptr_net->output_layer->ID, 
          ptr_net->output_layer->num_nodes);
   IO_print(0);
   S_show_wts(ptr_net->output_layer);

   current = ptr_net->hidden_front;
   while (current->value->ID != OUT_LAYER) BEGIN
      sprintf(IO_str, "layer %d size: %d; ", current->value->ID,
             current->value->num_nodes);
      IO_print(0);
      S_show_wts(current->value);
      current = current->next;
   ENDWHILE

END /* S_show_net */


void   S_show_wts(ptr_layer)
Layer  *ptr_layer;
/*
----------------------------------------------------------------------
 NOTE THAT THIS ROUTINE DOES NOT PRINT OUT WEIGHT VALUES. All that is 
  done here is to print out a list of the layer ID to which the input 
  layer acts as a source.  Then the same thing is done showing from   
  which the current layer acts as a target.                           
----------------------------------------------------------------------
*/
BEGIN
   Weights_lst  *current;

   sprintf(IO_str, "as source to: ");
   IO_print(0);
   current = ptr_layer->out_weights;
   while (current != NULL) BEGIN
      sprintf(IO_str, "%d ", current->value->target_layer->ID);
      IO_print(0);
      current = current->next;
   ENDWHILE

   sprintf(IO_str, " as target from: ");
   IO_print(0);
   current = ptr_layer->in_weights;
   while (current != NULL) BEGIN
      sprintf(IO_str, "%d ", current->value->source_layer->ID);
      IO_print(0);
      current = current->next;
   ENDWHILE
   sprintf(IO_str, "\n");
   IO_print(0);

END /* S_show_wts */


void  S_show_weights(ptr_net, source, target)
Net  *ptr_net;
int  source, target;
/*
----------------------------------------------------------------------
 Given a particular nerual net, a source layer, and a target layer,   
  this routine will print out the weights between those layers, if a  
  set of weights exitst.  If no weights exist between the two layers  
  this guy will print out an error message.                           
----------------------------------------------------------------------
*/
BEGIN
   Layer    *ptr_layer;
   Weights  *ptr_weights;
   
  IO_reset_more();
  
   ptr_layer = N_get_layer(ptr_net, source);
   if (ptr_layer->ID == ERROR) BEGIN
      sprintf(IO_str, "\n*** no source layer found ***\n");
      IO_print(0);
      return;
   ENDIF

   ptr_weights = L_get_weights(ptr_layer, target);
   if (ptr_weights->type == ERROR) BEGIN
      sprintf(IO_str, "\n*** no such source,target pair ***\n");
      IO_print(0);
      return;
   ENDIF

   sprintf(IO_str, "\n\nWeights FROM layer %d TO layer %d\n",
          ptr_weights->source_layer->ID, ptr_weights->target_layer->ID);
   IO_more(0);
   W_show_weights(ptr_weights);

END /* S_show_weights */


void  S_show_biases(ptr_net, layer_num)
Net  *ptr_net;
int  layer_num;
/*
----------------------------------------------------------------------
 Given a particular nerual net, a source layer, and a target layer,   
  this routine will print out the weights between those layers, if a  
  set of weights exitst.  If no weights exist between the two layers  
  this guy will print out an error message.                           
----------------------------------------------------------------------
*/
BEGIN
   Layer    *ptr_layer;   

   IO_reset_more();

   if (layer_num == 0) BEGIN
      sprintf(IO_str, "\n*** input layer has no bias values ***");
      IO_print(0);
      return;
   ENDIF
   
   ptr_layer = N_get_layer(ptr_net, layer_num);
   if (ptr_layer->ID == ERROR) BEGIN
      sprintf(IO_str, "\n*** layer %d not found ***\n", layer_num);
      IO_print(0);
      return;
   ENDIF

   sprintf(IO_str, "\n\nBias values for layer %d\n", ptr_layer->ID);
   IO_more(0);
   L_show_biases(ptr_layer);

END /* S_show_biases */

