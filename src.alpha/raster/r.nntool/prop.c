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
  Code For Forward and Backward Propagation in NETS (Prefix = P_)
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
extern  Sint         A_activation();
extern  Sint         C_float_to_Sint();


/*
======================================================================
  ROUTINES IN PROP.C                                                   
======================================================================
  The routines in this file are grouped below by function.  Each routine
  is prefixed by the string "P_" indicating that it is defined in the 
  "prop.c" file.  The types returned by the routines are also shown here
  so that cross checking is more easily done between these functions
  and the other files which intern them.


  Type Returned                 Routine
  -------------                 -------
    void                        P_prop_input
    D_Sint                      P_full_dot_forward
    D_Sint                      P_full_dot_backward
    void                        P_full_update_wts
    D_Sint                      P_s_pattern_dot_forward
    D_Sint                      P_s_pattern_dot_backward
    void                        P_s_pattern_update_wts
    void                        P_prop_error
    Sint                        P_calc_delta
    D_Sint                      P_t_pattern_dot_forward
    D_Sint                      P_t_pattern_dot_backward
    void                        P_t_pattern_update_wts
    void                        P_change_biases
======================================================================
*/


void  P_prop_input(ptr_net)
Net  *ptr_net;
/*
----------------------------------------------------------------------
 This routine is definitely NOT going to appear obvious to you the    
  first time you read it.  To understand how it works, or even why I  
  wrote the code the way I have, requires an intimate understanding of
  the Rummelhart paper, the structures I have set up, and the inner   
  workings of integer and pointer arithmetic.  I will assume that the 
  reader has either already read or has access to the Rummelhart paper
  and the '.h' files containing the structures.  As the algorithm is  
  explained, I will make references to the various elements from these
  two sources.                                                        
 Forward propagation, as explained in the paper, is the process of    
  calculating the outputs for the nodes of all layers.  This process  
  is somewhat recursive, in that some layers depend upon the outputs  
  of other layers being calculated first.  Thus the order of layer    
  calculation is important, and this order is presumed to be pre-set  
  by the user within the net configuration file (see doc with the     
  'B_setup_net' routine above and with 'PS_get_layer' in netio.c). The  
  first while loop below takes care of looping through the layers in  
  the correct order (which must be FORWARD since this is forward      
  propagation; see doc for net struc in 'net.h' and for Layer_lst in  
  'layer.h').                                                         
 Now, for each of the layers, we want to figure out the output of each
  of its nodes (target_nodes).  First, we get access to that layers   
  array of node outputs (via cur_layer->value->node_outputs) and then 
  we record (in target_size) how many nodes the layer has (via        
  cur_layer->value->num_nodes).  The first 'for' loop takes care of   
  looping through each of the nodes, in turn, to calculate its output.
 As described by Rummelhart, the output of a node is defined as the   
  dot product between its input layer and the weights connecting the  
  members of that layer to the node in question.  Since we have       
  allowed for the general case of multiple incoming layers, we need   
  another loop to make sure all inputs are processed.  Referring to   
  the layer structure (see layer.h), note that the incoming layers    
  are accessible only through the weights connecting the two layers.  
  (see Layer.in_weights).  Thus we access the incoming weights via    
  cur_layer->value->num_nodes, through which we will be able to get at
  each incoming layer, in turn.  The second while loop serves to loop 
  through each of the incoming weights (or layers).                   
 Now, a weight has a pointer to both its source and target layers, but
  we are only concerned with the source here since it is the incoming 
  layer.  Thus we access the source layer's node outputs (via         
  cur_weight->value->source_layer->node_outputs) and also its size    
  (via cur_weight->value->source_layer->num_nodes).  To multiply these
  outputs by the corresponding weights, we access the weights between 
  the layers (via cur_weight->value->values).                         
 Then, we call 'dot_product' to do the actual multiplications.  The   
  reason to separate this routine out is because it can be optimized  
  even further in assembly code, and we want to leave that option     
  available.                                                          
 Note that we have the statement 'cur_weight += (j * source_size)'    
  immediately preceding the call to the f_prop code.  This bizzare incr 
  statement right in the middle of things is not obvious; its roots   
  are directly linked to our weigts design choice.  Remember that we  
  decided to store the weights in a column major fashion, or more     
  simply, by source rather than by target.  Thus, if we had two       
  layers, s = source and t = target, then our weights array would be  
  ordered as follows:                                                 
                                                                      
   s1,t1 s2,t1 sN,t1   s1,t2 s2,t2 sN,t2   s1,tN s2,tN sN,tN          
                                                                      
  Keep in mind that the trick here is to be able to associate the     
  correct weights with any given source,target pair.  Assuming that   
  are steping down the nodes of a source layer IN ORDER we have no    
  great problems, since the weights are ordered to match the source   
  layer nodes.  For example, if we are currently using s1 and t2, our 
  weight between them is s1,t2.  Steping to the weight between s2 and 
  t2 (to s2,t2) is a simple increment.                                
 However, the problem is not so simple when you are steping through   
  the target layer node by node.  Here each increment in number is    
  paralleled by a SOURCE SIZE JUMP IN THE WEIGHTS ARRAY.  Stated      
  another way, the distance between sX,tY and sX,tY+1 is exactly equal
  to the size of the source layer.  Thus, before we call the routine  
  pointed to by f_prop we must make sure the weights array pointer is   
  set to the proper place for the target represented by "j".  This is 
  done by multiplying 'j' by SOURCE_SIZE and adding the result to the 
  weights pointer.                                                     
----------------------------------------------------------------------
*/
BEGIN
   register Sint  *target_nodes, *target_bias;
   Layer_lst    *cur_layer;
   Weights_lst  *cur_weight;
   D_Sint       sum;
   int          target_size, j;
  
   cur_layer = ptr_net->hidden_front;
   while (cur_layer != NULL) BEGIN
      target_nodes = cur_layer->value->node_outputs;
      target_bias  = cur_layer->value->node_bias;
      target_size  = cur_layer->value->num_nodes;

      for (j = 0; j < target_size; j++) BEGIN
         sum = 0;
         cur_weight = cur_layer->value->in_weights;
         while (cur_weight != NULL) BEGIN
            sum = (*cur_weight->value->f_prop) (sum, cur_weight->value, j);
            cur_weight = cur_weight->next;
         ENDWHILE
         *target_nodes++ = A_activation(sum + (*target_bias++));
      ENDFOR
      cur_layer = cur_layer->next;
   ENDWHILE

END /* P_prop_input */


D_Sint  P_full_dot_forward(cur_total, the_weights, t_node_index)
D_Sint   cur_total;
Weights  *the_weights;
int      t_node_index;
/*
----------------------------------------------------------------------
connect-all
 Finally we are ready to form the dot product between a set of outputs
  and the corresponding weights.  Since all of the weights between    
  two layers are stored together, we must choose only those weights   
  which are connected to the node in the target layer (ie, node j)    
  which we are currently handling.  Additionally, we must use exactly 
  as many weights as we have nodes in the source layer (source_size). 
  Happily, we knew this would be the case, thus we ordered the weights
  between two layers BY TARGET LAYER NUMBER.  Thus, all of node j's   
  weights come before those of node j+1, etc. (see doc in weights.c   
  for 'S_show_weights').  This routine takes care of doing just 'num' 
  number of multiplications.                                          
 Notice, however, that while I define variable i to determine         
  the correct number of times to loop, I NEVER USE THE VARIABLE in    
  an array access.  This is because an array access is an inheirantly 
  slow calculation.  Instead, we rely on the fact that the arrays we  
  are dealing with have all of their information stored sequentially, 
  so we can just increment the pointer to the array to move from      
  element to element.  The i  variable is used only to indicate       
  how many times the loop should be done.    
 Note that the indexing of the weights and nodes arrays depends upon
  the connection scheme. In the CONNECT_ALL case, we have very little 
  complication as everything is interconnected. In the PATTERNED case,
  I make use of a trick in which I write TWO loops, one the size of the
  smaller layer, and then increment the pointer from the smaller layer
  each time the INNER loop finishes. This way I can avoid one set of
  array accesses (ie, to the smaller layer). The larger layer I must still
  access using array notation via the "decoder" array (see 'weights.h').
 Finally, remember that we are dotting  SOURCE layer with the weights,
  thus every time we increment the source by one, we must increment   
  the weights by one to keep the weights and source numbers the same. 
  (see documentation under 'P_prop_inputs'). Also, since each Sint is
  actually an integer representation with an implied decimal point, we
  have the result that after our dot product is formed we have incidentally
  multiplied it by a constant factor of SINT_SCALE. That is, each time we
  do a multiplication of two Sints, we end up with an extra factor of
  SINT_SCALE. Thus we must divide this out before returning the value.
  Of course, this does not hold when floats are used rather than Sints.
  Lastly, note that we disallow any value which exceeds the maximum or
  minimum Sint value. This keeps the overall node value from exceeding
  its maximum (note that no intermediate product will exceed the maximum
  since the node values are between 0 and 1 and since only 10000 nodes
  are allowed per layer).
----------------------------------------------------------------------
 This routine has been further divided into three replicas. By separating
 the three different ways of propagating forward, I can speed the 
 learning process by avoiding "if" checking to see whether or not I
 should use patterened connection schemes or a connect-all scheme. The
 result is that I have to store a pointer to this function INSIDE THE
 WEIGHTS STRUCTURES THAT USE IT (change made 9-5-89). This first routine
 is for fully connected schemes only.
----------------------------------------------------------------------
*/
BEGIN
   register Sint   *ptr_Snodes, *ptr_weights;
   int             i, s_size;

   s_size      = the_weights->source_layer->num_nodes;
   ptr_Snodes  = the_weights->source_layer->node_outputs;
   ptr_weights = the_weights->values + (t_node_index * s_size);

   for (i = 0; i < s_size; i++)
      cur_total += ((D_Sint)(*ptr_Snodes++) * (D_Sint)(*ptr_weights++));

#if  USE_SCALED_INTS
   return(cur_total / (D_Sint)SINT_SCALE);
#else
   return(cur_total);
#endif
   
END /* P_full_dot_forward */


D_Sint  P_s_pattern_dot_forward(cur_total, the_weights, t_node_index)
D_Sint   cur_total;
Weights  *the_weights;
int      t_node_index;
/*
----------------------------------------------------------------------
  (see documentation under P_full_dot_forward) This routine is for 
  pattern connection schemes which have a large source layer connected
  to a smaller target layer.
----------------------------------------------------------------------
*/
BEGIN
   register Sint   *ptr_Snodes, *ptr_weights;
   register int16  *ptr_decoder;
   int             i, area;

   area        = the_weights->map_area;
   ptr_Snodes  = the_weights->source_layer->node_outputs;
   ptr_weights = the_weights->values + (area * t_node_index);
   ptr_decoder = the_weights->decoder + (area * t_node_index);

   for (i = 0; i < area; i++)
	  cur_total += ((D_Sint)(ptr_Snodes[*ptr_decoder++])
	                * (D_Sint)(*ptr_weights++));

#if  USE_SCALED_INTS
   return(cur_total / (D_Sint)SINT_SCALE);
#else
   return(cur_total);
#endif
   
END /* P_s_pattern_dot_forward */


D_Sint  P_t_pattern_dot_forward(cur_total, the_weights, t_node_index)
D_Sint   cur_total;
Weights  *the_weights;
int      t_node_index;
/*
----------------------------------------------------------------------
  (see documentation under P_full_dot_forward) This routine is for 
  pattern connection schemes which have a large target layer connected
  to a smaller source layer.
----------------------------------------------------------------------
*/
BEGIN
   register Sint   *ptr_Snodes, *ptr_weights;
   register int16  *ptr_decoder;
   int             i, j, s_size, area;

   s_size      = the_weights->source_layer->num_nodes;
   ptr_Snodes  = the_weights->source_layer->node_outputs;
   ptr_weights = the_weights->values;
   area        = the_weights->map_area;
   ptr_decoder = the_weights->decoder;

   for (i = 0; i < s_size; i++) BEGIN
      for (j = 0; j < area; j++) BEGIN
         if (*ptr_decoder == t_node_index)
            cur_total += ((D_Sint)(*ptr_Snodes)
                          * (D_Sint)(*ptr_weights));
         ptr_decoder++;
         ptr_weights++;
      ENDFOR
      ptr_Snodes++;
   ENDFOR

#if  USE_SCALED_INTS
   return(cur_total / (D_Sint)SINT_SCALE);
#else
   return(cur_total);
#endif
   
END /* P_t_pattern_dot_forward */


void  P_prop_error(ptr_net)
Net  *ptr_net;
/*
----------------------------------------------------------------------
 Similar to P_prop_input, only you propagate backwards.  Instead of     
  propagating outputs, however, you calculate DELTA values which are  
  a measure of how much error should be propagated back through a     
  particular node (see Rummelhart paper).  Thus, you start with the   
  'hidden_back' pointer, and move back through the list using 'prev'  
  pointers (instead of 'next'), calculating deltas as you go.  One    
  other difference here is that you DO NOT have to waste any time     
  propagating deltas back to layer 0, since by definition it does not 
  have any incoming weights.  That is, since the deltas are only      
  needed for changing weight values, and since weights are changed    
  from target to source layers (see Rummelhart paper), there is no    
  need for delta values in a layer which  never acts as a target.     
  Layer 0 (the input) is such a layer.                                
----------------------------------------------------------------------
*/
BEGIN
   register Sint  *source_deltas, *source_nodes;
   Layer_lst    *cur_layer;
   Weights_lst  *cur_weight;
   Sint         output, P_calc_delta();
   D_Sint       sum;
   int          source_size, j;
  
   cur_layer = ptr_net->hidden_back;
   while (cur_layer != NULL) BEGIN
      if (cur_layer->value->ID == 0) break;      /* dont do for layer 0 */
      source_nodes  = cur_layer->value->node_outputs;
      source_deltas = cur_layer->value->node_deltas;
      source_size   = cur_layer->value->num_nodes;
      for (j = 0; j < source_size; j++) BEGIN
         sum = 0;
         cur_weight = cur_layer->value->out_weights;
         while (cur_weight != NULL) BEGIN
            sum = (*cur_weight->value->b_prop) (sum, cur_weight->value, j);
            cur_weight = cur_weight->next;
         ENDWHILE
         output = *source_nodes++;
         *source_deltas++ = P_calc_delta(sum, output); 
      ENDFOR
      cur_layer = cur_layer->prev;
   ENDWHILE

END /* P_prop_error */


D_Sint  P_full_dot_backward(cur_total, the_weights, s_node_index)
D_Sint   cur_total;
Weights  *the_weights;
int      s_node_index;
/*
----------------------------------------------------------------------
 This routine is almost identical to the '...dot_forward' routines above. 
  Note that we pass in a second argument, "s_size", to this routine   
  because of how we must increment the weights pointer to keep it in  
  sync with the target pointer.  That is, every time we increment the 
  the target pointer by one, we must increment the weights pointer by 
  SOURCE size (see documentation under 'P_prop_input'). 
 NOTE that we are propagating back from the TARGET layer to the SOURCE
  layer.
----------------------------------------------------------------------
 I split this routine into three parts as I did for P_full_dot_forward
  above (see above documentation). This first version is for fully 
  connected schemes.
----------------------------------------------------------------------
*/
BEGIN
   register Sint   *ptr_Tdeltas, *ptr_weights;
   register int    i, s_size, t_size;

   s_size      = the_weights->source_layer->num_nodes;
   t_size      = the_weights->target_layer->num_nodes;
   ptr_Tdeltas = the_weights->target_layer->node_deltas;
   ptr_weights = the_weights->values + s_node_index;
   
   for (i = 0; i < t_size; i++) BEGIN
      cur_total += ((D_Sint)(*ptr_Tdeltas++) * (D_Sint)(*ptr_weights));
      ptr_weights += s_size;
   ENDFOR

#if  USE_SCALED_INTS
   return(cur_total / (D_Sint)SINT_SCALE);
#else
   return(cur_total);
#endif

END /* P_full_dot_backward */


D_Sint  P_s_pattern_dot_backward(cur_total, the_weights, s_node_index)
D_Sint   cur_total;
Weights  *the_weights;
int      s_node_index;
/*
----------------------------------------------------------------------
 (see documentation under P_full_dot_backward and P_full_dot_forward).
  This is the back propagation for weights which connect large source
  layers to smaller target layers.
----------------------------------------------------------------------
*/
BEGIN
   register Sint   *ptr_Tdeltas, *ptr_weights;
   register int16  *ptr_decoder;
   int             i, j, t_size, area;

   t_size      = the_weights->target_layer->num_nodes;
   ptr_Tdeltas = the_weights->target_layer->node_deltas;
   ptr_weights = the_weights->values;
   area        = the_weights->map_area;
   ptr_decoder = the_weights->decoder;
      
   for (i = 0; i < t_size; i++) BEGIN
      for (j = 0; j < area; j++) BEGIN
         if (*ptr_decoder == s_node_index)
            cur_total += ((D_Sint)(*ptr_Tdeltas) 
                          * (D_Sint)(*ptr_weights));
         ptr_decoder++;
         ptr_weights++;
      ENDFOR
      ptr_Tdeltas++;
   ENDFOR

#if  USE_SCALED_INTS
   return(cur_total / (D_Sint)SINT_SCALE);
#else
   return(cur_total);
#endif

END /* P_s_pattern_dot_backward */


D_Sint  P_t_pattern_dot_backward(cur_total, the_weights, s_node_index)
D_Sint   cur_total;
Weights  *the_weights;
int      s_node_index;
/*
----------------------------------------------------------------------
 (see documentation under P_full_dot_backward and P_full_dot_forward).
  This is the back propagation for weights which connect large target
  layers to smaller source layers.
----------------------------------------------------------------------
*/
BEGIN
   register Sint   *ptr_Tdeltas, *ptr_weights;
   register int16  *ptr_decoder;
   int             i, area;

   area        = the_weights->map_area;
   ptr_Tdeltas = the_weights->target_layer->node_deltas;
   ptr_weights = the_weights->values + (area * s_node_index);
   ptr_decoder = the_weights->decoder + (area * s_node_index);
   
   for (i = 0; i < area; i++)
      cur_total += ((D_Sint)(ptr_Tdeltas[*ptr_decoder++])
                    * (D_Sint)(*ptr_weights++));

#if  USE_SCALED_INTS
   return(cur_total / (D_Sint)SINT_SCALE);
#else
   return(cur_total);
#endif

END /* P_t_pattern_dot_backward */


Sint  P_calc_delta(error_sum, output)
D_Sint  error_sum;
Sint    output;
/*
----------------------------------------------------------------------
 This little routine computes the DELTA value for the node, given two 
  inputs:  an error sum (or just an error difference for the output   
  nodes), and an output value for the node.  The formula for computing
  the delta value is:  [ error_sum * output * (1 - output) ].  This   
  would be no hard computation, except for the fact that we have a    
  special SCALED representation for our incoming arguments, namely the
  'Sint' type.  Thus, we cannot just use the '1' in the above formula 
  since that number is in decimal, not in Sint form.  Also, because   
  our scaled integers are all multiplied by a common factor, every    
  we multiply two Sints together, we get a result which has that      
  that common factor TO A POWER OF TWO!  For example, the Sint = 1 is 
  1024 (also called SINT_SCALE).  If we multiply it by itself, we     
  will get 1,048,576; however, the corresponding decimal product would
  be 1 * 1 = 1, or 1024 in Sint!!! Thus, we must always divide each   
  multiplication of a Sint by 1024 so that our result is correct.     
 Note that this routine makes use of the D_Sint type, which is just   
  a Sint which is twice as big in the machine.  The reason for this is
  that multiplication of two Sints could lead to a result which is    
  larger than a Sint type can hold; by using the D_Sint we ensure that
  no intermediate results will be out of range.                       
 Note also that the error_sum argument comes in as a D_Sint, not a    
  Sint.  In the routine above, a type conversion has to be made for   
  the types to match.  Doesn't that seem odd? Actually, the answer for
  why this is done does not become apparent until you understand the  
  P_prop_error routine below.  It turns out that the general case of    
  calculating a delta for a given node will involve an error_sum of   
  all the error effects from target layers.  This sum involves a      
  product of two Sints and as such must be stored in a D_Sint if we   
  want to avoid overflows during the multiplication.  Thus, this      
  routine expects that the error_sum will come in as a D_Sint, thus   
  the code above must make a type conversion to remain compatible.    
----------------------------------------------------------------------
*/
BEGIN
#if  USE_SCALED_INTS
   D_Sint  temp;

   temp = (error_sum * (D_Sint)output) / SINT_SCALE;
   temp = (temp * (D_Sint)(SINT_SCALE - output)) / SINT_SCALE;
   return((Sint) temp);
#else
   return( (Sint)(error_sum * output * (1.0 - output)) );
#endif

END /* P_calc_delta */


void  P_full_update_wts(source, target, the_weights)
Layer    *source, *target;
Weights  *the_weights;
/*
----------------------------------------------------------------------
 This code follows the same rules for steping through a set of weights
  that both the 'dot_forward' and 'dot_backward' routines had to      
  follow.  Because the weights are stored by source size (column      
  major) each increment of source number increments the weights by one
  and each increment of target number increments the weights by the   
  source size (see P_prop_inputs doc.).  Thus, in order to step through 
  the weights one at a time and keep the source and target pointers   
  in sync, I put the target incrementation as the OUTTER loop and the 
  source incrementation as the INNER loop, for the CONNECT_ALL case. I
  can use the same trick in the PATTERNED connection case, and it will
  always refer to the layer with the fewer number of nodes. In this case,
  I have two loops, one for the size of the smaller layer and an inner one
  for the size of the map_area. Each time the second loop is completed we
  move on to the next element of the smaller layer, and the appropriate
  pointer is incremented.
 Once the correct weight, target delta, and source output are located 
  the code does the weight change calculation shown in Rummelhart.    
  Note once again that because of our Sint format, any products of    
  two Sint values must be divided by SINT_SCALE (see 'P_full_dot_forward'    
  documentation).                                                     
 A change was made here on 3-16-89 to calculate the learning rate 
  dynamically for each IO pair (see T_change_weights and T_setup_errors).
----------------------------------------------------------------------
 Like P_full_dot_forward and P_full_dot_backward, this routine was split
  into three different functions, one for each type of connection scheme.
  This first version handles weight updates for fully connected weight
  schemes only.
----------------------------------------------------------------------
*/
BEGIN
   register Sint  *source_nodes, *save_source, *target_deltas;
   D_Sint  temp, temp2, learn, momentum;
   int     s_size, i, j;
   Sint    *wt_values, *wt_deltas, new_delta;
#if  USE_SCALED_INTS
   D_Sint  scale;

   scale         = (D_Sint)SINT_SCALE;
#endif
   source_nodes  = source->node_outputs;
   save_source   = source_nodes;
   s_size        = source->num_nodes;
   target_deltas = target->node_deltas;
   wt_values     = the_weights->values;
   wt_deltas     = the_weights->prev_deltaW;
   learn         = target->cur_learn;
   momentum      = (D_Sint)C_float_to_Sint(target->momentum);
   
   for (i = 0; i < target->num_nodes; i++) BEGIN
      source_nodes = save_source;
#if  USE_SCALED_INTS
      temp = (learn * (D_Sint)(*target_deltas++)) / scale;
      for (j = 0; j < s_size; j++) BEGIN
         temp2 = (temp * (D_Sint)(*source_nodes++)) / scale;
         temp2 = temp2 + ((momentum * (D_Sint)(*wt_deltas)) / scale);
#else
      temp = (learn * (D_Sint)(*target_deltas++));
      for (j = 0; j < s_size; j++) BEGIN
         temp2 = temp * (D_Sint)(*source_nodes++);
         temp2 = temp2 + (momentum * (D_Sint)(*wt_deltas));
#endif
         new_delta = (Sint)temp2;
         *(wt_deltas++) = new_delta;
         *(wt_values++) += new_delta;
      ENDFOR
   ENDFOR

END /* P_full_update_wts */


void  P_s_pattern_update_wts(source, target, the_weights)
Layer    *source, *target;
Weights  *the_weights;
/*
----------------------------------------------------------------------
 (see documentation for P_full_update_wts and P_full_dot_backward). 
  This routine updates the weights for patterned connection schemes 
  where the source layer is larger than the target layer.
----------------------------------------------------------------------
*/
BEGIN
   register Sint  *source_nodes, *target_deltas;
   D_Sint  temp, temp2, learn, momentum;
   int     i, j, area;
   Sint    *wt_values, *wt_deltas, new_delta;
   int16   *ptr_decoder;
#if  USE_SCALED_INTS
   D_Sint  scale;

   scale         = (D_Sint)SINT_SCALE;
#endif
   source_nodes  = source->node_outputs;
   target_deltas = target->node_deltas;
   wt_values     = the_weights->values;
   wt_deltas     = the_weights->prev_deltaW;
   learn         = target->cur_learn;
   momentum      = (D_Sint)C_float_to_Sint(target->momentum);
   area          = the_weights->map_area;
   ptr_decoder   = the_weights->decoder;

   for (i = 0; i < target->num_nodes; i++) BEGIN
#if USE_SCALED_INTS
      temp = (learn * (D_Sint)(*target_deltas++)) / scale;
      for (j = 0; j < area; j++) BEGIN
         temp2 = (temp * (D_Sint)(source_nodes[*ptr_decoder++])) / scale;
         temp2 = temp2 + ((momentum * (D_Sint)(*wt_deltas)) / scale);
#else
      temp = (learn * (D_Sint)(*target_deltas++));
      for (j = 0; j < area; j++) BEGIN
         temp2 = temp * (D_Sint)(source_nodes[*ptr_decoder++]);
         temp2 = temp2 + (momentum * (D_Sint)(*wt_deltas));
#endif
         new_delta = (Sint)temp2;
         *(wt_deltas++) = new_delta;
         *(wt_values++) += new_delta;
      ENDFOR
   ENDFOR

END /* P_s_pattern_update_wts */


void  P_t_pattern_update_wts(source, target, the_weights)
Layer    *source, *target;
Weights  *the_weights;
/*
----------------------------------------------------------------------
 (see documentation for P_full_update_wts and P_full_dot_backward). 
  This routine updates the weights for patterned connection schemes 
  where the target layer is larger than the source layer.
----------------------------------------------------------------------
*/
BEGIN
   register Sint  *source_nodes, *target_deltas;
   D_Sint  temp, temp2, learn, momentum;
   int     i, j, area;
   Sint    *wt_values, *wt_deltas, new_delta;
   int16   *ptr_decoder;
#if  USE_SCALED_INTS
   D_Sint  scale;

   scale         = (D_Sint)SINT_SCALE;
#endif
   source_nodes  = source->node_outputs;
   target_deltas = target->node_deltas;
   wt_values     = the_weights->values;
   wt_deltas     = the_weights->prev_deltaW;
   learn         = target->cur_learn;
   momentum      = (D_Sint)C_float_to_Sint(target->momentum);
   area          = the_weights->map_area;
   ptr_decoder   = the_weights->decoder;

   for (i = 0; i < source->num_nodes; i++) BEGIN
#if USE_SCALED_INTS
      temp = (learn * (D_Sint)(*source_nodes++)) / scale;
      for (j = 0; j < area; j++) BEGIN
         temp2 = (temp * (D_Sint)(target_deltas[*ptr_decoder++])) / scale;
         temp2 = temp2 + ((momentum * (D_Sint)(*wt_deltas)) / scale);
#else
      temp = (learn * (D_Sint)(*source_nodes++));
      for (j = 0; j < area; j++) BEGIN
         temp2 = temp * (D_Sint)(target_deltas[*ptr_decoder++]);
         temp2 = temp2 + (momentum * (D_Sint)(*wt_deltas));
#endif
         new_delta = (Sint)temp2;
         *(wt_deltas++) = new_delta;
         *(wt_values++) += new_delta;
      ENDFOR
   ENDFOR

END /* P_t_pattern_update_wts */


void  P_change_biases(ptr_layers)
Layer_lst  *ptr_layers;
/*
----------------------------------------------------------------------
 The goal of this routine is the updating of the node bias values, 
  similar to the way in which weights are changed. Two new values are
  determined for each node of the network (1) a new bias value (2) the
  amount by which to change the bias value. In fact, as with the weights
  the delta bias (deltaB) is determined based on 
  
     dBias = learn_rate * delta_of_output_node * output_of_input_node
  
  In the bias case, the last element of the product is always = 1.0, so
  it can be dropped out of the calculation. The routine is called after
  all delta values have been computed, thus we only need move through
  the layers in consecutive order making the changes as we go. 
  Note: 
----------------------------------------------------------------------
*/
BEGIN
   register Sint  *cur_bias, *cur_dB, *cur_delta;
   Layer      *cur_layer;
   int        i;
   D_Sint     temp, learn, momentum;
   Sint       new_delta;
#if  USE_SCALED_INTS
   D_Sint     scale;

   /*------------------*/
   /* get scale factor */
   /*------------------*/
   scale = (D_Sint)SINT_SCALE;
#endif

   while (ptr_layers != NULL) BEGIN
      /*--------------------------*/
      /* access the current layer */
      /*--------------------------*/
      cur_layer = ptr_layers->value;

      /*------------------------------------*/
      /* get the learning rate and momentum */
      /*------------------------------------*/
      learn     = cur_layer->cur_learn;
      momentum  = (D_Sint)C_float_to_Sint(cur_layer->momentum);
      
      /*--------------------------*/
      /* get pointers to bias and */
      /* previous deltaB arrays   */
      /*--------------------------*/
      cur_bias  = cur_layer->node_bias;
      cur_delta = cur_layer->node_deltas;
      cur_dB    = cur_layer->prev_deltaB;
      
      /*--------------------------------*/
      /* loop to update both the biases */
      /* and the prev_deltaB values     */
      /*--------------------------------*/
      for (i = 0; i < cur_layer->num_nodes; i++) BEGIN
#if  USE_SCALED_INTS
         temp = (learn * (D_Sint)(*cur_delta++)) / scale;
         new_delta = 
           (Sint)(temp + ((momentum * (D_Sint)(*cur_dB)) / scale));
#else
         temp = learn * (D_Sint)(*cur_delta++);
         new_delta = 
           (Sint)(temp + (momentum * (D_Sint)(*cur_dB)));
#endif
         *cur_dB++     = new_delta;
         *cur_bias++  += new_delta;
      ENDFOR   
      
      /*---------------------*/
      /* go on to next layer */
      /*---------------------*/
      ptr_layers = ptr_layers->next;
   ENDWHILE
   
END /* P_change_biases */
