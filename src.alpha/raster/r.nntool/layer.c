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
  Code For Manipulating Layer And Layer_lst Structures (Prefix = L_)
----------------------------------------------------------------------
  This code is divided into 2 major sections:

  (1) include files
  (2) subroutines
 
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
#include  "netio.h"


/*
----------------------------------------------------------------------
  EXTERNED ROUTINES AND GLOBALS
----------------------------------------------------------------------
*/
extern void    W_free_weights_lst();
extern float   C_Sint_to_float();
extern D_Sint  LR_calc_learn_rate();
extern float   LR_scale_default();
extern char    *sys_alloc();
extern void    sys_free();
extern void    IO_print();
extern int     IO_more();
extern float   IO_get_default_float();
extern void    IO_insert_format();

extern char    IO_wkstr[MAX_LINE_SIZE];
extern char    IO_str[MAX_LINE_SIZE];


/*
======================================================================
  ROUTINES IN LAYER.C                                                   
======================================================================
  The routines in this file are grouped below by function.  Each routine
  is prefixed by the string "L_" indicating that it is defined in the 
  "layer.c" file.  The types returned by the routines are also shown here
  so that cross checking is more easily done between these functions
  and the other files which intern them.


  Type Returned                 Routine                                 
  -------------                 -------                                 
                                                                      
  LAYER ROUTINES                                             
    Layer *                     L_alloc_layer
    Weights *                   L_get_weights
    void                        L_free_layer
    void                        L_update_learn_rates
    void                        L_modify_learning
    void                        L_show_biases
    void                        L_show_learn_rates
                                                                      
  LAYER_LST ROUTINES                                                    
    Layer_lst *                 L_alloc_layer_lst
    Layer_lst *                 L_insert_before
    Layer_lst *                 L_insert_after
    void                        L_free_layer_lst
======================================================================
*/


Layer  *L_alloc_layer(ptr_spec)
Layer_spec  *ptr_spec;
/*
----------------------------------------------------------------------
 Given a number of nodes for the layer, this routine creates a Layer  
  structure and mallocs enough space for the 'outputs' and 'deltas'   
  arrays.  Note, however, that the pointers to the weights are NOT    
  filled in at this time.  That is done later, after all other layers 
  and weights have been created.                                      
 This routine also takes the time to reset all of the output and      
  delta values to 0.  Of course, this could be changed to a random    
  number.                                                             
----------------------------------------------------------------------
*/
BEGIN
   int           i, nodes;
   unsigned int  size;
   Layer         *result;

   /*-----------------------------------------*/
   /* set up a temporary variable holding the */
   /* number of nodes for multiple accesses   */
   /*-----------------------------------------*/
   nodes = ptr_spec->num_nodes;
   
   result = (Layer *) sys_alloc((unsigned)sizeof(Layer));
   result->ID           = ptr_spec->ID;
   result->num_nodes    = nodes;
   result->max_incoming = 0;
   result->X_dim        = ptr_spec->X_dim;
   result->Y_dim        = ptr_spec->Y_dim;
   result->cur_learn    = 0;
   result->learn_base   = ptr_spec->learn_base;
   result->learn_scale  = ptr_spec->learn_scale;
   result->momentum     = ptr_spec->momentum;
   size = ((unsigned) nodes) * ((unsigned) sizeof(Sint));
   result->node_outputs = (Sint *) sys_alloc(size);
   result->node_deltas  = (Sint *) sys_alloc(size);
   result->node_bias    = (Sint *) sys_alloc(size);
   result->prev_deltaB  = (Sint *) sys_alloc(size);
   for (i = 0; i < nodes; i++) BEGIN
      result->node_outputs[i] = 0;
      result->node_deltas[i]  = 0;
      result->node_bias[i]    = 0;
      result->prev_deltaB[i]  = 0;
   ENDFOR
   result->in_weights   = NULL;
   result->out_weights  = NULL;

   return(result);

END /* L_alloc_layer */


void  L_free_layer(ptr_layer)
Layer  *ptr_layer;
/*
----------------------------------------------------------------------
  This routine frees the space associated with a layer structure and 
   any of the space associated with its INCOMING weights. Only the 
   incoming weights are addressed as each weight structure has two 
   references so only one need be used (both is redundant). Note that
   the Layer_lst structures are freed by the CALLER of this routine. 
   That is, an assumption is made that whomever is cleaning up takes
   the responsibility of removing the layer list.
----------------------------------------------------------------------
*/
BEGIN
   Weights_lst  *cur, *last;
   
   if (ptr_layer == NULL) return;
   
   /*-----------------------------------*/
   /* free the actual node value arrays */
   /*-----------------------------------*/
   sys_free((char *) ptr_layer->node_outputs);
   sys_free((char *) ptr_layer->node_deltas);
   sys_free((char *) ptr_layer->node_bias);
   sys_free((char *) ptr_layer->prev_deltaB);

   /*---------------------------------*/
   /* free the incoming weight arrays */
   /*---------------------------------*/
   W_free_weights_lst(ptr_layer->in_weights);
   
   /*------------------------------------*/
   /* loop through the outgoing weights  */
   /* freeing JUST WEIGHT_LST structures */
   /*------------------------------------*/
   cur = ptr_layer->out_weights;
   while (cur != NULL) BEGIN
      last = cur;
      cur = cur->next;
      sys_free((char *) last);
   ENDWHILE
   
   sys_free((char *) ptr_layer);

END /* L_free_layer */


Layer_lst  *L_alloc_layer_lst(ptr_layer)
Layer  *ptr_layer;
/*
----------------------------------------------------------------------
 Given a pointer to a Layer structure, this routine allocates enough  
  space for a Layer_lst structure and initialized the 'value' field   
  to point to 'ptr_layer'.  As above, by returning a pointer to the   
  malloc'ed space, the allocation remains after this routine returns. 
----------------------------------------------------------------------
*/
BEGIN
   Layer_lst  *result;

   result = (Layer_lst *) sys_alloc((unsigned) sizeof(Layer_lst));
   result->value = ptr_layer;
   result->prev  = NULL;
   result->next  = NULL;

   return(result);

END /* L_alloc_layer_lst */


Layer_lst  *L_insert_before(ptr_node, ptr_lst)
Layer_lst  *ptr_node, *ptr_lst;
/*
----------------------------------------------------------------------
 The inputs to this routine are both pointers to Layer_lst structures 
  The first should be a pointer to a NEW element to be added to the   
  list of Layer_lst structures, which are pointed to by the second    
  argument.  Additionally, the second argument is assumed to point to 
  an element in the list BEFORE WHICH the new node is to be put.      
 Note that the Layer_lst structured form a DOUBLY linked list, so     
  both previous ('prev') and next ('next') pointers must be set up    
  here.                                                               
 Finally, note that there can be some confusion as to what value to   
  return from this routine.  Think of it this way: say you had a list 
  (x y z) and you wanted to add element Q to the list.  The result in 
  this routine would be (Q x y z); thus you would want the pointer    
  returned from this routine to be the POINTER TO THE INSERTED        
  ELEMENT.  If, however, one of the arguments were NULL, then you     
  would always want to return the other argument.  This takes care of 
  the case when you insert an element into an empty list.             
 As a result, when using this routine you should ALWAYS reassign the  
  value of you list to the output returned here.  Otherwise, the NULL 
  cases will not be handled properly.                                 
----------------------------------------------------------------------
*/
BEGIN
   if (ptr_node == NULL)
      return(ptr_lst);
   else if (ptr_lst == NULL)
      return(ptr_node);
   else BEGIN
      ptr_node->prev = ptr_lst->prev;      /* set up new node pointers */
      ptr_node->next = ptr_lst;

      if (ptr_node->prev != NULL)          /* reset prev node next ptr */
         ptr_node->prev->next = ptr_node;  /* if there is a prev node  */

      ptr_node->next->prev = ptr_node;     /* reset next node prev ptr */
      return(ptr_node);                    /* return ptr to new node   */
   ENDELSE

END /* L_insert_before */


Layer_lst  *L_insert_after(ptr_node, ptr_lst)
Layer_lst  *ptr_node, *ptr_lst;
/*
----------------------------------------------------------------------
 The inputs to this routine are both pointers to Layer_lst structures 
  The first should be a pointer to a new element to be added to the   
  list of Layer_lst structures, which are pointed to by the second    
  argument.  Additionally, the second argument is assumed to point to 
  an element in the list AFTER WHICH the new node is to be put.       
 Note that the Layer_lst structured form a DOUBLY linked list, so     
  both previous ('prev') and next ('next') pointers must be set up    
  here.                                                               
 Like the above routine, this guy should always be used to reset the  
  the value of the original list pointer, because of the differences  
  between NULL cases and the normal case.  However, unlike the routine
  above, this routine returns a pointer to the OLD list, not the newly
  inserted node.  That is, if you want Q to be inserted after your    
  pointer to (x y z), then you want (x Q y z) as your result, not     
  (Q y z)!                                                            
----------------------------------------------------------------------
*/
BEGIN
   if (ptr_node == NULL)
      return(ptr_lst);
   else if (ptr_lst == NULL)
      return(ptr_node);
   else BEGIN
      ptr_node->prev = ptr_lst;            /* set up new node pointers */
      ptr_node->next = ptr_lst->next;

      ptr_node->prev->next = ptr_node;     /* reset prev node next ptr */

      if (ptr_node->next != NULL)          /* reset next node prev ptr */
         ptr_node->next->prev = ptr_node;  /* if there is a next node  */
      return(ptr_lst);                     /* return ptr to list       */
   ENDELSE

END /* L_insert_after */


void  L_free_layer_lst(ptr_lst)
Layer_lst  *ptr_lst;
/*
----------------------------------------------------------------------
  Loops through a layer list linked list freeing layers as it goes.
   Each layers must also have its corresponding layer_lst structure 
   freed. Note that the layers free weights, but only the INCOMING 
   weights since each weight structure is doubly referenced.
  NOTE THAT THE INCOMING POINTER IS DESTROYED by the operations in this
   routine.
----------------------------------------------------------------------
*/
BEGIN
   Layer_lst  *last_layer;
   void        L_free_layer();
  
   if (ptr_lst == NULL) return;
   
   /*-------------------------------------*/
   /* loop through the layer list freeing */
   /* layers (and thus weights). Also be  */
   /* sure to free layer_lst pointers.    */
   /*-------------------------------------*/
   while (ptr_lst != NULL) BEGIN
   
      /*---------------------------------------*/
      /* free current layer and its IN weights */
      /*---------------------------------------*/
      L_free_layer(ptr_lst->value);
      
      /*-------------------------------*/
      /* then free layer_lst structure */
      /*-------------------------------*/
      last_layer = ptr_lst;
      ptr_lst = ptr_lst->next;
      sys_free((char *) last_layer);
   ENDWHILE
   
END /* L_free_layer_lst */


Weights  *L_get_weights(ptr_layer, target)
Layer  *ptr_layer;
int    target;
/*
----------------------------------------------------------------------
 This routine attempts to return a pointer to a Weights structure     
  given the assumption that the input 'ptr_layer' is a pointer to the 
  layer which is the "source" for the Weights structure, and the      
  'target' input parameter is a number representing the ID of the     
  target layer.  If no such target layer is found, then a dummy       
  Weights structure is returned with -1 as the value for the target   
  size, indicating that no such set of weights exists.                
 The mechanics of the routine are not hard.  All that need be done is 
  to make a search through the OUT weights list for a Weights struct  
  which has 'target' as the ID of its target layer.                   
----------------------------------------------------------------------
*/
BEGIN
   Weights_lst  *ptr_weights_lst;
   Weights      *dummy;

   ptr_weights_lst = ptr_layer->out_weights;
   while (ptr_weights_lst != NULL) BEGIN
      if (ptr_weights_lst->value->target_layer->ID == target)
         return(ptr_weights_lst->value);
      ptr_weights_lst = ptr_weights_lst->next;
   ENDWHILE

   dummy = (Weights *) sys_alloc((unsigned) sizeof(Weights));
   dummy->type = ERROR;
   return(dummy);

END /* L_get_weights */


void  L_update_learn_rates(ptr_layers, avg_err)
Layer_lst  *ptr_layers;
Sint       avg_err;
/*
----------------------------------------------------------------------
  Given an input to the hidden_front list of layers (see net.h) this 
  routine computes the new learning rates for each layer of the network
  except the input layer (the input layer has no learning rate).
----------------------------------------------------------------------
*/
BEGIN
   Layer  *cur_layer;
   
   while(ptr_layers != NULL) BEGIN
      cur_layer = ptr_layers->value;
      /*-----------------------------------*/
      /* if scale == 0, then it is assumed */
      /* that the learning rate stays same */
      /*-----------------------------------*/
      if (cur_layer->learn_scale != 0.0)
         cur_layer->cur_learn = 
            LR_calc_learn_rate(cur_layer->learn_base, cur_layer->learn_scale,
                               avg_err);
      ptr_layers = ptr_layers->next;
   ENDWHILE

END /* L_update_learn_rates */


void  L_modify_learning(layer_num, ptr_layer)
int    layer_num;
Layer  *ptr_layer;
/*
----------------------------------------------------------------------
  Prompts the user for new learning rate, scaling, and momentum 
  values.
----------------------------------------------------------------------
*/
BEGIN
   float  temp_scale;
   
   if (ptr_layer->ID == ERROR) BEGIN
      sprintf(IO_str, "\n*** layer %d does not exist ***", layer_num);
      IO_print(0);
   ENDIF
   else if (layer_num < 1) BEGIN
      sprintf(IO_str, "\n*** no learning rate associate with layer %d ***", layer_num);
      IO_print(0);
   ENDIF
   else BEGIN
      sprintf(IO_str, "\n     Enter learning rate for layer %d", layer_num);
      IO_print(0);
      sprintf(IO_wkstr, " (default=%%.f): ");
      IO_insert_format(IO_wkstr);
      sprintf(IO_str, IO_wkstr, ptr_layer->learn_base);
      IO_print(0);
      ptr_layer->learn_base = IO_get_default_float(ptr_layer->learn_base);

      temp_scale = LR_scale_default(ptr_layer->learn_base);
      sprintf(IO_str, "\n     Enter scaling factor or 0 if not desired");
      IO_print(0);
      sprintf(IO_wkstr, " (default=%%.f): ");
      IO_insert_format(IO_wkstr);
      sprintf(IO_str, IO_wkstr, temp_scale);
      IO_print(0);
      ptr_layer->learn_scale = IO_get_default_float(temp_scale);
     
      sprintf(IO_str, "\n     Enter momentum for layer %d", layer_num);
      IO_print(0);
      sprintf(IO_wkstr, " (default=%%.f): ");
      IO_insert_format(IO_wkstr);
      sprintf(IO_str, IO_wkstr, ptr_layer->momentum);
      IO_print(0);
      ptr_layer->momentum = IO_get_default_float(ptr_layer->momentum);      
   ENDELSE

END /* L_modify_learning */


void  L_show_biases(ptr_layer)
Layer  *ptr_layer;
/*
----------------------------------------------------------------------
 Given a pointer to a layer, this routine prints out all the bias values
  associated with that layer.
----------------------------------------------------------------------
*/
BEGIN
   int    i;
   float  t1;
   Sint   *bias_values;
   
   bias_values = ptr_layer->node_bias;
   for (i = 0; i < ptr_layer->num_nodes; i++) BEGIN
      t1 = C_Sint_to_float(bias_values[i]);
      sprintf(IO_wkstr, "bias %d = %%.f  ", i);
      IO_insert_format(IO_wkstr);
      sprintf(IO_str, IO_wkstr, t1);
      if (IO_more (0) == ERROR) return;
      if ((i+1) % 3 == 0) BEGIN
         sprintf(IO_str, "\n");
         if (IO_more (0) == ERROR) return;
      ENDIF
   ENDFOR
   sprintf(IO_str, "\n\n");
   if (IO_more (0) == ERROR) return;
   
END /* L_show_biases */


void  L_show_learn_rates(ptr_layer)
Layer  *ptr_layer;
/*
----------------------------------------------------------------------
  Given a pointer to a layer, this routine prints out the learning rate
  momentum and scale factors for that layer.
----------------------------------------------------------------------
*/
BEGIN
   sprintf(IO_str, "\nLayer %d", ptr_layer->ID);
   IO_print(0);
   sprintf(IO_wkstr, "\n   Base learning rate    :%%.f");
   IO_insert_format(IO_wkstr);
   sprintf(IO_str, IO_wkstr, ptr_layer->learn_base);
   IO_print(0);
   if (ptr_layer->learn_scale != 0) BEGIN
      sprintf(IO_wkstr, "\n   Scaling factor        :%%.f");
      IO_insert_format(IO_wkstr);
      sprintf(IO_str, IO_wkstr, ptr_layer->learn_scale);
      IO_print(0);
   ENDIF
   sprintf(IO_wkstr, "\n   Delta weight constant :%%.f\n");
   IO_insert_format(IO_wkstr);
   sprintf(IO_str, IO_wkstr, ptr_layer->momentum);
   IO_print(0);
   
END /* L_show_learn_rates */
