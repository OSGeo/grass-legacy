
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
  WEIGHTS MODULE  
----------------------------------------------------------------------
 This module represents a weight matrix, which is defined to       
  be the set of all weight connections between the nodes of        
  one layer and another.  For simplicity sake, I assumed that      
  these connections are only one way, (ie not symetric).           
  A symetric connection could be made by using two weight          
  matricies.							     
 Note, however, that all of the formulas used here adhere to       
  the generalized delta theories described by Rumelhart. Such      
  a net defines itself as not having symetric connections!         
 Also, I assumed that any set of weights connecting a source       
  layer to a target layer included ALL of the nodes in the two     
  layers. Again, this was just a design choice to make the code    
  easier, but note that this is not necessary in the Rumelhart     
  scheme (actually, Rumelhart, Hinton, & Williams). This choice    
  enabled me to NOT have to define a NODE module.  Instead, all    
  nodes within a layer were treated the same.                      
 Finally, note that the weights between two layers are defined     
  here as a one-dimensional array of sufficient size.  This may    
  seem strange, after all, wouldn't a 2-dimensional array be the   
  way to go?  The answer is, yes, 2-d is probably the natural      
  way to approach the weight matrix.  The reason for choosing a    
  1-d representation is for SPEED.  We wanted to have an array     
  representation which we controlled so that indexing through      
  the weights could be made as fast a possible.  This is moti-     
  vated by the fact that most of the computation time in this      
  type of nerual net is spent working with the weights and nodes   
  of two layers.  Having the means to quickly access this info     
  is absolutely essential for the program to run quickly.          
-------------------------------------------------------------------
  4-5-88 I've decided to add another type of connection scheme to  
  the network structure, and thus the weights representation has   
  to change. There are two differences. First, I've added an int   
  to specify the type as either PATTERNED or CONNECT_ALL (see the  
  "common.h" file).  If the type is connect all, then all the      
  source nodes are connected to all the target nodes, otherwise    
  the connections occur in some sort of pattern (specified by the  
  user). This pattern can be relatively complex, and thus to save  
  time a "decoder" array, exactly as long as the "values" array,   
  can be used. The decoding values need then only be calculated    
  once, with the corresponding array indicies to the LARGER of the 
  two layers stored in the "decoder" matrix. Thus, for the weight  
  stored in values[5], there will be an index in decoder[5] which  
  will indicate the node of the larger layer which is connected by 
  this weight.                                                     
-------------------------------------------------------------------
  9-5-89 To make things go (hopefully) faster during the learning
  process, I have decided to use function pointers to determine
  how the weights should be propagated/updated. This amounts to 
  writing three versions of the propagating/updating routines and
  setting function pointers appropriately. What made most sense to 
  me was to put those function pointers with each weight, since the
  weights are dealt with as individuals. This should make the code
  much cleaner and should speed up the learning process somewhat.
-------------------------------------------------------------------
*/
typedef struct weights_str {
   int               type;           /* type of connection scheme    */
   struct layer_str  *source_layer;  /* layer that weights come FROM */ 
   struct layer_str  *target_layer;  /* layer that weights go TO     */
   int               source_size;    /* # of nodes in source layer   */
   int               target_size;    /* # of nodes in target layer   */
   Sint              *values;        /* array of Sints, size equal   */
                                     /* to source_size * target_size */
                                     /* IFF the type is PATTERENED,  */
                                     /* the number of values will be */
                                     /* X_dim * Y_dim of the smaller */
                                     /* layer, times the area of the */
                                     /* mapping rectangle.           */
   int16             *decoder;       /* array of indices for the     */
                                     /* nodes of the LARGER of the   */
                                     /* two layers IFF these weights */
                                     /* are of type PATTERNED.       */
	                                 /* Otherwise, NULL.             */
   int               map_area;       /* IFF patterned, this area is  */
                                     /* needed for propagating input */
                                     /* It indicates the size of the */
                                     /* region created by a pattern  */
                                     /* connection scheme, ie, P * Q */
   Sint              *prev_deltaW;   /* previous weight change, ie,  */
                                     /* from last back propigation.  */
   D_Sint            (*f_prop) ();   /* The three function pointers  */
   D_Sint            (*b_prop) ();   /* show which functs. are to be */
   void              (*w_update) (); /* used for propagation and for */
} Weights;                           /* weight updates (see prop.c). */


/*
-------------------------------------------------------------------
 The need for the structure below arises within the Layer module.  
  Each layer needs to keep two lists for both the forward and the  
  backward propigation procedures described by Rumelhart.  These   
  are lists of the weights which exist between the layer in ques-  
  tion and other layers.  The structure below provides the means   
  for building such a linked list.                                 
-------------------------------------------------------------------
*/

typedef struct weights_lst_str {
   Weights                 *value;   /* ptr to a weights structure   */
   struct weights_lst_str  *next;    /* ptr to next set of weights   */
} Weights_lst;
