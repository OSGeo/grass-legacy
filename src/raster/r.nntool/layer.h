
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
  LAYER MODULE  
----------------------------------------------------------------------
  A layer is defined in this system as a grouping of nodes which are  
   treated in a uniform manner.  This is similar to the slab concept  
   put forth by Hinton in his discussion of the Boltzman machine.     
  In the generalized delta concept, all that is necessary is that     
   all of the connections between nodes by unidirectional, and that   
   no cycles be placed in the graph.  Nothing, however, is stated     
   which requires that all nodes within a layer be connected to all   
   nodes in another layer; weights between layers are node-to-node.   
   However, to keep things simple, we decided NOT to detail each node 
   connection, but rather treat all nodes of a layer identically.     
  At a later time, control over particular nodes may be desirable,    
   and I would suggest that a new type (and module) be created for    
   the concept of a NODE at that time.                                
----------------------------------------------------------------------
  4-4-88 I've added a new feature which allows the connections be-    
   tween layers to be more flexible than the strict connect-all       
   scheme. The idea is that each layer is given dimensional info, as  
   though it represented a grid.  Then, two layers can be connected   
   via smaller rectangular "maps" which map several nodes "close      
   together" in the larger layer to one node in the smaller layer.    
   These mappings can also overlap. Anyway, the upshot is that the    
   layer information must be stored somewhere, and this is the most   
   appropriate spot, in the X_dim and Y_dim elements.                 
----------------------------------------------------------------------
  4-6-89 Interesting! Almost a year later to the day I'm adding another
   change to the layer structure. Anyway, there are two new additions
   to the layer structure: "node_bias" and "prev_deltaB". Apparently I
   had some difficulty understanding all the theory outlined by Rumelhart
   in the PDP book since I left out the bias term. It functions exactly
   like a weight, but with one difference. The bias is assumed to be
   "connected" from an imaginary incoming node whose value is always 
   1.0 to the node in question. Thus the bias term is exactly that: it
   biases the final dot product sum before it is fed through the 
   activation function.
----------------------------------------------------------------------
  4-12-89 today I'm adding another element to the layer structure. This
   is in response to the idea that each layer might well be best treated
   as having a unique learning rate. In part this arises out of work on
   the spectrum problem, where the number of connections between different
   layers varies from 1200 to 30. The large range makes small weight
   changes drastic for one layer and insignificant for another. The
   "max_incoming" integer indicates the maximum weights coming into the
   layer.
----------------------------------------------------------------------
  4-13-89 Because there is some debate as to whether or not each layer
   should have its own learning rate, I have moved the three structure
   elements regarding learning to this layer structure from the net 
   structure.
----------------------------------------------------------------------
*/
typedef struct layer_str {
   int    ID;                    /* identification number for layer      */
   int    num_nodes;             /* num of nodes in the layer            */
   int    max_incoming;          /* max weights coming into the layer    */
   int    X_dim;                 /* X dimension of the layer             */
   int    Y_dim;                 /* Y dimension of the layer             */
   D_Sint cur_learn;             /* current learning rate for layer      */
   float  learn_base;            /* base learning rate for layer         */
   float  learn_scale;           /* scale for learning rate if csc used  */
   float  momentum;              /* momentum for layer                   */
   Sint   *node_outputs;         /* An array of Sint's, size = num_nodes */
   Sint   *node_deltas;          /* as above, but for delta values       */
   Sint   *node_bias;            /* as above, but for bias values        */
   Sint   *prev_deltaB;          /* as above, but for last bias change   */
   struct weights_lst_str  *in_weights;
                                 /* ptr to a list of weights which have  */
                                 /* this layer as their target           */
   struct weights_lst_str  *out_weights; 
                                 /* ptr to a list of weights which have  */
                                 /* this layer as their source           */
} Layer;


/*
-----------------------------------------------------------------------
 the following is a structure for keeping a doubly linked list of the  
  layers in the net.  This list should be created in the order that    
  the layers should be evaluated for forward propagation.  Then, of    
  course, backward propigation can be done in the reverse order.  Thus 
  the list needs to be doubly linked so that one may move through it   
  in either order.                                                     
-----------------------------------------------------------------------
*/
typedef struct layer_lst_str {
   Layer                 *value;  /* a ptr to a layer                    */
   struct layer_lst_str  *prev;   /* ptr to previous element in the list */
   struct layer_lst_str  *next;   /*  "   "   next     "      "  "    "  */
} Layer_lst;

