
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
  OVERALL NERUAL NET MODULE  
----------------------------------------------------------------------
  The structure defined below for a Net is pretty self-explanatory. 
  One must keep track of the number of layers and the parameters for
  the back propagation algorithm (ie, learning_rate, momentum, etc.)
  There are a few peculiarities, however. Note that the input and 
  output layer are separated from the rest of the layers.  This is
  because these layers are given their values (during learning) from
  a file of inputs specified by the user, unlike other layers which 
  have their values calculated.  Also note that there are TWO pointers
  to the list of layers (a doubly linked list). One points to the
  beginning for use when propagating forwards, and the other to the end
  for propagating backwards.  Lastly, please note that no weights are 
  shown here as part of the net.  This is because there is really no way
  to talk about a weight without refering to the layers which it connects. 
  Thus the weight definitions go along with the layer descriptions.        
----------------------------------------------------------------------
 4-13-89  I got rid of the learning rate, learning scale, and momentum
  from this structure. Because some discrepancies exists as to whether 
  or not each layer should have a different learning rate, I decided to
  move these elements to the layer structures.  If a constant rate is
  still desired, it will end up being the same for each layer.
----------------------------------------------------------------------
*/


typedef struct net_str {
   int        ID;            /* identification tag, -1 if error during   */
                             /* creation of the net.                     */
   int        num_layers;    /* ALL layers in the net                    */
   Layer      *input_layer;
   Layer      *output_layer;
   Layer_lst  *hidden_front; /* ptr to start of hidden layers list       */
   Layer_lst  *hidden_back;  /* ptr to end of hidden layers list         */
                             /* Note that the hidden layers list         */
                             /* should be in order of evaluation so that */
                             /* forward/backward propagation will work   */
   int        use_biases;    /* boolean indicating bias used/not used    */
   int        num_inputs;    /* number of nodes in input layer           */
   int        num_outputs;   /* number of nodes in output layer          */
   int        num_io_pairs;  /* number of i/o pairs for testing          */
} Net;
