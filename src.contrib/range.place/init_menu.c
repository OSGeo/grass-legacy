/****************************************************************/
/*                                                              */
/*      initialize_menu.c  in      ~/src/i_range                */
/*                                                              */
/*	This recursive function reads in the information for 	*/
/* 	the menu from the file pointed to by "fpr" and		*/
/*	initializes all the option boxes in the multiple layers	*/
/*	of the interconnected rings of boxes.			*/
/*                                                              */
/****************************************************************/      

#include "menu.h"
#include  <stdio.h>

struct box *initialize_menu(parent,first_child,fpr)

        struct box *parent,*first_child;
        FILE *fpr;

{
        struct box *box;
        char *malloc();
        static char text[25];
        int left,top,right,bot;
        char continue_flag,brother_flag,child_flag;
        int no_of_functions,r;
        int *int_ptr;

	/*	read location parameters and text for box	*/
	if(fscanf(fpr,"%3d%3d%3d%3d%16s",
			&left,&top,&right,&bot,text)!=EOF)
	{
	/*	allocate memory for the option box		*/
        box= (struct box *) malloc(sizeof(struct box));
	
	
	/*  first_child points to the first child box in the	*/
	/*  child ring						*/
        if(first_child == NULL) first_child = box;
 
	/*	assign the location parameters to fields of box	*/
        box->l = raster_x(left);
        box->t = raster_y(top);
        box->r = raster_x(right);
        box->b = raster_y(bot) ;
        box->parent = parent;

	/*	assign text to field of box			*/	
        strcpy(box->text,text);
 
	/* read flags to direct flow of control and number of	*/
	/* functions to be executed if mouse clicked inside box	*/
        fscanf(fpr," %c %c %c%3d",&continue_flag,&brother_flag,
				  &child_flag,&no_of_functions);
 
	/*  allocate an array of integers to hold the index     */
	/*  numbers (specified in the func table) of the funcs  */
	/*  to be executed 					*/
        int_ptr = (int *) malloc(no_of_functions * sizeof(int));
 

	/*	read in the index numbers in the integer array	*/
	for(r=0; r<no_of_functions ; r++)
        {
        fscanf(fpr,"%3d",int_ptr+r);
        }

	/*  assign number of functions and the index no: array	*/
	/*  to appropriate fields in box			*/
        box->i = no_of_functions;
        box->func_list = int_ptr;
 

        if(child_flag=='n')
	{	/* if no child,make child pointer point to NULL	*/     
	box->child = NULL;
	}
        else	
	/* otherwise, make recursive call to initialize child	*/ 
	box->child = initialize_menu(box,NULL,fpr);
 
	if(brother_flag=='n')
	{	/* if no brother, make brother ptr point to NULL*/
	box->brother = NULL;
	}
        else 	/*	otherwise				*/

	if (continue_flag == 'l') 
	/*	close circular ring if last box in ring		*/
	box->brother = first_child; 

        else 
	/* otherwise, make recursive call to initialize brother	*/
	box->brother = initialize_menu(parent,first_child,fpr);

	/*  return the address of the newly initialized box	*/
        return(box);                                           
	}

}     

/***************** END OF FUNCTION "INITIALIZE_MENU" *************/

 

