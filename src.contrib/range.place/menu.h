/****************************************************************/
/*                                                              */
/*	menu_option_box.h	in  ~/src/i_range		*/
/*                                                              */
/*	This header file defines the data structure of every	*/
/*	option box in the menu tree.                            */
/*                                                              */
/****************************************************************/

       	struct box{

	int l,r,t,b;		/* location of box on screen	*/
        char text[25];          /* text inside box              */
        struct box *brother;    /* pointer to brother box	*/
        struct box *parent;  	/* pointer to parent box	*/
        struct box *child;      /* pointer to child box		*/
        int i;                  /* no: of funcs. to be executed */
        int *func_list;         /* ptr to array of func numbers */

        } ;

/****************************************************************/
