/*
*  Functions to ask user questions and if possible use the digitizer cursor for
*  input.
*    ask_yes_no(),  ask_driver_yes_no(),  ask_driver()
*
*	GRASS 3.0, Spring 88,    -mh
*/

/*
*  Before using ask_driver_yes_no() make a call to D_cursor_buttons() to make
*  sure that a specific driver can handle the question.  Otherwise use the
*  keyboard for input.
*/


ask_yes_no(quest)
	char  *quest ;
{

#ifdef CURSORKEYS
	if (D_cursor_buttons())
	    return( ask_driver_yes_no(quest)) ;
	else
	    return( curses_yes_no(4, quest)) ;
#else	    
    return( ask_driver_yes_no(quest)) ;
#endif

}

ask_driver_yes_no(quest)
	char  *quest ;
{
	int  button ;
	int  first_button ;
	int  priority_on ;

	char buf[100] ;

    first_button = D_start_button() ;

#ifdef USE_KEYS
    _Write_info(1, "  USE DIGITIZER CURSOR OR KEYBOARD FOR INPUT");
#else
    if (D_cursor_buttons() < 5)
        _Write_info(1, "  USE KEYBOARD FOR INPUT");
    else
	_Write_info(1, "  USE DIGITIZER CURSOR FOR INPUT");
#endif

    sprintf( buf, "Key '%d' for YES   :   Key '%d' for NO",
	first_button, first_button+1);
    _Write_info(2, buf);

    Write_info(4, quest);

    priority_on = set_priority() ;

    while (1)
    {

	button = ask_driver();

	switch (button)
	{
	    case 1:
    		if (priority_on == 0)
			unset_priority() ;
		return(1);
		break ;
	    case 2:
    		if (priority_on == 0)
			unset_priority() ;
		return(0);
		break ;
	    default:
	    /**
		Write_info(4, "Please answer yes or no");
		sleep(2);
	    **/
		break ;
	}
    }
}

/*  ask until any key is hit, returns which button hit  */
ask_driver()
{
	double  X ;
	double  Y ;

	return( ask_driver_raw( &X, &Y) ) ;
}

get_digitizer_button_xy (X, Y)
	double  *X ;
	double  *Y ;
{
	double Xraw, Yraw;
	int ret;

	ret = ask_driver_raw( &Xraw, &Yraw) ;
	transform_a_into_b ((double)Xraw, (double)Yraw, X, Y) ;

	return ret;
}


/*  making calls directly to driver  */

ask_driver_raw( X, Y) 
	double  *X ;
	double  *Y ;
{
	int  button ;
	int  priority_on ;

	priority_on = set_priority() ;
/*	D_clear_driver() ; */
	button = D_ask_driver_raw( X, Y) ;

    	if (priority_on == 0)
		unset_priority() ;

	flush_keyboard ();  /* for SCS problems */
/*DEBUG*/ debugf ("leaving ask_driver_raw: button = %d\n", button);

	return(button) ;
}

coll_a_pnt ( x, y)
    double    *x;
    double    *y;
{


    D_clear_driver() ;
    
    return( _coll_a_pnt(x,y) ) ;

}	    /*  coll_a_pnt ()  */


_coll_a_pnt ( x, y)
    double    *x;
    double    *y;
{

    int	Xraw, Yraw;
    int	KeyHit ;

    KeyHit = D_read_raw (&Xraw, &Yraw) ;
    
    transform_a_into_b ((double) Xraw, (double) Yraw, x, y) ;
#ifdef DEBUG
    debugf ("BUTTON: %d RAW: x = %d y = %d    WORLD: x = %lf y = %lf\n",
				   KeyHit, Xraw, Yraw, *x, *y);
#endif

    return(KeyHit ) ;

}	    /*  _coll_a_pnt ()  */
