/*******************************************************************************
*        This group of programs collect input from all graphics input devices: *
*                                                                              *
*  long raw_diginput();   reads raw device & collects 1 packet of information  *
*  long decode_digpkt();  decodes 1 packet of information from raw device      *
*  long start_ginput();   opens a serial line to raw device & sets up data flow*
*  long query_ginput();   queries a serial device to send a packet of info.    *
*  long stop_ginput();    stops data flow & closes serial line to raw device   *
*  long ginput_ctl();     executes a list of actions on raw device/serial line *
*  long read_digfile();   reads & interprets file describing the digitizer     *
*  long read_value();     reads a single start/stop/or query action value.     *
*  long digdefaults();    sets all digitizer default values                    *
*  long set_diginternals(); sets DIGDEVICE variables not directly set by file  *
*  long ginput_setup();   arranges for setup of all graphics input devices     *
*  int  dig_input();      manages all graphics input, screen disp, & button str*
*  long bslash();         converst backslash combinations & returns length     *
*  long dparse();         parses strings (carefully)                           *
*                                                                              *
*******************************************************************************/

static int Debug = 0;

#include <sys/ioctl.h>
#include <ctype.h>
#include <string.h>
/*
#include "gis.h"
*/
#include "ginput.h"

#ifdef  USE_TERMIO
#include <fcntl.h>
#endif

#ifndef USE_TERMIO
#include <sgtty.h>
#include <sys/file.h>
#endif

#include <signal.h>
#include <setjmp.h>

#define MAX_TRIES 5

static jmp_buf jump;
static int alarm_time = 2;

dead ()
{
    longjmp (jump, 1);
}

/*******************************************************************************
*    raw_diginput                                                              *
*       This program tries a finite number of times to read a packet of        *
*    information from a raw graphics input device.  If read, the packet is     *
*    decoded.                                                                  *
*                                                                              *
*    Parameters:   none                                                        *
*                                                                              *
*    Values returned:     0 = no input read                                    *
*                         1 = input read                                       *
*                                                                              *
*        Returns value greater than zero if input is read.   otherwise 0       *
*******************************************************************************/


long
raw_diginput()
{
    static char string[512];       /*  for input & input manipulation */
    static char string2[96];       /*  for copying                    */
    static char *ptr,*ptr2;
    static char *endptr,*endpkt;

    static long rtn;
    static unsigned long bytes;
    static long readflag;
    int dead();
    int (*sigalarm)();

        /************* initialize pointers *******/
    if(ptr == (char *) 0) 
    {    
	endptr = string;
        ptr = string;
    }


try_again:
    if (setjmp (jump))
    {
	signal (SIGALRM, SIG_DFL);
	alarm (0);
    }
        /************* query digitizer device if needed *************/
    if(digdevice.mode == QUERY_MODE && digdevice.no_query_actions)
    {
        query_ginput();
    }


     /************* find a good packet (MAX_TRIES reads max, if necess) ***/
    readflag = 0;
    if(digdevice.file_type)          /*** ascii file ***/
    {
	for(;;)
        { 
	    /********** find a non lf/cr byte ************************/
            for(; (ptr<endptr) && (*ptr==(char) 10 || *ptr==(char) 13); ptr++);
            
	    /********** check for enough non-cr/lf bytes *************/
            endpkt = ptr + digdevice.packet_length[0];
            ptr2 = ptr + 1;
            for(; (ptr2 < endpkt) && (ptr2 < endptr) ; ptr2++)
            {

		if((*ptr2==(char) 10) || (*ptr2==(char) 13))
		 {
                    break;
		}
            }


                /********** read if necess or break out ******************/
            if(ptr2 == endpkt)  /** a good packet **/
	    {
                break;
	    }
            else if(ptr2 < endptr)    /*** not a good packet ***/
	    {
                ptr = ptr2;
	    }
            else
            {
		if (readflag > MAX_TRIES)           /*** read already tried ***/
		{
                    return(0);
		}
                else                   /*** read not tried yet ***/
                {

                    if(endptr - string > 400)
                    {        
			/*** copy tail to beginning of string ***/
                        for(ptr2=string; ptr < endptr; ptr++, ptr2++)
                            *ptr2 = *ptr;
                        ptr = string;
                        endptr = ptr2;
                    }

                        /*** calc bytes to read (up to 500)   ***/
                    bytes = string + 500 - endptr;

                        /*** read bytes ASCII_READ ***/

		    {
			sigalarm = (int (*)()) signal (SIGALRM, dead);
			alarm (alarm_time);
		    }
                    bytes = read(fd_ginput,endptr,bytes);
		    {
			alarm (0);
			signal (SIGALRM, sigalarm);
		    }

                    if(bytes <= 0)
		    {
                        return(0);
		    }

                    if (bytes >= digdevice.packet_length[0])
		        readflag = MAX_TRIES;
		    else 
			readflag++;
                    endptr += bytes;

                        /*** remove parity, if any ***/
                    for(ptr2 = ptr; ptr2 < endptr; ptr2++)
                        *ptr2 &= 127;

                }
            }
        }
            /************* decode packet ********************************/
        rtn = decode_digpkt(ptr);
        ptr += digdevice.packet_length[0];
    }
    else             /**** binary file type ***/
    {
        for(;;)
        {        /********** find a sync byte *****************************/
            for(; (ptr<endptr) && !(*ptr & digdevice.sync_mask); ptr++);

                /********** check for full packet ************************/
                /********** check for enough non-sync bytes **************/
            endpkt = ptr + digdevice.packet_length[0];
            ptr2 = ptr + 1;
            if(digdevice.sync_bytes > 1)
            {
		for(; (ptr2 < endpkt) && (ptr2 < endptr); ptr2++)
		{
                    if(*ptr2 & digdevice.sync_mask)
		    {
                        break;
		    }
                }
            }
            else
            {
		if((*ptr & digdevice.sync_offbits) == 0)
                    for(; (ptr2 < endpkt) && (ptr2 < endptr); ptr2++);
            }
#ifdef MICE
            /** if optional bytes, check for beginning of next packet **/
            if(digdevice.packet_length[0] != digdevice.packet_length[1])
            {    
		if(ptr2 == endpkt)
                {        
		    /***** first, check for another sync byte, or max_length **/
                    endpkt = ptr + digdevice.packet_length[1];
                    if(digdevice.sync_bytes > 1)
                    {    
			for(; (ptr2 < endpkt) && (ptr2 < endptr); ptr2++)
                        {    
			    if(*ptr2 & digdevice.sync_mask)
                            {    
				endpkt = ptr2;
                                break;   
                            }
                        }
                    }
                    else
                    {    
			for(; (ptr2 < endpkt) && (ptr2 < endptr); ptr2++)
                        {    
			    if((*ptr2 & digdevice.sync_mask) && 
				 ((*ptr & digdevice.sync_offbits) == 0))
                            {   
				endpkt = ptr2;
                                break;   
                            }
                        }
                    }

                    /*** finally, check for a button press if not max_len ***/
                    if(ptr2 < endpkt)
                    {    
			for(i=0; i<digdevice.brecipe_length; i++)
                        {
                            if(digdevice.brecipe[i].b_no < 
						   digdevice.packet_length[0])
                            {    
				if(ptr[digdevice.brecipe[i].b_no] & 
						    digdevice.brecipe[i].mask)
                                {    
				    endpkt = ptr2;
                                    break;
                                }
                            }
                        }
                    }
                }
            }
#endif

                /********** read if necess or break out ******************/
            if(ptr2 == endpkt)  /** a good packet **/
	    {
                break;
	    }
            else if(ptr2 < endptr)    /*** not a good packet ***/
	    {
                ptr = ptr2;
	    }
            else
            {
		if(readflag >= MAX_TRIES)           /*** read already tried ***/
		{
                    return(0);
		}
                else                   /*** read not tried yet ***/
                {
                    if(endptr - string > 400)
                    {
			/*** copy tail to beginning of string ***/
                        if(ptr != string)
                        {
			    for(ptr2=string; ptr < endptr; ptr++, ptr2++)
			    {
                                *ptr2 = *ptr;
			    }
                            ptr = string;
                            endptr = ptr2;
                        }
                    }

                        /*** calc bytes to read (up to 500)   ***/
                    bytes = string + 500 - endptr;

                        /*** read bytes ***/
/*****************************************************************************/
                    {
			sigalarm = (int (*)()) signal (SIGALRM, dead);
			alarm (alarm_time);
		    }
		    bytes = read(fd_ginput,endptr,bytes);
		    {
			alarm (0);
			signal (SIGALRM, sigalarm);
		    }
                    if(bytes <= 0)
		    {
                        return(0);
		    }
                    if (bytes >= digdevice.packet_length[0])
		        readflag = MAX_TRIES;
		    else 
			readflag++;
                    endptr += bytes;
                }
            }
        }

#ifdef MICE
            /************** if binary optional bytes, copy string ***********/
        if(digdevice.packet_length[0] != digdevice.packet_length[1])
        {        /*** copy packet until max length or next packet reached **/
            string2[0] = *ptr;
            ptr++;
            ptr2 = string2 + 1;
            endpkt = ptr + digdevice.packet_length[1];
            for(;ptr < endpkt  &&  ptr < endptr ; ptr++, ptr2++)
            {    if(*ptr & digdevice.sync_mask)
                    break;
                *ptr2 = *ptr;
            }
                /*** fill out to max length with nulls if necessary ***/
            endpkt = string2 + digdevice.packet_length[1];
            for(;ptr2 < endpkt; ptr2++)
                *ptr2 = (char) 0;
	    rtn = decode_digpkt(string2);
        }
        else
#endif
        {
	    rtn = decode_digpkt(ptr);
            ptr += digdevice.packet_length[0];
        }
    }

        /************* return ***************************************/
    return(rtn);
}

/*******************************************************************************
*     decode_digpkt                                                            *
*          This program decodes a packet of binary or ascii digitizer          *
*     information according to the data in the DIGDEVICE structure.            *
*     X and Y are changed so that positive is up and to the right.             *
*                                                                              *
*     Parameters:   char string[];    a packet from the raw graphics input dev *
*                                                                              *
*     Return values:     0 = error decoding                                    *
*                        1 = packet decoded successfully                       *
*******************************************************************************/

long decode_digpkt(string)
char string[];
{
    static char str[96];

    static long i;
    static long length;


        /************** set button value ********************************/
    if( !digdevice.file_type )    /*** binary ****/
    {
	ginfo.button = 0;
        for(i=0; i<digdevice.brecipe_length; i++)
        {    
	    if(digdevice.brecipe[i].shift >= 0)
		ginfo.button |= (string[digdevice.brecipe[i].b_no] & digdevice.brecipe[i].mask) << digdevice.brecipe[i].shift;
            else
                ginfo.button |= (string[digdevice.brecipe[i].b_no] & digdevice.brecipe[i].mask) >> -digdevice.brecipe[i].shift;
        }
	    
	/* offset to get button values to 1-15 range */
	ginfo.button += digdevice.buttonoffset; 
	
	/**added 9/92 for v.digit make sure button is pressed****************/
	if (digdevice.push_byte  &&
	    !(digdevice.push_mask & string[digdevice.push_byte-1]))
	    {
	    ginfo.button = 0;
	    }
    }
    else                /***** ascii format ****/
    {
	char buf[2];
	int val;

	/* instead of saving the char, turn it into a number from 1-16 */
/*DEBUG*/if (Debug > 1) 
/*DEBUG*/ fprintf (stderr, "\nbutton: '%c' upchar '%c'\nstring: <%s\n>\n", 
			     string[digdevice.b_byte],
			     digdevice.button_up_char, string);
	buf[0] = string[digdevice.b_byte];
	buf[1] = 0;

	/**added 9/92 for v.digit make sure button is pressed****************/
	/* push byte specified in format */
	if ((digdevice.push_byte >= 0) &&
	    string[digdevice.push_byte] == digdevice.button_up_char)

		ginfo.button = 0;
	else
	{
	 /* else  up_char specified for button number field default: <space> */
	
	    if (buf[0] == digdevice.button_up_char)
		ginfo.button = 0;
	    else
	    {
		sscanf (buf, "%x", &val);
		val += digdevice.buttonoffset;
		/*ginfo.button = (val >=1 && val <= digdevice.buttons) ? val : 0;*/
		/* NOTE this doesnt allow for keys above 9 
		** this was a hack to get digit working.  should
		**  be fixed later
		*/

		ginfo.button = (val >=1 && val <= 9) ? val : 0;
	    }
	}
	
    }


        /************** set x & y raw values ****************************/
    if( !digdevice.file_type )          /*** binary ****/
    {        /*** get x coord **/
        ginfo.rawcoord[0] = 0;
        for(i=0; i<digdevice.xrecipe_length; i++)
        {    if(digdevice.xrecipe[i].shift >= 0)
                ginfo.rawcoord[0] |= (string[digdevice.xrecipe[i].b_no] & digdevice.xrecipe[i].mask) << digdevice.xrecipe[i].shift;
            else
                ginfo.rawcoord[0] |= (string[digdevice.xrecipe[i].b_no] & digdevice.xrecipe[i].mask) >> -(digdevice.xrecipe[i].shift);
        }
            /*** get y coord **/
        ginfo.rawcoord[1] = 0;
        for(i=0; i<digdevice.yrecipe_length; i++)
        {    if(digdevice.yrecipe[i].shift >= 0)
                ginfo.rawcoord[1] |= (string[digdevice.yrecipe[i].b_no] & digdevice.yrecipe[i].mask) << digdevice.yrecipe[i].shift;
            else
                ginfo.rawcoord[1] |= (string[digdevice.yrecipe[i].b_no] & digdevice.yrecipe[i].mask) >> -digdevice.yrecipe[i].shift;
        }
            /**** take sign information into account ********/
        switch(digdevice.sign_type) {
        case 0:      /*** 0negative ***/
            if(!(ginfo.rawcoord[0] & digdevice.xsign_mask)) /** x negative **/
                ginfo.rawcoord[0] = -(ginfo.rawcoord[0] & ~digdevice.xsign_mask);
            else
                ginfo.rawcoord[0] = ginfo.rawcoord[0] & ~digdevice.xsign_mask;

            if(!(ginfo.rawcoord[1] & digdevice.ysign_mask)) /** y negative **/
                ginfo.rawcoord[1] = -(ginfo.rawcoord[1] & ~digdevice.ysign_mask);
            else
                ginfo.rawcoord[1] = ginfo.rawcoord[1] & ~digdevice.ysign_mask;
            break;
        case 1:      /*** 1negative ***/
            if(ginfo.rawcoord[0] & digdevice.xsign_mask) /** x negative **/
                ginfo.rawcoord[0] = -(ginfo.rawcoord[0] & ~digdevice.xsign_mask);
            if(ginfo.rawcoord[1] & digdevice.ysign_mask) /** y negative **/
                ginfo.rawcoord[1] = -(ginfo.rawcoord[1] & ~digdevice.ysign_mask);
            break;
        case 2:      /*** 2s complement ***/
            if(ginfo.rawcoord[0] & digdevice.xsign_mask) /*xnegative,propagate1s*/
                ginfo.rawcoord[0] = ginfo.rawcoord[0] | digdevice.xsign_mask;
            if(ginfo.rawcoord[1] & digdevice.ysign_mask) /*ynegative,propagate1s*/
                ginfo.rawcoord[1] = ginfo.rawcoord[1] | digdevice.ysign_mask;
            break;
        }
    }
    else                /***** ascii format ****/
    {        /*** get x coord ***/
        length = digdevice.x_byten - digdevice.x_byte1 + 1;
        strncpy(str,&(string[digdevice.x_byte1]),length);
        str[length] = (char) 0;
        sscanf(str,"%ld",&ginfo.rawcoord[0]);

            /*** get y coord ***/
        length = digdevice.y_byten - digdevice.y_byte1 + 1;
        strncpy(str,&(string[digdevice.y_byte1]),length);
        str[length] = (char) 0;
        sscanf(str,"%ld",&ginfo.rawcoord[1]);
	
    }

        /**** check for y_positive = down ***/
    if(digdevice.y_positive)
        ginfo.rawcoord[1] = -ginfo.rawcoord[1];

        /**** check for x_positive = left ***/
    if(digdevice.x_positive)
        ginfo.rawcoord[0] = -ginfo.rawcoord[0];

    return(1);
}

/*******************************************************************************
*   start_ginput                                                               *
*       This program opens a graphics input device according to the path and   *
*   parameters in the DIGDEVICE structure.                                     *
*                                                                              *
*   parameters:   char dev_path[];      path of device file.                   *
*                                                                              *
*   Values returned:  0 - device opened & fd set ok.                           *
*                     1 - failure, there's trouble in River City               *
*******************************************************************************/

long
start_ginput(dev_path, mode)
    char dev_path[];
    int mode;
{
#ifdef USE_TERMIO
    struct termio termvar;
#else
    int             disc ;
    struct  sgttyb  sgttyb ;
    struct  tchars  tchars;
    struct  ltchars ltchars;
#endif

    char tmp_msg[STRLIST];
    long cursorx,cursory;
    double tend,t;
    int rtn;
    int x, y;

        /***** Open the digitizer file device ***********/
#ifdef USE_TERMIO
    if( (fd_ginput = openf( dev_path, 2 ))  < 0 )
#else
    if( (fd_ginput = openf( dev_path, O_RDWR | O_NDELAY )) < 0 )
#endif
    {
	sprintf(tmp_msg, "Cannot open %s.  Check the device field \
in the digcap file.", dev_path);
        fprintf(stderr,"%s",tmp_msg);
        sprintf(tmp_msg, "Ensure %s has read write permissions (0666).", dev_path);
        fprintf (stderr,"%s", tmp_msg);
        return(1);
    }
#ifdef USE_TERMIO
        /**** Set initial port attributes ******************************/
    if( ioctl( fd_ginput, TCGETA, &termvar) == (int) -1 )
    {
	fprintf (stderr, "Cannot do ioctl #1 on digitizer file device");
        if(fd_ginput > 0)
            closef(fd_ginput);
        return(1);
    }

        /*** set baud rate ***/
    switch(digdevice.baud) {
    case 300:
        termvar.c_cflag = CREAD | CLOCAL | HUPCL | B300;
        break;
    case 600:
        termvar.c_cflag = CREAD | CLOCAL | HUPCL | B600;
        break;
    case 1200:
        termvar.c_cflag = CREAD | CLOCAL | HUPCL | B1200;
        break;
    case 1800:
        termvar.c_cflag = CREAD | CLOCAL | HUPCL | B1800;
        break;
    case 2400:
        termvar.c_cflag = CREAD | CLOCAL | HUPCL | B2400;
        break;
    case 4800:
        termvar.c_cflag = CREAD | CLOCAL | HUPCL | B4800;
        break;
    case 9600:
        termvar.c_cflag = CREAD | CLOCAL | HUPCL | B9600;
        break;
    case 19200:
        termvar.c_cflag = CREAD | CLOCAL | HUPCL | B19200;
        break;
    case 38400:
        termvar.c_cflag = CREAD | CLOCAL | HUPCL | B38400;
        break;
    default:
        termvar.c_cflag = CREAD | CLOCAL | HUPCL | B9600;
        break;
    }
    /*** added 9-28-92 ****************/
    {
        termvar.c_iflag = IGNBRK ;
        termvar.c_oflag = 0 ;
        termvar.c_cc[VEOF] = 0 ;
        termvar.c_cc[VEOL] = 0 ;
        termvar.c_lflag = 0 ;
    }
        /*** set parity    ***/
    switch(digdevice.parity) {
    case 1:     /** odd parity **/
        termvar.c_cflag |= PARENB | PARODD;
        break;
    case 2:     /** even parity **/
        termvar.c_cflag |= PARENB;
        break;
    }
        /*** set number of data bits ***/
    switch(digdevice.data_bits) {       /** does not include parity, if any **/
    case 5:
        termvar.c_cflag |= CS5;
        break;
    case 6:
        termvar.c_cflag |= CS6;
        break;
    case 7:
        termvar.c_cflag |= CS7;
        break;
    case 8:
        termvar.c_cflag |= CS8;
        break;
    default:
        termvar.c_cflag |= CS8;
        break;
    }
        /*** set stop bits ***/
    if(digdevice.stop_bits == 2)
        termvar.c_cflag |= CSTOPB;

        /*** do ioctl to set serial port to these characteristics */
    if( ioctl( fd_ginput, TCSETAF, &termvar) == (int) -1 )
    {    fprintf (stderr, "Cannot do ioctl #2 on digitizer file device");
        if(fd_ginput > 0)
            closef(fd_ginput);
        return(1);
    }

#else /* end USE_TERMIO */

        if (ioctl (fd_ginput, TIOCGETP, &sgttyb) < 0)
                fprintf (stderr, "ioctl failed on device");
        if (ioctl (fd_ginput, TIOCGETC, &tchars) < 0)
                fprintf (stderr,  "ioctl failed on device");
        if (ioctl (fd_ginput, TIOCGLTC, &ltchars) < 0)
                fprintf (stderr, "ioctl failed on device");
        if (ioctl (fd_ginput, TIOCGETD, &disc) < 0)
                fprintf (stderr, "ioctl failed on device");

 /*               eliminate effect of these characters.  */

    tchars.t_intrc = tchars.t_quitc = tchars.t_startc = tchars.t_stopc =  -1;
    ltchars.t_suspc = ltchars.t_dsuspc = ltchars.t_flushc
                                                    = ltchars.t_lnextc =  -1 ;


        disc = OTTYDISC ;

        /*** set baud rate ***/
    switch(digdevice.baud) {
    case 300:
        sgttyb.sg_ispeed = sgttyb.sg_ospeed = B300;
        break;
    case 600:
        sgttyb.sg_ispeed = sgttyb.sg_ospeed = B600;
        break;
    case 1200:
        sgttyb.sg_ispeed = sgttyb.sg_ospeed = B1200;
        break;
    case 1800:
        sgttyb.sg_ispeed = sgttyb.sg_ospeed = B1800;
        break;
    case 2400:
        sgttyb.sg_ispeed = sgttyb.sg_ospeed = B2400;
        break;
    case 4800:
        sgttyb.sg_ispeed = sgttyb.sg_ospeed = B4800;
        break;
    case 9600:
        sgttyb.sg_ispeed = sgttyb.sg_ospeed = B9600;
        break;
    case 19200:
        sgttyb.sg_ispeed = sgttyb.sg_ospeed = B19200;
        break;
    case 38400:
        sgttyb.sg_ispeed = sgttyb.sg_ospeed = B38400;
        break;
    default:
        sgttyb.sg_ispeed = sgttyb.sg_ospeed = B9600;
        break;
    }
        /*** set parity    ***/
        /*** (also sets number of data bits, 7 if parity, 8 if not***/
    switch(digdevice.parity) {
    case 1:     /** odd parity **/
        sgttyb.sg_flags = ODDP | RAW;
        break;
    case 2:     /** even parity **/
        sgttyb.sg_flags = EVENP | RAW;
        break;
    default:     /** no  parity **/
        sgttyb.sg_flags = RAW;
        break;
    }
        /*** set stop bits ***/

        /*** do ioctl to set serial port to these characteristics */
            
        if ((ioctl (fd_ginput, TIOCSETP, &sgttyb) < 0) ||
            (ioctl (fd_ginput, TIOCSETC, &tchars) < 0) ||
            (ioctl (fd_ginput, TIOCSLTC, &ltchars) < 0) ||
            (ioctl (fd_ginput, TIOCSETD, &disc) < 0))
        {
            fprintf (stderr, "ioctl could not set parameters");
	    closef(fd_ginput);
            return(1);
        }

#endif
    sleep_ltp((double) 0.05);

        /********** execute start actions ****************************/
    if (mode == RUN_MODE)
        rtn = start_run_mode();
    else if (mode == POINT_MODE)
        rtn = start_point_mode();
    else if(mode == QUERY_MODE)
	rtn = start_query_mode();
    else 
    {
	fprintf (stderr, "Unknown mode.\n");
	rtn = 1;
    }
    if(rtn)	
    {   
        fprintf (stderr,"Cannot do start actions on dig input device.");
        if(fd_ginput > 0)
            closef(fd_ginput);
        return(1);
    }
        /********** flush input and output queues ********************/
    sleep_ltp((double) 0.05);
#ifdef USE_TERMIO
    ioctl( fd_ginput, TCFLSH, 2);
#else
    ioctl( fd_ginput, TIOCFLUSH, 2);
#endif
	/** do an initial read on the digitizer to update dig_input variables*/
    time_ltp(&t);
    tend = t + 0.2;
    for(;t < tend; time_ltp(&t))
        dig_input(&x, &y);
        /********** return *******************************************/
    return(0);
}


start_run_mode ()
{
    if(digdevice.no_startrun_actions &&
       !ginput_ctl(digdevice.no_startrun_actions,digdevice.startrun))
    {
	digdevice.mode = RUN_MODE;
	return (0);
    }
    return (1);
}


start_point_mode ()
{
    if(digdevice.no_startpoint_actions &&
       !ginput_ctl(digdevice.no_startpoint_actions,digdevice.startpoint))
    {
	digdevice.mode = POINT_MODE;
	return (0);
    }
    return (1);
}


start_query_mode ()
{
    if(digdevice.no_startquery_actions &&
       !ginput_ctl(digdevice.no_startquery_actions,digdevice.startquery))
    {
	digdevice.mode = QUERY_MODE;
	return (0);
    }
    return (1);
}



/*******************************************************************************
*   query_ginput                                                               *
*       This program executes ACTIONs on a currently open graphics input device*
*                                                                              *
*   parameters:   none                                                         *
*                                                                              *
*   Values returned:  0 - ok.                                                  *
*                     1 - failure, there's trouble in Gotham City              *
*******************************************************************************/

long
query_ginput()
{


#ifdef QUERYFLUSH
#ifdef USE_TERMIO
    ioctl( fd_ginput, TCFLSH, 2);
#else
    ioctl( fd_ginput, TIOCFLUSH, 2);
#endif
#endif
        /********** execute query actions ****************************/
    if(ginput_ctl(digdevice.no_query_actions,digdevice.query))
    {    fprintf (stderr, "Cannot do query actions on dig input device.");
        return(1);
    }

        /********** wait a short time for the ginput device to send a packet **/
    sleep_ltp((double) 0.005);

        /********** return *******************************************/
    return(0);
}



/*******************************************************************************
*   stop_ginput                                                                *
*       This program closes a graphics input device according to the           *
*   parameters in the DIGDEVICE structure.                                     *
*                                                                              *
*   parameters:   none                                                         *
*                                                                              *
*   Values returned:  0 - device closed ok.                                    *
*                     1 - failure, there's trouble in Podunk                   *
*******************************************************************************/

long stop_ginput()
{
        /********** execute stop actions ****************************/
    if(ginput_ctl(digdevice.no_stop_actions,digdevice.stop))
    {    fprintf (stderr, "Cannot do stop actions on dig input device.");
        return(1);
    }

        /********** close file fd ************************************/
    if(fd_ginput > 0)
    {    if(closef(fd_ginput))
            return(1);
    }
    fd_ginput = -1;

        /********** return *******************************************/
    return(0);
}

/*******************************************************************************
*    ginput_ctl                                                                *
*       This program executes actions devined by the array type ACTION on      *
*    a graphics input device.  These may be to start, query, or stop the       *
*    device.                                                                   *
*                                                                              *
*    Parameters:  long no_actions; number of actions to execute                *
*                 ACTION *action;  actions to be executed                      *
*                                                                              *
*    Values returned:    0 = success,  1 = failure                             *
*                                                                              *
*******************************************************************************/
long
ginput_ctl(no_actions,action)
long no_actions;
ACTION *action;
{
#ifdef USE_TERMIO
    struct termio termvar;
#else
    int             disc ;
    struct  sgttyb  sgttyb ;
    struct  tchars  tchars;
    struct  ltchars ltchars;
#endif
    ACTION *aptr;

    double value;

    char string[96];
    char *ptr, *endptr;

    long    i;
    double    timein, timeout;	/* dpg */

        /**** Set digitizer mode.  */
    aptr = action;
    for(i=0; i < no_actions; i++, aptr++)
    {
         switch (aptr->action) {
        case 0:        /* send */
            if( write( fd_ginput, aptr->string, (unsigned) aptr->value) == -1 )
	    {
                return(1);
	    }
            break;
        case 1:        /* read */
            time_ltp(&timein);
            timeout = timein + 1.0;
            ptr = string;
            endptr = ptr + aptr->value;
            for(; (ptr < endptr) && (timein < timeout);)
            {
	        ptr += read(fd_ginput, ptr, (unsigned) aptr->value);
                time_ltp(&timein);
            }
            if(ptr < endptr)
                return(1);
            break;
        case 2:        /* wait */
            value = aptr->value / 1000.0;
            sleep_ltp(value);
            break;
        case 3:        /* baud */
#ifdef USE_TERMIO
                /**** Set initial port attributes ******************************/
            if( ioctl( fd_ginput, TCGETA, &termvar) == (int) -1 )
            {    fprintf (stderr, "Cannot do ioctl #1 on digitizer file device");
                if(fd_ginput > 0)
                    closef(fd_ginput);
                return(1);
            }
                /*** set baud rate ***/
            switch(aptr->value) {
            case 300:
                termvar.c_cflag = CREAD | CLOCAL | HUPCL | B300;
                break;
            case 600:
                termvar.c_cflag = CREAD | CLOCAL | HUPCL | B600;
                break;
            case 1200:
                termvar.c_cflag = CREAD | CLOCAL | HUPCL | B1200;
                break;
            case 1800:
                termvar.c_cflag = CREAD | CLOCAL | HUPCL | B1800;
                break;
            case 2400:
                termvar.c_cflag = CREAD | CLOCAL | HUPCL | B2400;
                break;
            case 4800:
                termvar.c_cflag = CREAD | CLOCAL | HUPCL | B4800;
                break;
            case 9600:
                termvar.c_cflag = CREAD | CLOCAL | HUPCL | B9600;
                break;
            case 19200:
                termvar.c_cflag = CREAD | CLOCAL | HUPCL | B19200;
                break;
            case 38400:
                termvar.c_cflag = CREAD | CLOCAL | HUPCL | B38400;
                break;
            default:
                termvar.c_cflag = CREAD | CLOCAL | HUPCL | B9600;
                break;
            }
                /*** set parity    ***/
            switch(digdevice.parity) {
            case 0:     /** no parity  **/
                termvar.c_cflag &= ~PARENB;
                break;
            case 1:     /** odd parity **/
                termvar.c_cflag |= PARENB | PARODD;
                break;
            case 2:     /** even parity **/
                termvar.c_cflag |= PARENB;
                break;
            }
                /*** set number of data bits ***/
	    /** does not include parity, if any **/
            switch(digdevice.data_bits) {  
            case 5:
                termvar.c_cflag |= CS5;
                break;
            case 6:
                termvar.c_cflag |= CS6;
                break;
            case 7:
                termvar.c_cflag |= CS7;
                break;
            case 8:
                termvar.c_cflag |= CS8;
                break;
            default:
                termvar.c_cflag |= CS8;
                break;
            }
                /*** set stop bits ***/
            if(digdevice.stop_bits == 2)
                termvar.c_cflag |= CSTOPB;

                /*** do ioctl to set serial port to these characteristics */
            if( ioctl( fd_ginput, TCSETAF, &termvar) == (int) -1 )
            {    
		fprintf (stderr, "Cannot do ioctl #2 on digitizer file device");
                if(fd_ginput > 0)
                    closef(fd_ginput);
                return(1);
            }

            sleep_ltp((double) 0.05);
            break;
#else /* end USE_TERMIO */

        if (ioctl (fd_ginput, TIOCGETP, &sgttyb) < 0)
                fprintf (stderr, "ioctl failed on device");
        if (ioctl (fd_ginput, TIOCGETC, &tchars) < 0)
                fprintf (stderr,  "ioctl failed on device");
        if (ioctl (fd_ginput, TIOCGLTC, &ltchars) < 0)
                fprintf (stderr, "ioctl failed on device");
        if (ioctl (fd_ginput, TIOCGETD, &disc) < 0)
                fprintf (stderr, "ioctl failed on device");

 /*               eliminate effect of these characters.  */

    tchars.t_intrc = tchars.t_quitc = tchars.t_startc = tchars.t_stopc =  -1;
    ltchars.t_suspc = ltchars.t_dsuspc = ltchars.t_flushc
                                                    = ltchars.t_lnextc =  -1 ;


        disc = OTTYDISC ;

        /*** set baud rate ***/
    switch(digdevice.baud) {
    case 300:
        sgttyb.sg_ispeed = sgttyb.sg_ospeed = B300;
        break;
    case 600:
        sgttyb.sg_ispeed = sgttyb.sg_ospeed = B600;
        break;
    case 1200:
        sgttyb.sg_ispeed = sgttyb.sg_ospeed = B1200;
        break;
    case 1800:
        sgttyb.sg_ispeed = sgttyb.sg_ospeed = B1800;
        break;
    case 2400:
        sgttyb.sg_ispeed = sgttyb.sg_ospeed = B2400;
        break;
    case 4800:
        sgttyb.sg_ispeed = sgttyb.sg_ospeed = B4800;
        break;
    case 9600:
        sgttyb.sg_ispeed = sgttyb.sg_ospeed = B9600;
        break;
    case 19200:
        sgttyb.sg_ispeed = sgttyb.sg_ospeed = B19200;
        break;
    case 38400:
        sgttyb.sg_ispeed = sgttyb.sg_ospeed = B38400;
        break;
    default:
        sgttyb.sg_ispeed = sgttyb.sg_ospeed = B9600;
        break;
    }
        /*** set parity    ***/
        /*** (also sets number of data bits, 7 if parity, 8 if not***/
    switch(digdevice.parity) {
    case 1:     /** odd parity **/
        sgttyb.sg_flags = ODDP | RAW;
        break;
    case 2:     /** even parity **/
        sgttyb.sg_flags = EVENP | RAW;
        break;
    default:     /** no  parity **/
        sgttyb.sg_flags = RAW;
        break;
    }
        /*** set stop bits ***/

        /*** do ioctl to set serial port to these characteristics */
            
        if ((ioctl (fd_ginput, TIOCSETP, &sgttyb) < 0) ||
            (ioctl (fd_ginput, TIOCSETC, &tchars) < 0) ||
            (ioctl (fd_ginput, TIOCSLTC, &ltchars) < 0) ||
            (ioctl (fd_ginput, TIOCSETD, &disc) < 0))
        {
            fprintf (stderr, "ioctl could not set parameters");
	    closef(fd_ginput);
            return(1);
        }

#endif

        default:
            return(1);
        }
    }
    return(0);
}

/*******************************************************************************
*   read_digfile                                                               *
*        This program reads an ascii digitizer file which describes a graphics *
*   input device and stores the information in a DIGDEVICE structure           *
*                                                                              *
*   Parameters:  char *path;           path of the file to be read             *
*                                                                              *
*   Values returned:   0 = files read ok                                       *
*                      1 = error                                               *
*******************************************************************************/


typedef struct {
        char p1[128];       /** variable name of a statement **/
        char p2[128];       /** assign values of the statement **/
    } TEMP;


long
read_digfile(path)
char *path;
{
    int file;                     /* file descriptor */

    TEMP tempfile[100];           /* temporary data storage */

    char    part1[128], part2[128];  /* temporary string storages */
    char line[1024];              /* readin line, max 1024 bytes */
    char token[10][5];            /* binary format strings */
    char *ptr;
    char string[256];
    char parsed[3][256];
    char temppath[300];	

    long bytes;                    /* reserve buffer */
    long  loop;                    /* { chlecking value  }*/
    long k, j, len, i, count;     /* counter variables */
    long numline;                  /* number of lines of the specified file */
    long no_token;                /* token number of a binary string */
    long val_x[10];              /* binary value of x, y, b */
    long val_y[10];              /* binary value of x, y, b */
    long val_b[10];              /* binary value of x, y, b */
    long rc;                     /* reverse counter */
    long x_length, y_length, b_length;
    long byte_no[10];
    long scount,icount;
    long opt_bytes;


/*********************** set the dig structure to default values **************/
    digdefaults();

/*********************** Open the digitizer file. *****************************/

    file = openf(path, 0);
    if(file < 0)
    {   
        sprintf(temppath,"%s.dgt",path);
        file = openf(temppath, 0);
        if(file < 0)
	{
            sprintf(string,"error, cannot open file (%s).\n",path);
            fprintf (stderr,"%s", string);
            return(1);
	}
    }
/*****  1.  read the input file, remove comments, & do some string processing */

        /*    read the dig info file 1 record at a time and        */
        /*    place in appropriate diginfo structure element     */
    bytes = 1024;
    k = 0;
    loop = 1;
    for(; reada(line, bytes, file) ;)
    {
        /*********** null out comments ************************************/
        for(ptr = line; *ptr && (*ptr != '#'); ptr++);
        *ptr = (char) 0;
        /*********** get rid of beginning & ending blanks *****************/
        trim(line);
         /*********** see if anything is left ******************************/
        if(!*line)
            continue;
        /*********** split the line accross '{' or '}' if any  ************/
        if(strsplit(line, '{',parsed[0],parsed[2]))
        {
	    parsed[1][0] = '{';
            parsed[1][1] = (char) 0;
            scount = 3;
        }
        else if(strsplit(line, '}',parsed[0],parsed[2]))
        {
	    parsed[1][0] = '}';
            parsed[1][1] = (char) 0;
            scount = 3;
        }
        else
        {
	    strcpy(parsed[0],line);
            scount = 1;
        }
        for(icount = 0; icount < scount; icount++)
        {
	    strcpy(line,parsed[icount]);
            /**** see if anything is left ****/
            trim(line);
            if(!line[0])
                continue;
        /*********** split the line accross an = sign (if any) ************/
            strsplit(line, '=', part1, part2);
        /*********** trim both parts, make sure first part is lower case **/
            lowerc(trim(part1));
            trim(part2);
        /*********** check for the beginning of a group *******************/
            if(!strcmp(part1,"setup"))        /* Checking setup format */
                loop = 0;
            if(!strcmp(part1,"startrun"))        /* Checking start format */
                loop = 0;
            if(!strcmp(part1,"startpoint"))        /* Checking start format */
                loop = 0;
            if(!strcmp(part1,"startquery"))        /* Checking start format */
                loop = 0;
            if(!strcmp(part1,"stop"))         /* Checking stop format */
                loop = 0;
            if(!strcmp(part1,"query"))        /* Checking query format */
                loop = 0;
            if(!strcmp(part1,"format"))       /* Checking format format */
                loop = 0;
         /************ check for opening braces ***************************/
            if(!strcmp(part1,"{"))   /* } <-- to go with string */
            {
		if (loop != 0)
                {
		    sprintf(string,
			  "Usage %s: missing { \n",path);
                    fprintf (stderr,"%s",string);
                    return(1);
                }
                loop = 1;
            }
        /************ check for closing braces ***************************/
                                   /* { <-- to go with string below */
            if(!strcmp(part1,"}") && loop == 0)
            {
		sprintf(string,"structure error  }\n");
                fprintf (stderr,"%s",string);
                return(1);
            }
          /*********** put both parts into tempfile array ******************/
            strncpy(tempfile[k].p1,part1,128);
            strncpy(tempfile[k].p2,part2,128);
           /*********** increment tempfile array pointer ********************/
            k++;
        }
    }
    numline = k;
        /*********** close file ******************************************/
    closef(file);

/*****  2.  ************ copy the digitizer values ****************************/
        /*********** init variables **************************************/
    digdevice.xrecipe_length = x_length = 0;
    digdevice.yrecipe_length = y_length = 0;
    digdevice.brecipe_length = b_length = 0;
    opt_bytes = 0;
        /*********** interpret file lines from tempfile array ************/
    for(i = 0; i < numline;i++)
    {
	if(!strcmp(tempfile[i].p1,"setup"))   /* read setup values */
        {
	    i++;                     /* { <-- to go with string below */
            bslash(tempfile[i].p2);
            while(strcmp(tempfile[i].p1,"}"))
            {
                if(!strcmp(tempfile[i].p1,"digname"))
                    strcpy(digdevice.digname,tempfile[i].p2);
                if(!strcmp(tempfile[i].p1,"description"))
                    strcpy(digdevice.description,tempfile[i].p2);
                if(!strcmp(tempfile[i].p1,"baud"))
                    sscanf(tempfile[i].p2,"%ld",&digdevice.baud);
                if(!strcmp(tempfile[i].p1,"parity"))
                {
		    trim(tempfile[i].p2);
                    if(!strcmp(tempfile[i].p2,"none"))
                        digdevice.parity = 0;
                    if(!strcmp(tempfile[i].p2,"odd"))
                        digdevice.parity = 1;
                    if(!strcmp(tempfile[i].p2,"even"))
                        digdevice.parity = 2;
                }
                if(!strcmp(tempfile[i].p1,"data_bits"))
                {
		    sscanf(tempfile[i].p2,"%ld",&digdevice.data_bits);
                    if((digdevice.data_bits < 5) || (digdevice.data_bits > 8))
                        digdevice.data_bits = 8;
                }
                if(!strcmp(tempfile[i].p1,"stop_bits"))
                    sscanf(tempfile[i].p2,"%ld",&digdevice.stop_bits);
                if(!strcmp(tempfile[i].p1,"digcursor"))
                    strcpy(digdevice.digcursor_fname,tempfile[i].p2);
                if(!strcmp(tempfile[i].p1,"coordinates"))
                {
		    trim(tempfile[i].p2);
                    switch(tempfile[i].p2[0]) {
                    case 'a':         /*** absolute ***/
                    case 'A':
                        digdevice.relative_coords = 0;
                        break;
                    case 'r':         /*** relative ***/
                    case 'R':
                        digdevice.relative_coords = 1;
                        break;
                    }
                }
                if(!strcmp(tempfile[i].p1,"debounce"))
                {
		    if(2 > sscanf(tempfile[i].p2,
		       "%ld %ld",&digdevice.debounce[0],&digdevice.debounce[1]))
                        digdevice.debounce[1] = digdevice.debounce[0];
                    if(digdevice.debounce[1] < 1) /** ensure repete rate >= 1 */
                        digdevice.debounce[1] = 1;
                }
                if(!strcmp(tempfile[i].p1,"units_per_inch"))
		{
                    sscanf(tempfile[i].p2,"%lf",&digdevice.units_per_inch);
		}
                if(!strcmp(tempfile[i].p1,"sign_type"))
                {    switch(tempfile[i].p2[0]) {
                    case 'n':                   /** none **/
                    case 'N':
                        digdevice.sign_type = -1;
                        break;
                    case '0':                   /** 0negative **/
                        digdevice.sign_type = 0;
                        break;
                    case '1':                   /** 1negative **/
                        digdevice.sign_type = 1;
                        break;
                    case '2':                   /** 2s-complement **/
                        digdevice.sign_type = 2;
                        break;
                    }
                }
                if(!strcmp(tempfile[i].p1,"x_positive"))
                {     trim(tempfile[i].p2);
                    switch(tempfile[i].p2[0]) {
                    case 'r':         /*** right    ***/
                    case 'R':
                        digdevice.x_positive = 0;
                        break;
                    case 'l':         /*** left     ***/
                    case 'L':
                        digdevice.x_positive = 1;
                        break;
                    }
                }
                if(!strcmp(tempfile[i].p1,"y_positive"))
                {     trim(tempfile[i].p2);
                    switch(tempfile[i].p2[0]) {
                    case 'u':         /*** up       ***/
                    case 'U':
                        digdevice.y_positive = 0;
                        break;
                    case 'd':         /*** down     ***/
                    case 'D':
                        digdevice.y_positive = 1;
                        break;
                    }
                }
		/** added the rest of this list for v.digit---9/92 *********/
                if(!strcmp(tempfile[i].p1,"button_up_char"))
                {     
		    trim(tempfile[i].p2);
		    if (tempfile[i].p2[0] == 92)
		    {
		        digdevice.button_up_char = bslash (tempfile[i].p2);
        		sscanf(tempfile[i].p2,"%c",&digdevice.button_up_char);
		    }
		    else 
			digdevice.button_up_char =tempfile[i].p2[0]; 
		}
                if(!strcmp(tempfile[i].p1,"buttons"))
                {
		    trim(tempfile[i].p2);
		    digdevice.buttons = atoi(tempfile[i].p2);
		}
                if(!strcmp(tempfile[i].p1,"buttonstart"))
                {
		    trim(tempfile[i].p2);
		    digdevice.buttonstart = atoi(tempfile[i].p2);
		}
                if(!strcmp(tempfile[i].p1,"buttonoffset"))
                {
		    trim(tempfile[i].p2);
		    digdevice.buttonoffset= atoi(tempfile[i].p2);
		}
                if(!strcmp(tempfile[i].p1,"footswitch"))
                {
		    trim(tempfile[i].p2);
		    digdevice.footswitch = atoi(tempfile[i].p2);
		}
                if(!strcmp(tempfile[i].p1,"digname"))
                    strcpy(digdevice.digname,tempfile[i].p2);
                if(!strcmp(tempfile[i].p1,"description"))
                    strcpy(digdevice.description,tempfile[i].p2);
                i++;
            }
        }
        if(!strcmp(tempfile[i].p1,"startrun"))   /* read start values */
        {
	    i++;                     /* { <-- to go with string below */
            for(k=0;strcmp(tempfile[i].p1,"}");i++)
                k +=
		read_value(tempfile[i].p1,tempfile[i].p2,&digdevice.startrun[k]);
            digdevice.no_startrun_actions = k;
        }
        if(!strcmp(tempfile[i].p1,"startpoint"))   /* read start values */
        {
	    i++;                     /* { <-- to go with string below */
            for(k=0;strcmp(tempfile[i].p1,"}");i++)
                k +=
		read_value(tempfile[i].p1,tempfile[i].p2,&digdevice.startpoint[k]);
            digdevice.no_startpoint_actions = k;
        }
        if(!strcmp(tempfile[i].p1,"startquery"))   /* read start values */
        {
	    i++;                     /* { <-- to go with string below */
            for(k=0;strcmp(tempfile[i].p1,"}");i++)
                k +=
		read_value(tempfile[i].p1,tempfile[i].p2,&digdevice.startquery[k]);
            digdevice.no_startquery_actions = k;
        }
        if(!strcmp(tempfile[i].p1,"query"))   /* read query values */
        {
	    i++;                     /* { <-- to go with string below */
            for(k=0;strcmp(tempfile[i].p1,"}");i++)
                k +=
		read_value(tempfile[i].p1,tempfile[i].p2,&digdevice.query[k]);
            digdevice.no_query_actions = k;
        }
        if(!strcmp(tempfile[i].p1,"stop"))   /* read stop values */
        {
	    i++;                     /* { <-- to go with string below */
            for(k=0;strcmp(tempfile[i].p1,"}");i++)
                k +=
		read_value(tempfile[i].p1,tempfile[i].p2,&digdevice.stop[k]);
            digdevice.no_stop_actions = k;
        }
        if(!strcmp(tempfile[i].p1,"format"))   /* read format values */
        {
	    i++;
            k = 0;
            digdevice.file_type = -1;
            bslash(tempfile[i].p2);       /* { <-- to go with string below */
            while(strcmp(tempfile[i].p1,"}"))
            {
                 if(!strcmp(tempfile[i].p1,"ascii"))  /* get ascii format */
                {     digdevice.x_byte1 = digdevice.x_byten = -1;
                    digdevice.y_byte1 = digdevice.y_byten = -1;
                    digdevice.push_byte = -1;
                    rc = 0;
                    digdevice.file_type = 1;
                    trim(tempfile[i].p2);
                    digdevice.packet_length[0] = strlen(tempfile[i].p2);
                    for (j = 0,count = 0 ; j <= digdevice.packet_length[0];
								    ++j,++count)
                    {
			switch (tempfile[i].p2[j]) {
                        case 'x':                  /* a digit of the x coord. */
                            if( digdevice.x_byte1 == -1)
                            {    digdevice.x_byte1 = count;
                                digdevice.x_byten = count;
                            } else
                                digdevice.x_byten = count;
                            break;
                        case 'y':                  /* a digit of the y coord */
                            if( digdevice.y_byte1 == -1)
                            {    digdevice.y_byte1 = count;
                                digdevice.y_byten = count;
                            }else
                            digdevice.y_byten = count;
                            break;
                        case 'b':                  /* button binary info. */
                            digdevice.b_byte = count;
                            break;
                        case 'p':                  /* button push info. */
                            digdevice.push_byte = count;
                            break;
                        case ',':                 /* the delineator char */
                            digdevice.comma_bytes[rc] = count;
                            rc++;
                            break;
                        case 'c':                   /* carriage return */
                            break;
                        case 'l':                   /* line feed  */
                            break;
                        default:
                            break;
                        }
                    }
                    digdevice.comma_cnt = rc;
                }
                else if(!strncmp(tempfile[i].p1,"byte",4)) /* binary format */
                {
		    digdevice.file_type = 0;
                        /*** check for optional byte ***/
                    if(strchr(tempfile[i].p1,'o') != (char *) 0)
                        opt_bytes++;

                        /**** decode bit specifiers ****/
		                     /* read bi_value */
                    sscanf(tempfile[i].p1,"byte%ld",&byte_no[k]);
                    no_token = dparse(tempfile[i].p2,"     ",(char *)token,128,3,5,9);
                    if( no_token != 8)
                    {     
			sprintf(string," Usage: %s format error..\n", path);
                	fprintf (stderr,"%s",string);
                        return(1);
                    }
                    else
                    {     for(j = 0,rc = no_token-1; j < no_token; rc--,j++)
                        {     val_x[rc] = -1;
                            val_y[rc] = -1;
                            val_b[rc] = -1;
                            switch (token[j][0]) {
                            case 'x':
                                sscanf(&token[j][1],"%ld",&val_x[rc]);
                                break;
                            case 'y':
                                sscanf(&token[j][1],"%ld",&val_y[rc]);
                                break;
                            case 'b':
                                sscanf(&token[j][1],"%ld",&val_b[rc]);
                                break;
                            case '1':
                                digdevice.sync_mask &= 1 << rc;
                                digdevice.sync_bytes |= 1 << k;
                                break;
                            case '0':
/** start here, digdevice.sync_offbits **/
                                digdevice.sync_offbits |= 1 << rc;
                                digdevice.sync_bytes |= 1 << k;
/** start here, testing digdevice.sync_bytes **/
                                break;
/* added 9/92 check for button press bit */
			    case 'p':
                                digdevice.push_mask |= 1 << rc;
                                digdevice.push_byte |= 1 << k;
                                break;
                            default:
                                break;
                            }
                        }

                        for(j = 0; j < no_token ; j++)
                        {     if(val_x[j] != -1)
                            {     digdevice.xrecipe[x_length].b_no = byte_no[k] - 1;
                                digdevice.xrecipe[x_length].shift = val_x[j] - j;
                                digdevice.xrecipe[x_length].mask = 1 << j;
                                while (val_x[j] == (val_x[j + 1]-1))
                                {     j++;
                                    digdevice.xrecipe[x_length].mask |= 1 << j;
                                }
                                x_length++;
                                digdevice.xrecipe_length = x_length;
                            }
                            if(val_y[j] != -1)
                            {     digdevice.yrecipe[y_length].b_no = byte_no[k] - 1;
                                digdevice.yrecipe[y_length].shift = val_y[j] - j;
                                digdevice.yrecipe[y_length].mask = 1 << j;
                                while (val_y[j] == (val_y[j + 1]-1))
                                {     j++;
                                    digdevice.yrecipe[y_length].mask |= 1 << j;
                                }
                                y_length++;
                                digdevice.yrecipe_length = y_length;
                            }
                            if(val_b[j] != -1)
                            {     digdevice.brecipe[b_length].b_no = byte_no[k] - 1;
                                digdevice.brecipe[b_length].shift = val_b[j] - j;
                                digdevice.brecipe[b_length].mask = 1 << j;
                                while (val_b[j] == (val_b[j + 1]-1))
                                {     j++;
                                    digdevice.brecipe[b_length].mask |= 1 << j;
                                }
                                b_length++;
                                digdevice.brecipe_length = b_length;
                            }
                        }
                    }
                    k++;
                }
                i++;
            }
            if (digdevice.file_type == -1)
            {
                sprintf(string,"Usage %s: is not a ascii or binary.\n", path);
                fprintf (stderr,"%s",string);
                return(1);
            }
            if(digdevice.file_type == 0)
            {    digdevice.packet_length[1] = k;     /* max length of packet */
                digdevice.packet_length[0] = k - opt_bytes; /* min lnth of packet */
            }
        }
    }

    set_diginternals();

    return(0);
}

/*******************************************************************************
*  read_value                                                                  *
*     This program sets the action, value, and string parts of one ACTION      *
*  structure.                                                                  *
*                                                                              *
*  Parameters:  char p1[];     string containing 1st half of digfile string    *
*               char p2[];     string containing 2nd half of digfile string    *
*               ACTION *act;   pointer to ACTION structure to be filled        *
*                                                                              *
*  Values returned:  1 = ACTION structure has been filled                      *
*                    0 = ACTION structure NOT filled                           *
*******************************************************************************/

long
read_value(p1,p2,act)
char p1[];
char p2[];
ACTION *act;
{
    long j;

    double number;


    if(!strcmp(p1,"send"))
    {
        act->action = 0;
        act->value = bslash(p2);
        for(j=0; j<act->value; j++)
            act->string[j] = p2[j];
        act->string[j] = (char) 0;
    }
    else if(!strcmp(p1,"wait"))
    {
        bslash(p2);
        act->action = 2;
        sscanf(p2,"%lf",&number);
        act->value = number * 1000 + 0.5;
    }
    else if(!strcmp(p1,"baud"))
    {
        bslash(p2);
        act->action = 3;
        sscanf(p2,"%ld",&act->value);
    }
    else if(!strcmp(p1,"read"))
    {
        bslash(p2);
        act->action = 1;
        sscanf(p2,"%ld",&act->value);
    }
    else
        return(0);

    return(1);
}

/*******************************************************************************
*   digdefaults()                                                              *
*      This program sets all values in one DIGDEVICE structure to the default  *
*   values.                                                                    *
*                                                                              *
*   Parameters:   none                                                         *
*                                                                              *
*   Value returned:  0                                                         *
*******************************************************************************/

long digdefaults()
{
        /******* source filename ************/
    digdevice.digcursor_fname[0] = (char) 0;
        /******* setup information **********/
    digdevice.baud = 9600;
    digdevice.parity = 0;       /** none **/
    digdevice.data_bits = 8;    /** not including parity **/
    digdevice.stop_bits = 1;
        /******* user preference information */
    digdevice.debounce[0] = 0;               /** delay, 0 = infinite debounce **/
    digdevice.debounce[1] = 0;               /** repete rate default   **/
    digdevice.relative_coords = 0;        /** 0 = absolute coords **/
    digdevice.units_per_inch = 0.0;
    digdevice.sign_type = 3;              /** no sign type has been assigned **/
    digdevice.x_positive = 0;             /** left is default **/
    digdevice.y_positive = 0;             /** up is default **/
        /******* bin/ascii flag **************/
    digdevice.file_type = 1;           /** 0=binary, 1=ascii      **/
        /******* binary and ascii decoding info ***/
    digdevice.packet_length[0] = 0;
    digdevice.packet_length[1] = 0;
        /******* binary decoding info ********/
    digdevice.sync_mask = (char) -1;
/*** start here test variable ***/
    digdevice.sync_bytes = 0;
    digdevice.sync_offbits = 0;
/** end of test stuff, start here ***/
    digdevice.xrecipe_length = 0;    /** binary x decoding info **/
    digdevice.yrecipe_length = 0;    /** binary y decoding info **/
    digdevice.brecipe_length = 0;    /** binary button decoding **/
    digdevice.xsign_mask = 0;
    digdevice.ysign_mask = 0;
        /******* ascii decoding info  ********/
    digdevice.comma_cnt = 0;
    digdevice.x_byte1 = 0;
    digdevice.x_byten = 0;                   /** ascii x decoding info  **/
    digdevice.y_byte1 = 0;
    digdevice.y_byten = 0;                   /** ascii y decoding info  **/
    digdevice.b_byte = 0;                    /** ascii button decoding  **/
        /******* start, stop, & uery device information **/
    digdevice.no_startrun_actions = 0;
    digdevice.no_startpoint_actions = 0;
    digdevice.no_startquery_actions = 0;
    digdevice.no_query_actions = 0;
    digdevice.no_stop_actions = 0;
       /***** added for v.digit ********/
    digdevice.buttons = 0;
    digdevice.buttonstart = 0;
    digdevice.buttonoffset = 0;
    digdevice.footswitch = 0;
    digdevice.digname[0] = (char) 0;
    digdevice.description[0] = (char) 0;
    digdevice.push_byte = 0;
    digdevice.push_mask = 0;

        /*********** return ************************************************/
    return(0);
}

/*******************************************************************************
*  set_diginternals                                                            *
*      This program sets digdevice variables which are secondary to the ones   *
*  which may or may not have been set by the digitizer file.                   *
*                                                                              *
*  Parameters    none                                                          *
*  Values returned:    0                                                       *
*******************************************************************************/

long set_diginternals()
{

    long i;
    char cpath[256];
    char string[256];


        /**** set default sign, if not specified ****/
    if(!digdevice.file_type)           /**** binary file type ****/
    {    if(digdevice.sign_type > 2)    /** sign type not explicitly set **/
        {    if(digdevice.relative_coords == 1)
                digdevice.sign_type = 2;       /**** 2s complement sign type **/
            else
                digdevice.sign_type = -1;      /**** no sign, all coords positive */
        }
            /******** build x & y masks for sign bit & other bits ************/
        if(digdevice.sign_type >= 0)
        {
	/** build a mask of all x bits used (31-0), including sign bit **/
            digdevice.xsign_mask = 0;
            for(i=0; i< digdevice.xrecipe_length; i++)
            {    if(digdevice.xrecipe[i].shift >= 0)
                    digdevice.xsign_mask |= (digdevice.xrecipe[i].mask) << digdevice.xrecipe[i].shift;
                else
                    digdevice.xsign_mask |= (digdevice.xrecipe[i].mask) >> -(digdevice.xrecipe[i].shift);
            }
                /** reverse it & add 1 lower bit **/
            digdevice.xsign_mask = ~(digdevice.xsign_mask >> 1);

        /** build a mask of all y bits used (31-0), including sign bit **/
            digdevice.ysign_mask = 0;
            for(i=0; i< digdevice.yrecipe_length; i++)
            {    if(digdevice.yrecipe[i].shift >= 0)
                    digdevice.ysign_mask |= (digdevice.yrecipe[i].mask) << digdevice.yrecipe[i].shift;
                else
                    digdevice.ysign_mask |= (digdevice.yrecipe[i].mask) >> -(digdevice.yrecipe[i].shift);
            }
                /** reverse it & add 1 lower bit **/
            digdevice.ysign_mask = ~(digdevice.ysign_mask >> 1);
        }
    }
    else                        /*** ascii file type **/
    {    digdevice.sign_type = 1;      /*** 1negative sign type (negation) **/
        digdevice.packet_length[1] = digdevice.packet_length[0];
    }


        /********* set units_per_inch ************************************/
    if(digdevice.units_per_inch == 0.0)
    {    if(digdevice.relative_coords == 0)    /** absolute coords **/
            digdevice.units_per_inch = 1000;
        else
            digdevice.units_per_inch = 200;
    }

        /********* return ************************************************/
    return(0);
}

/*******************************************************************************
*   ginput_setup   (see manual, Internal Functions)             <in>           *
*           Closes existing graphics input device (if any),                    *
*           gets new ginfo structure information (if new device is digitizer), *
*           opens and starts new device.                                       *
*                                                                              *
*           Globals Modified:  fd_ginput,         diginfo.                     *
*                                                                              *
*           Returns 0 for success, 1 for failure.                              *
*                                                                              *
*           J.Dabritz, 6/19/89, including code by C.Jonson 5/89.               *
*******************************************************************************/


long
ginput_setup( new_dev, ttypath, mode )
char *new_dev;
char *ttypath;
int  mode;
{
    char path[300];

        /******************* close out old device ********************/
    if(fd_ginput >= 0)
    {
        stop_ginput();
    }

        /******************* start new device ************************/
            /********* get path of diginfo file ********/
        sprintf(path,"./digitizers/%s", new_dev);

            /********* read diginfo file ***************/
    if(read_digfile(path))
    {
        return(1);
    }


            /********* initialize device ***************/
    if(start_ginput(ttypath, mode))
    {
         return(1);
    }

        /******************* return **********************************/
    return(0);
}



/*******************************************************************************
*   dig_input                                                                  *
*        checks for digitizer input.  If available, the raw input is cooked    *
*   into x & y screen & tile coordinates, the button is debounced, and the     *
*   cursor is displayed.                                                       *
*                                                                              *
*   Parameters:   none                                                         *
*                                                                              *
*   Values Returned:    -1 = error  (no input read)                            *
*                        0 =  no button press                                  *
*                        n = button press                                      *
*                                                                              *
*   Globals modified:  ginfo                                                   *
*                                                                              *
*   JPD                                                                        *
*******************************************************************************/

int
dig_input(x, y)
     int *x, *y;
{    /*  Automatics  */

    static int last_button;          /*  for button debouncing          */
    static long button_count;         /*  for button debouncing          */
    static long button_press;
    static long i;                    /*  counter                        */
    static long rtn;
    static int first_time = 1;
    char *getenv();

    if (first_time)
    {
	char *p;

	first_time = 0;
	if (NULL != (p = getenv ("DEB")))
	{
	    if (!(Debug = atoi (p)))
		Debug = 1;
	}
    }


        /************* init return value ************************************/
    rtn = -1;
    /************* get raw digitizer input from graphics input device ***/
    if(!raw_diginput())
    {
        return(rtn);
    }

    rtn = 0;    /*** there is dig input ***/


        /***************** debounce button ****************************/
    button_press = 0;           /** default **/
    if(digdevice.debounce[0])   /** finite debounced **/
    {    if(ginfo.button != last_button)     /** first report with button **/
        {
	    last_button = ginfo.button;
            button_press = 1;
            button_count = 0;
        }
        else                 /** not first rep. w button  **/
        {    if(button_count < digdevice.debounce[0])  /** in delay range **/
                ginfo.button = 0;
            else             /** in repete rate range **/
            {    if((button_count - digdevice.debounce[0]) % digdevice.debounce[1])
                    ginfo.button = 0;
                else
                {    button_count = digdevice.debounce[0];
                    button_press = 1;
                }
            }
        }
        button_count++;
    }
    else       /*** infinite debounce ***/
    {    if(ginfo.button == last_button)
            ginfo.button = 0;
        else
        {
	    last_button = ginfo.button;
            button_press = 1;
        }
    }

        /****************** return flag dependent on string input **/
    *x = (int)ginfo.rawcoord[0];
    *y = (int)ginfo.rawcoord[1];
    return(ginfo.button);
}
/*******************************************************************************
*       bshash          (see manual, Internal Functions)                <in>   *
*                       This program reads the input string and converts       *
*                       backslash sequences as follows:                        *
*                         ( n represents a digit 0 through 9 )                 *
*                       \\    converts to \                                    *
*                       \nnn  converts to ascii decimal char  nnn (unsigned)   *
*                       \nn   converts to ascii decimal char  nn (unsigned)    *
*                       \n    converts to ascii decimal char  n (unsigned)     *
*                                                                              *
*                       No globals or files changed.                           *
*                                                                              *
*                       Returns the length of the processed string             *
*                      j.dabritz  6/22/89                                      *
*******************************************************************************/

long
bslash(string)
char *string;
{
    char *ptr, *ptr2;
    long n, digits, i;

        /*  convert \n & \nn & \nnn & \\ groups in string  */
    for(ptr=string, i=0; *ptr; ptr++, i++)
    {    if(*ptr == 92)
        {        /*  convert '\\' groups to '\'   */
            if(*(ptr+1) == 92)     /*   '\\' found, reduce to '\'  */
            {    for(ptr2=ptr+1; *(ptr2) ; ptr2++)
                    *ptr2 = *(ptr2 + 1);
            }
                /*  convert '\n' & '\nn' & '\nnn' to decimal ascii */
            else if((*(ptr+1) >= '0')  &&  (*(ptr+1) <= '9'))
            {    n = (*(ptr+1)) - '0';
                digits = 1;
                if((*(ptr+2) >= '0')  &&  (*(ptr+2) <= '9'))
                {    n = 10 * n + (*(ptr+2)) - '0';
                    digits = 2;
                    if((*(ptr+3) >= '0')  &&  (*(ptr+3) <= '9'))
                    {    n = 10 * n + (*(ptr+3)) - '0';
                        digits = 3;
                    }
                }
                *ptr = (n % 256);
                for(ptr2=ptr+1; *(ptr2 + digits) ; ptr2++)
                    *ptr2 = *(ptr2 + digits);
                *ptr2 = (char) 0;
            }
        }
    }

    return(i);

}
/*******************************************************************************
*      dparse           (see manual, Internal Functions)                <in>   *
*     The parse program receives a string of raw input, breaks it up into      *
*     separate pieces based on specified delimeters. If there are more pieces  *
*     than the maximum specified, then the remainder of the string is placed   *
*     in the final piece, regardless of delimeters.                            *
*                                                                              *
*     example:  char raw[80] = "   how are  you today?   ";                    *
*               char delims[32] = " ";                                         *
*               char parsed[3][10];                                            *
*                                                                              *
*               dparse(raw,delims,(char *) parsed, 80, 32, 10, 3);             *
*        parse returns 3, and loads the parsed array as follows:               *
*              parsed[0] is "how"                                              *
*              parsed[1] is "are"                                              *
*              parsed[2] is "you today?   "                                    *
*                                                                              *
*     Parameters: char raw[];        raw input string                          *
*                 char delims;       all delimeter characters allowed          *
*                 char *parsed;      pointer to beginning of an ary of strings *
*                 long max_raw;      maximum length allowed for raw string     *
*                 long max_delims;   maximum length of string containing delims*
*                 long max_parsed;   maximum length of individual parsed elems.*
*                 long max_p_elems;  maximum number of parsed elems allowed.   *
*                                                                              *
*     Values returned:                                                         *
*                 long n:  number of parsed elements filled (0 or greater)     *
*                          returns negative number on error.                   *
*                                                                              *
*                      j.dabritz/tru phan  July 24,1991                        *
*******************************************************************************/

long
dparse(raw, delims, parsed, max_raw, max_delims, max_parsed, max_p_elems)
    char raw[];
    char delims[];
    char *parsed;
    long max_raw, max_delims, max_parsed;
    long max_p_elems;
{
    char *rptr, *rend;     /** ptrs for raw string **/
    char *dptr, *dend;     /** ptrs for delimeter string **/
    char *pptr, *pend;     /** ptrs for parsed elements **/

    long group_counter;
    long begin_group;


        /***** check input params for legitimacy **/
    if(raw == (char *) 0)
        return(-1);
    if(delims == (char *) 0)
        return(-2);
    if(parsed == (char *) 0)
        return(-3);
    if(max_raw < 1)
        return(-4);
    if(max_delims < 1)
        return(-5);
    if(max_parsed < 1)
        return(-6);
    if(max_p_elems < 1)
        return(-7);


        /** init flags, counters, & pointers **/
    begin_group = 1;
    group_counter = 0;
    rend = raw + max_raw;
    pend = parsed;
    dend = delims + max_delims;

        /** read raw string 1 character at a time & parse as we go **/
    for(rptr=raw ; *rptr && (rptr < rend); rptr++)
    {        /**** see if character is a delimeter ****/
        for(dptr=delims; *dptr && (*dptr!=*rptr) && (dptr<dend); dptr++);

        if(!*dptr  ||  (dptr == dend))     /** NOT a delimeter character **/
        {    if(begin_group)
            {    begin_group = 0;
                pptr = pend;
                pend = pptr + max_parsed;
                group_counter++;
            }
            *pptr = *rptr;          /** copy character into parsed group **/
            pptr++;
            if(pptr == pend)
                pptr--;
        }
        else            /** a delimiter character  ***/
        {    if(group_counter == max_p_elems)
            {    *pptr = *rptr;    /** copy character into parsed group **/
                pptr++;
                if(pptr == pend)
                    pptr--;
            }
            else
            {    if(!begin_group)
                {    *pptr = (char) 0;
                    begin_group = 1;
                }
            }
        }
    }

        /** terminate last parsed group if necessary **/
    if(group_counter && !begin_group)
        *pptr = (char) 0;

        /** return number of groups parsed **/
    return(group_counter);
}

/*DEBUG*/
print_binary_int (val)
    int val;
{
    int i;

    for (i = 8 * sizeof (val) - 1 ; i >= 0 ; i--)
	fprintf (stderr, "%1d ", (int) (((val) >> i) & 0x01));
    fprintf (stderr,  "\n");
}

print_binary_char (val)
    unsigned char val;
{
    int i;

    for (i = 8 * sizeof (val) - 1 ; i >= 0 ; i--)
	fprintf (stderr, "%1d ", (int) (((val) >> i) & 0x01));
    fprintf (stderr,  "\n");
}
int 
change_mode (mode)
    int mode;
{
    int rtn;

    if (mode == RUN_MODE)
        rtn = start_run_mode();
    else if (mode == POINT_MODE)
        rtn = start_point_mode();
    else if(mode == QUERY_MODE)
	rtn = start_query_mode();
    else 
    {
	fprintf (stderr, "Unknown mode.\n");
	rtn = 1;
    }
}
