#include <tools.h>
#include <stdio.h>

#define MYEVENTS (REFRESH_EVENT | BUTTON_EVENT | KEYBOARD_EVENT | DELETE_EVENT)

int	vsno;		/* Number of active virtual screen */
int	wno;		/* Window number */

main()
{

	
	int	curevents;	/* Mask of outstanding events */
	char	keybuf;		/* Buffer for keyboard data */
	int	keycnt;		/* Number of characters received */
	int	tmp;		/* Dummy variable... stuff I don't care about */

	/* Perform initialization and activate process */
	Enter_tools();

	/* Set process logo */
	Set_logo ("XX");
	
	/* Determine the number of the active virtual screen */
	Inq_displayed_vs (&vsno);
	
	Create_win (vsno, "TEST DRIVER X", 0, 0, MAX_SCREEN_WIDTH, MAX_SCREEN_HEIGHT, &wno);
	
	/* Disable manual refresh */
	Set_win_sys_icon (wno, REFRESH_ICON_VALUE, 0);
	
	/* Display the window */
	Display_win (wno);
	
	store_vlt_info (vsno);

	/* Exit_tools will clean up windows */
	Exit_tools();
	exit(0);

}
			
store_vlt_info(vsno) 
	int	vsno ;
{

int		i ;
double		num_colors ;
double		num_planes ;

	struct  scr_info  info[MAX_SCREENS] ;
	struct vlt_slot vlt[40] ;

	FILE  *fp, *fopen() ;

	double  pow() ;

	if ( (fp = fopen( "vlt.out", "w") ) == NULL)
	{
		fprintf(stderr, "Can't open file for write\n") ;
		Exit_tools() ;
		exit(-1) ;
	}

	fprintf( fp, " wno:  %d\n", wno) ;

	Inq_screen_info (info) ;

	fprintf( fp, " vsi_screen_num:  %d\n",
		info[vsno].vsi_screen_num) ;

	fprintf( fp, " vsi_num_planes:  %d\n",
		info[vsno].vsi_num_planes) ;

	fprintf( fp, " vsi_plane_mask:  %lu, hex %x\n",
		info[vsno].vsi_plane_mask, info[vsno].vsi_plane_mask) ;

	fprintf( fp, " vsi_vlt_size:  %d\n",
		info[vsno].vsi_vlt_size) ;

	fprintf( fp, " vsi_DAC_size:  %d\n",
		info[vsno].vsi_DAC_size) ;

	fprintf( fp, " vsi_fixed_vlt_start:  %d\n",
		info[vsno].vsi_fixed_vlt_start) ;

	fprintf( fp, " vsi_fixed_vlt_size:  %d\n",
		info[vsno].vsi_fixed_vlt_size) ;

	fprintf( fp, " vsi_x:  %d\n",
		info[vsno].vsi_x) ;

	fprintf( fp, " vsi_y:  %d\n",
		info[vsno].vsi_y) ;

		num_planes = info[vsno].vsi_num_planes ;
		num_colors = pow((double)2, num_planes) ;

	fprintf( fp, " num_colors:  %lf\n",
		num_colors) ;


		Readvlt( vsno, vlt, 40, 0) ;

		fprintf( fp, "\n") ;
		for ( i=0; i<39; i++)
		{
		fprintf( fp, " i: %d,  v.slot: %u, r: %u, g: %u, b: %u\n",
			i,
			vlt[i].v_slot,
			vlt[i].v_red,
			vlt[i].v_green,
			vlt[i].v_blue) ;
		}


	fclose(fp) ;
	return(0) ;

} 
  
