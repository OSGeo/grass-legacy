
/*
 *  Written by the GRASS Team in the Spring of 90, -mh.
*/

#include	<stdio.h>
#include	<tools.h>
#include	"gis.h"
#include	"env.h"
#include	"igraphics.h"

#define	EXIT_NUM	6

static int m_red[] = { 5, 0, 1, 6} ;
static int m_green[] = { 5, 0, 8, 0} ;
static int m_blue[] = { 5, 0, 14, 1} ;


/*
* Main pull down menu 
*/ 

static int entry_mask_1[] = { EN_TITLE|EN_TEXT, EN_SUB|EN_TEXT, EN_SUB|EN_TEXT,
	EN_TITLE|EN_TEXT, EN_SUB|EN_TEXT, EN_SUB|EN_TEXT, EN_SUB|EN_TEXT,
	EN_TEXT } ;

static char *entry_1[] = {"GRASS Commands", "start/select", "stop",
			"GRASS Graphics",
			"resize/move", "virtual-screens",
			"colors", "EXIT" } ;

static int command_1[] = { 0, 1, 2, 0, 3, 4, 5, 6 } ;
static int link_1[] =    { 0, 0, 0, 0, 0, 0, 0, 0 } ;


/*
* Sub-menu 40's: Virtual Screens
*/ 
static int entry_mask_40[] = {  EN_TEXT, EN_TEXT, EN_TEXT } ;

static char *entry_40[] = {"0", "1", "2", "3" } ;

static int command_40[] = { 41, 42, 43, 44 } ;
static int link_40[] =    { 0, 0, 0, 0 } ;


/*
* Sub-menu 50's: Number of Colors
*/ 

#define  Colors_16_index	0
#define  Colors_32_index	1
#define  Colors_128_index	2
#define  Colors_256_index	3
#define  Colors_512_index	4

#define  NUM_OF_COLOR_INDEX	5

static int entry_mask_50[] = {  EN_TEXT, EN_TEXT, EN_TEXT, EN_TEXT, EN_TEXT } ;

static char *entry_50[] = {"16", "32", "128", "256", "512" } ;
/*  Integer version of entry_50[]  */
static int int_entry_50[] = {16, 32, 128, 256, 512 } ;

static int command_50[] = { 51, 52, 53, 54, 55 } ;
static int link_50[] =    { 0,  0,  0,  0,  0 } ;


pull_down_menu (desc)
	struct screen_description *desc ;
{

	int  i ;
	int  menu_id_1, menu_id_40, menu_id_50 ;
	int  pd_command ;
	char  buf[80] ;

	Set_logo("Grass Graphics") ;

	Create_pd( m_red, m_green, m_blue, TITLE_FONT, TEXT_FONT,
		 0, 8, 0, entry_mask_1, entry_1, command_1,
		 link_1, &menu_id_1) ;

/*
*  Set up virtual screen sub-menu
*/
	Create_quick_pd( m_red, m_green, m_blue, TEXT_FONT,
		 4, 0, entry_40, command_40, link_40, &menu_id_40) ;

	Link_pde_name(menu_id_1, "virtual-screens", menu_id_40) ;

/*
*  Turn off any virtual screen numbers in the pull down menu
*  that don't apply to this specific workstation configuration.
*  Range is 0 to 3.
*/

	for ( i = MIN_VS_NO;  i<= MAX_VS_NO; ++i)
	{
		if ( i == desc->primary_vs_no 
		||  i == (desc->primary_vs_no+1)  )
			 continue ;

		Deactivate_pde_index( menu_id_40, i ) ;
	}

/*
*  Set up colors sub-menu
*/
	Create_quick_pd( m_red, m_green, m_blue, TEXT_FONT,
		 NUM_OF_COLOR_INDEX, 0, entry_50, command_50,
		 link_50, &menu_id_50) ;

	Link_pde_name(menu_id_1, "colors", menu_id_50) ;

/*
*  Turn off any num of colors in the pull down menu
*  that are too large for this specific workstation configuration.
*/

	for ( i = 0;  i <= NUM_OF_COLOR_INDEX; ++i)
	{
		if ( int_entry_50[i] > desc->max_num_colors )
			Deactivate_pde_index( menu_id_50, i ) ;
	}

/*
*  Start the main processing loop.
*/

pd_command = 0 ;

while (pd_command != EXIT_NUM)
{

Process_pd( menu_id_1, 0, 290, 390, &pd_command) ;

switch(pd_command)
{
	case -1:
	case 0: keybd_bell(0) ;
		 break ;

	case 1:
		Close_curses() ;

		fprintf( stderr, "--------------------------------------\n\n\n") ;
		fprintf( stderr, "Starting GRASS Graphics driver: %s\n" ,
			desc->driver_name) ;

		if (desc->vs_no != desc->current_vs_no)
			fprintf( stderr, "\nGraphics driver is starting on the other virtual screen (%d).\n", desc->vs_no) ;
		else
			fprintf( stderr, "On virtual screen number: %d\n",
				desc->vs_no) ;

		sprintf( buf, "d.mon sta=%s -s", desc->driver_name) ;
		system( buf) ;
/*
*  This sleep is needed.  If you select the monitor to rapidly,
*  this error message will appear:  $LOCATION_NAME not set.
*/

		sleep(2) ;
		sprintf( buf, "d.mon sel=%s", desc->driver_name) ;
		system( buf) ;
		fprintf( stderr, "\n\n--------------------------------------\n\n\n\n") ;
		/*
		* Exit, because graphics window is probably over the top of us
		*/
		pd_command = EXIT_NUM ;
		break ;

	case 2:
		sprintf( buf, "d.mon sto=%s > /dev/null 2>&1 ",
			desc->driver_name) ;
		system( buf) ;
		break ;

	case 3:

		/*
		* Exit, from this function.
		* Go back to main and setup window for resize/move.
		*/
		modify_window(desc) ;
		break ;

	case 4:  
		/*
		*  Link to the virtual screen menu.
		*/
		break ;

	case 5:  
		/*
		*  Link to the color menu.
		*/
		break ;


	case 6:
		/*
		*  Exit number to leave this menu.
		*/
		break ;

    /*  Virtual screen sub menu  */

	case 41:
		desc->vs_no = 0 ;
		store_vs ( desc );
		break ;

	case 42:
		desc->vs_no = 1 ;
		store_vs ( desc );
		break ;

	case 43:
		desc->vs_no = 2 ;
		store_vs ( desc );
		break ;

	case 44:
		desc->vs_no = 3 ;
		store_vs ( desc );
		break ;

    /*  Number of Colors sub menu  */

	case 51:
		desc->num_colors = 16 ;
		store_color( desc );
		break ;

	case 52:
		desc->num_colors = 32 ;
		store_color( desc );
		break ;

	case 53:
		desc->num_colors = 128 ;
		store_color( desc );
		break ;

	case 54:
		desc->num_colors = 256 ;
		store_color( desc );
		break ;

	case 55:
		desc->num_colors = 512 ;
		store_color( desc );
		break ;

	/*
	*  Huh, this couldn't/shouldn't happen.
	*/
	default:
		break ;

}

if (pd_command != EXIT_NUM)
	print_current_parameters( desc) ;

}

	return( 0) ;

}   /*  end of pull_down_menu()  */


