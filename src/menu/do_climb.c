/* %W% %G% */
/* :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: */

#include "do_climb.h"
#include "menu.h"
main() 
{
	int	choice[1] ;
	int	menu_state ;
	int new_dir = 1 ;
	int dir_dpth = 0 ;
	char	response[256] ;
	char *getenv() ;
	short verbose = 1 ;
	int Length ;  
	int Prob ;
	char *Tablbuf ;
	char *G_gisbase() ;


/*** Check Desire for Verbosity */
/*	if(! strcmp(getenv("VERBOSE"),"on")) verbose = 1 ;  */

/*** Initialize the menu   */
	P_menuinit();


	menu_state = MINIRESP ;

/*** Move to base menu directory */
	sprintf(response,"%s/menu", G_gisbase()) ;
	chdir(response) ;

/*** Main loop */
	while(1)
	{
		new_dir = 0 ;

	/*** Get user response */
		if(verbose) {
			Prob = F_fetchfile (VERBOSE_FILE, &Tablbuf, &Length);
			P_writowin (TablW, Tablbuf, 1, Length, HELPWINHITE - 2);
			if (Prob < 0)
				P_menuerror (Prob, "dumtable");
		}
		F_menu(INDEX_FILE, HELP_FILE, choice, response, &menu_state);	

	/*** If they want to exit, then good_bye */
		if ( ! strcmp("exit", response) ) do_leave() ;

	/*** Option 1:  backup one directory */
		if ( ! strcmp("back", response) ) 
		{
			if (dir_dpth != 0)
			{
				chdir("..") ;
				new_dir = 1 ;
				dir_dpth-- ;
			}
		}

	/*** Option 2:  Go to the base directory (top of the tree) */
		else if ( ! strcmp("top", response) )  {
			sprintf(response,"%s/menu", G_gisbase()) ;
			chdir(response) ;
			dir_dpth = 0 ;
			new_dir = 1 ;
		}

	/*** Option 3:  Assume a menu command */
		else {

		/*** Option 3.1 If command is menu, move into new menu (directory) */
			if ( chdir(response) != -1) {
				new_dir = 1 ;
				dir_dpth++ ;
			}

		/*** Option 3.2  Otherwise attempt to execute command */
			else 
				do_call(response) ;
		}
	}
}

/* :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: */
