#include <string.h>
#include <unistd.h>
#include <sys/file.h>
#include "gis.h"
#include "main.h"
#include "menu.h"
#include "help_proto.h"

static char **phrases ;
static int alloc_phrase = 0 ;
static int n_phrases ;
static char **files ;
#define MAX_FILE 100

static int init_phrases();
static char *get_file_name(char *);

int main(int argc, char **argv)
{
	int	choice[1] ;
	int	menu_state ;
	char response[256] ;
	int Length ;  
	int Prob ;
	char *Tablbuf ;
	char file_stack[MAX_FILE][512] ;
	char response_stack[MAX_FILE][80] ;
	int cur_line[MAX_FILE] ;
	int cur_hilite[MAX_FILE] ;
	int stack_ptr ;
	char *get_file_name() ;
	char *file_name ;
	char errbuf[128] ;

	sprintf(errbuf,"%s/etc/help", G_gisbase()) ;
	if (chdir(errbuf))
	{
		fprintf(stderr,"Directory: %s unavailable\n", errbuf) ;
		exit(1) ;
	}

/*** Initialize the phrase to file lookup table ***/
	init_phrases() ;

/*** Initialize the menu   */
	P_menuinit();
	menu_state = MINIRESP ;

	strcpy(file_stack[0], INDEX_FILE) ;
	strcpy(response_stack[0], 
	"GRASS ON-LINE HELP                     Press [q]uit to exit program");
	cur_hilite[0] = 1 ;
	cur_line[0] = 1 ;
	stack_ptr = 0 ;

	Prob = F_fetchfile (VERBOSE_FILE, &Tablbuf, &Length);
	P_writowin (TablW, Tablbuf, 1, Length, 4);

/*** Main loop */

	for(;;)
	{
		*response = 0 ;
		P_writowin(PlanetW, response_stack[stack_ptr], 1, 1, 1);

		F_menu(file_stack[stack_ptr], HELP_FILE, choice, response,
			&menu_state, &cur_line[stack_ptr], &cur_hilite[stack_ptr]  );

		if(*response == '\0')
			continue ;

		if(ESC == *response)
		{
			if (stack_ptr > 0)
				stack_ptr-- ;
			else
				do_leave() ;
			continue ;
		}

		file_name = get_file_name(response) ;

		if ( file_name == NULL )
		{
			sprintf(errbuf,"<%s> does not map to a file name", response) ;
			P_writowin (ErrorW, errbuf, 1, 1, 1);
			sleep(5) ;
		}
		else if (stack_ptr > MAX_FILE - 1)
		{
			P_writowin (ErrorW, "Too many help levels active", 1, 1, 1);
			sleep(5) ;
		}
		else if (access(file_name, R_OK))
		{
			sprintf(errbuf,"File <%s> not accessible", file_name) ;
			P_writowin (ErrorW, errbuf, 1, 1, 1);
			sleep(5) ;
		}
		else
		{
			stack_ptr++ ;
			strcpy(file_stack[stack_ptr], file_name) ;
			strcpy(response_stack[stack_ptr], response) ;
			cur_line[stack_ptr] = 1 ;
			cur_hilite[stack_ptr] = 1 ;
		}
	}
}

static char *get_file_name(char *phrase )
{
	int i ;
	for(i=0; i<n_phrases; i++)
	{
		if(! strcmp(phrase, phrases[i]))
			return(files[i]) ;
	}
	return(NULL) ;
}

static int 
init_phrases (void)
{
	FILE *fptr, *fopen() ;
	char buffer[128] ;
	char *fgets() ;

	if (NULL == (fptr = fopen(LOOKUP, "r")))
	{
		fprintf(stderr,"ERROR: lookup-file <%s> unavailable.\n", LOOKUP) ;
		exit(-1) ;
	}

	for(n_phrases=0; ; n_phrases++)
	{
		if (alloc_phrase <= n_phrases)
		{
			alloc_phrase += 50 ;
			phrases = (char **)G_realloc((char *) phrases, sizeof (char *) * alloc_phrase) ;
			files = (char **)G_realloc((char *) files, sizeof (char *) * alloc_phrase) ;
		}
		if (NULL == fgets(buffer, 128, fptr))
			return 0;
		buffer[strlen(buffer)-1] = '\0' ;
		phrases[n_phrases] = G_store(buffer) ;

		if (NULL == fgets(buffer, 128, fptr))
			return 0;
		buffer[strlen(buffer)-1] = '\0' ;
		files[n_phrases] = G_store(buffer) ;
	}
}
