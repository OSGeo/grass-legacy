#include "watershed.h"
#include "string.h"

com_line_Gwater (input, output)
	INPUT	*input;
	OUTPUT	*output;
{
  struct Cell_head *window;
  char	map_layer[48], char_input[48], buf[100], *prog_name, *mapset;
  double d;
  int	i;
  
  window = &(output->window);
  if (0 == G_yes ("Continue?", 1)) exit (0);
  input->haf_name = (char *) calloc (40, sizeof (char));
  input->accum_name = (char *) calloc (40, sizeof (char));
  printf ("\nThis set of questions will organize the command line for the\n");
  printf ("%s program to run properly for your application.\n", NON_NAME);
  printf ("The first question is whether you want %s to run\n", NON_NAME);
  printf ("in its fast mode or its slow mode.  If you run %s\n", NON_NAME);
  printf ("in the fast mode, the computer will finish about 10 times faster\n");
  printf ("than in the slow mode, but will not allow other programs to run\n");
  printf ("at the same time.  The fast mode also places all of the data into\n");
  printf ("RAM, which limits the size of window that can be run.  The slow \n");
  printf ("mode uses disk space in the same hard disk partition as where GRASS is\n");
  printf ("stored.  Thus, if the program does not work in the slow mode, you will\n");
  printf ("need to remove unnecessary files from that partition.  The slow mode\n");
  printf ("will allow other processes to run concurrently with %s.\n\n", NON_NAME);
  sprintf (buf, "Do you want to use the fast mode of %s?", NON_NAME);
  input->com_line_ram = input->com_line_seg  = NULL;
  input->fast = 0;
  input->slow = 0;
  if (G_yes (buf,1))	{
	input->fast = 1;
  	input->com_line_ram = (char *) calloc (400, sizeof (char));
	prog_name = G_store (RAM_NAME);
  	sprintf(input->com_line_ram, 
		"%s/etc/water/%s", G_getenv("GISBASE"), RAM_NAME);
  	printf ("\nIf there is not enough ram for the fast mode (%s) to run,\n", RAM_NAME);
  	sprintf (buf, "should the slow mode (%s) be run instead?", SEG_NAME);
  	if (G_yes (buf,1)) {
		input->slow = 1;
  		input->com_line_seg = (char *) calloc (400, sizeof (char));
  		sprintf(input->com_line_seg, 
			"%s/etc/water/%s", G_getenv("GISBASE"), SEG_NAME);
	}
  } else	{
  	input->slow = 1;
	prog_name = G_store (SEG_NAME);
  	input->com_line_seg = (char *) calloc (400, sizeof (char));
  	sprintf(input->com_line_seg, 
		"%s/etc/water/%s", G_getenv("GISBASE"), SEG_NAME);
  }
  printf ("\nIf you hit <return> by itself for the next question, this \n");
  printf ("program will terminate.\n");
  mapset = G_ask_old ("What is the name of the elevation map layer?",
  		map_layer, "cell", "cell");
  if (!mapset) exit (1);
  if (input->fast)
  	com_line_add (&(input->com_line_ram), " el=", map_layer, mapset);
  if (input->slow)
  	com_line_add (&(input->com_line_seg), " el=", map_layer, mapset);
  printf ("\nOne of the options for %s is a `depression map'.  A\n", prog_name);
  printf ("depression map indicates all the locations in the current map window where \n");
  printf ("water accumulates and does not leave by the edge of the map. Lakes without \n");
  printf ("outlet streams and sinkholes are examples of `depressions'.  If you wish to \n");
  printf ("have a depression map, prepare a map where non-zero values indicate the \n");
  printf ("locations where depressions occur.\n\n");
  printf ("Hit <return> by itself for the next question if there is no depression map.\n");
  mapset = G_ask_old ("What is the name of the depression map layer?",
  		map_layer, "cell", "cell");
  if (mapset) {
     if (input->fast)
	com_line_add (&(input->com_line_ram), " de=", map_layer, mapset);
     if (input->slow)
	com_line_add (&(input->com_line_seg), " de=", map_layer, mapset);
  }
  printf ("\nThe %s program will divide the elevation map into a number of\n", prog_name);
  printf ("watershed basins.  The number of watershed basins is indirectly determined \n");
  printf ("by the `basin threshold' value.  The basin threshold is the area necessary for \n");
  printf ("%s to define a unique watershed basin.  This area only applies to\n", prog_name);
  printf ("`exterior drainage basins'.  An exterior drainage basin does not have any \n");
  printf ("drainage basins flowing into it.  Interior drainage basin size is determined \n");
  printf ("by the surface flow going into stream segments between stream interceptions.  \n");
  printf ("Thus interior drainage basins can be of any size.  The %s program\n", prog_name);
  printf ("also allows the user to relate basin size to potential overland flow\n");
  printf ("(i.e., areas with low infiltration capacities will need smaller areas to\n");
  printf ("develop stream channels than neighboring areas with high infiltration rates).\n");
  printf ("The user can create a map layer with potential overland flow values, and\n");
  printf ("%s will accumulate those values instead of area.\n\n", prog_name);
  printf ("What unit of measure will you use for the basin threshold:\n");
  do	{
	printf (" 1) acres,          2) meters sq., 3) miles sq., 4) hectares,\n");
	printf (" 5) kilometers sq., 6) map cells,  7) overland flow units\n");
  	printf ("Choose 1-7 or 0 to exit this program: ");
  	G_gets (map_layer);
  	sscanf (map_layer, "%d", &i);
  } while (i > 7 || i < 0);
  if (!i) exit(0);
  output->type_area = (char) i;
  printf ("\nHow large an area (or how many overland flow units) must a drainage basin \n"); 
  printf ("be for it to be an exterior drainage basin: ");
  G_gets (map_layer);
  sscanf (map_layer, "%lf", &d);
  switch (i)	{
	case 1: if (input->fast)
			basin_com_add (&(input->com_line_ram), d, ACRE_TO_METERSQ, window);
		if (input->slow)
			basin_com_add (&(input->com_line_seg), d, ACRE_TO_METERSQ, window);
		break;
	case 2: if (input->fast)
			basin_com_add (&(input->com_line_ram), d, 1.0, window, window);
		if (input->slow)
			basin_com_add (&(input->com_line_seg), d, 1.0, window, window);
		break;
	case 3: if (input->fast) 
			basin_com_add (&(input->com_line_ram), d, MILESQ_TO_METERSQ, window);
		if (input->slow) 
			basin_com_add (&(input->com_line_seg), d, MILESQ_TO_METERSQ, window);
		break;
	case 4: if (input->fast) 
			basin_com_add (&(input->com_line_ram), d, HECTACRE_TO_METERSQ, window);
		if (input->slow)
			basin_com_add (&(input->com_line_seg), d, HECTACRE_TO_METERSQ, window);
		break;
	case 5: if (input->fast) 
			basin_com_add (&(input->com_line_ram), d, KILOSQ_TO_METERSQ, window); 
	 	if (input->slow) 
			basin_com_add (&(input->com_line_seg), d, KILOSQ_TO_METERSQ, window); 
		break; 
	case 6: if (input->fast) 
			basin_com_add (&(input->com_line_ram), d, (window->ns_res * window->ew_res), window);
		if (input->slow)
			basin_com_add (&(input->com_line_seg), d, (window->ns_res * window->ew_res), window);
		break;
	case 7: /* needs an overland flow map */
  		printf ("\nIf you hit <return> by itself for the next question, this \n");
  		printf ("program will terminate.\n");
  		mapset = G_ask_old ("What is the name of the overland flow map layer?",
  				map_layer, "cell", "cell");
  		if (!mapset) exit (1);
  		if (input->fast) {
  			com_line_add (&(input->com_line_ram), " ov=", map_layer, mapset);
			basin_com_add (&(input->com_line_ram), d, (window->ns_res * window->ew_res), window);
		}
  		if (input->slow) {
  			com_line_add (&(input->com_line_seg), " ov=", map_layer, mapset);
			basin_com_add (&(input->com_line_seg), d, (window->ns_res * window->ew_res), window);
		}
		break;
  }
  printf ("\n%s must create a map layer of watershed basins\n", prog_name);
  printf ("before %s can run properly.\n", G_program_name());
  strcpy (buf, "Please input watershed basin map name:");
  do	{
  	mapset = G_ask_new (buf, input->haf_name, "cell", "");
  } while (NULL == mapset);
  if (input->fast)
	com_line_add (&(input->com_line_ram), " ba=", input->haf_name, NULL);
  if (input->slow)
	com_line_add (&(input->com_line_seg), " ba=", input->haf_name, NULL);
  printf ("\n%s must create a file of watershed basin relationships \n", prog_name);
  printf ("before %s can run properly (this file is also used by the \n", G_program_name());
  printf ("grass.armsed program).\n");
  input->ar_file_name = NULL;
  while (input->ar_file_name == NULL)	{
  	printf ("\nPlease input the name that this file will use:");
  	G_gets (char_input);
  	if ( 1 != G_legal_filename (char_input))	{
		printf ("[%s] file name not legal\n", char_input);
		free (char_input);
	} else	input->ar_file_name = G_store (char_input);
  }
  if (input->fast)
	com_line_add (&(input->com_line_ram), " ar=", input->ar_file_name, NULL);
  if (input->slow)
	com_line_add (&(input->com_line_seg), " ar=", input->ar_file_name, NULL);
  printf ("\n%s will generate a lot of output.  Indicate a file\n", G_program_name());
  printf ("name for %s to send the output to.\n", G_program_name());
  output->file_name = NULL;
  while (output->file_name == NULL)	{
  	printf ("\nPlease input the name of this file:");
  	G_gets (char_input);
  	if ( 1 != G_legal_filename (char_input))	{
		printf ("[%s] file name not legal\n\n", char_input);
		free (char_input);
	} else	output->file_name = G_store (char_input);
  }
  printf("\nThe accumulation map from %s must be present for\n", prog_name, G_program_name());
  printf ("%s to work properly.\n", G_program_name());
  strcpy (buf, "Please input accumulation map name:");
  do	{
  	mapset = G_ask_new (buf, input->accum_name, "cell", "");
  } while (NULL == mapset);
  if (input->fast)
	com_line_add (&(input->com_line_ram), " ac=", input->accum_name, NULL);
  if (input->slow)
	com_line_add (&(input->com_line_seg), " ac=", input->accum_name, NULL);
  printf ("\n%s can produce several maps not necessary for\n", prog_name);
  printf ("%s to function (stream channels, overland flow aspect, and \n", G_program_name());
  printf ("a display version of the accumulation map).  %s also has the \n", prog_name);
  printf ("ability to generate several variables in the Revised Universal Soil Loss \n");
  printf ("Equation (Rusle): Slope Length (LS), and Slope Steepness (S).\n\n");
  sprintf (buf, "Would you like any of these maps to be created?");
  if (G_yes(buf, 1))	{
	mapset = G_ask_new ("", map_layer, "cell", "stream channel");
	if (mapset != NULL) {
	    if(input->fast)
		com_line_add (&(input->com_line_ram), " se=", map_layer, NULL);
	    if(input->slow)
		com_line_add (&(input->com_line_seg), " se=", map_layer, NULL);
	}
	mapset = G_ask_new ("", map_layer, "cell", "half basin");
	if (mapset != NULL) {
	    if(input->fast)
		com_line_add (&(input->com_line_ram), " ha=", map_layer, NULL);
	    if(input->slow)
		com_line_add (&(input->com_line_seg), " ha=", map_layer, NULL);
	}
	mapset = G_ask_new ("", map_layer, "cell", "overland aspect");
	if (mapset != NULL) {
	    if(input->fast)
		com_line_add (&(input->com_line_ram), " dr=", map_layer, NULL);
	    if(input->slow)
		com_line_add (&(input->com_line_seg), " dr=", map_layer, NULL);
	}
	mapset = G_ask_new ("", map_layer, "cell", "display");
	if (mapset != NULL) {
	    if(input->fast)
		com_line_add (&(input->com_line_ram), " di=", map_layer, NULL);
	    if(input->slow)
		com_line_add (&(input->com_line_seg), " di=", map_layer, NULL);
	}
	i = 0;
	mapset = G_ask_new ("", map_layer, "cell", "Slope Length");
	if (mapset != NULL) {
	    i = 1;
	    if(input->fast)
		com_line_add (&(input->com_line_ram), " LS=", map_layer, NULL);
	    if(input->slow)
		com_line_add (&(input->com_line_seg), " LS=", map_layer, NULL);
	}
	mapset = G_ask_new ("", map_layer, "cell", "Slope Steepnes");
	if (mapset != NULL) {
	    i = 1;
	    if(input->fast)
		com_line_add (&(input->com_line_ram), " S=", map_layer, NULL);
	    if(input->slow)
		com_line_add (&(input->com_line_seg), " S=", map_layer, NULL);
	}
	if (i)	{
	 	printf("\nThe Slope Length factor (LS) and Slope Steepness (S) are influenced by \n");
	 	printf("disturbed land.  %s reflects this with an optional map layer or value\n", prog_name);
	 	printf("where the value indicates the percent of disturbed (barren) land in that cell.\n");
	 	printf("Type <return> if you do not have a disturbed land map layer.\n");
	 	mapset = G_ask_old ("", map_layer, "cell", "disturbed land");
	 	if (mapset != NULL) {
		    if (input->fast)
			com_line_add (&(input->com_line_ram), " r=", map_layer, NULL);
		    if (input->slow)
			com_line_add (&(input->com_line_seg), " r=", map_layer, NULL);
	 	}else {
	 		printf ("\nType the value indicating the percent of disturbed land.  This value will\n");
			printf ("be used for every cell in the current region.\n");
			i = -6;
			while (i < 0 || i > 100) {
				printf ("\nInput value here [0-100]: ");
				fgets (buf, 80, stdin);
				sscanf (buf, "%d", &i);
			}
			if (input->fast) com_add (&(input->com_line_ram), " r=", i);
			if (input->slow) com_add (&(input->com_line_seg), " r=", i);
	 	}
	 	/*       12345678901234567890123456789012345678901234567890123456789012345678901234567890 */
	 	printf ("\nOverland surface flow only occurs for a set distance before swales form.\n");
	 	printf ("Because of digital terrain model limitations, %s cannot pick up\n", prog_name);
	 	printf ("these swales.  %s allows for an input (warning: kludge factor)\n", prog_name);
	 	printf ("that prevents the surface flow distance from getting too long.  Normally,\n");
	 	printf ("maximum slope length is around 600 feet (about 183 meters).\n");
	 	i = -1;
	 	while (i < 0) {
			printf ("\nInput maximum slope length here (in meters): ");
			fgets (buf, 80, stdin);
			sscanf (buf, "%d", &i);
	 	}
	 	if (input->fast) com_add (&(input->com_line_ram), " ms=", i);
	 	if (input->slow) com_add (&(input->com_line_seg), " ms=", i);
	 	/*       12345678901234567890123456789012345678901234567890123456789012345678901234567890 */
  		printf ("\nRoads, ditches, changes in ground cover, and other factors will stop\n");
		printf ("slope length.  You may input a raster map indicating the locations of these\n");
		printf ("blocking factors.\n\n");
  		printf ("Hit <return> by itself for the next question if there is no blocking map.\n");
  		mapset = G_ask_old ("What is the name of the blocking map layer?",
  				map_layer, "cell", "cell");
  		if (mapset) {
     			if (input->fast)
				com_line_add (&(input->com_line_ram), " ob=", map_layer, mapset);
     			if (input->slow)
				com_line_add (&(input->com_line_seg), " ob=", map_layer, mapset);
  		}
	}
  }
}

com_line_add (com_line, prompt, map_layer, mapset)
char **com_line, *prompt, *map_layer, *mapset;
{
	G_strcat (*com_line, prompt);
	G_strcat (*com_line, "\"");
	G_strcat (*com_line, map_layer);
	if (mapset)	{
		G_strcat (*com_line, "@");
		G_strcat (*com_line, mapset);
	}
	G_strcat (*com_line, "\"");
}

basin_com_add (com_line, d, modifier, window)
char **com_line;
double modifier, d;
struct Cell_head *window;
{
	int i;
	char buf[20];

	i = (int) (.5 + modifier * d / window->ns_res / window->ew_res);
	if (i < 1) i = 1;
	sprintf (buf, " t=%d", i);
	G_strcat (*com_line, buf);
}

com_add (com_line, prompt, ril_value)
char **com_line, *prompt;
int	ril_value;
{
	char buf[20];

	G_strcat (*com_line, prompt);
	sprintf (buf, "%d", ril_value);
	G_strcat (*com_line, buf);
}
