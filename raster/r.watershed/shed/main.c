#include <stdlib.h>
#include "watershed.h"
#include "string.h"

int main (int argc, char *argv[]) {
	INPUT	input;
	OUTPUT	output;
	char	buf[500];

	G_gisinit (argv[0]);
	G_set_program_name ("r.watershed");
	G_get_window (&(output.window));
	intro ();
	output.num_maps = 0;
	com_line_Gwater (&input, &output); /* develops r.watershed command line */
	basin_maps (&input, &output); /* organizes map layers output */
	if (input.fast || input.slow) { 
	  if (input.fast) {
	    if (G_system (input.com_line_ram)) {
	      if (input.slow) {
		fprintf (stdout,"Slow version of water analysis program starting now\n");
    		if (G_system (input.com_line_seg)) {
			sprintf(buf,"<<%s>> command line failed",
				input.com_line_seg);
			G_fatal_error (buf);
			free_input (&input);
			free_output (&output);
			exit (1);
		}
	      }
	    }
	  } else if (G_system (input.com_line_seg)) {
		sprintf(buf,"<<%s>> command line failed",
			input.com_line_seg);
		G_fatal_error (buf);
		free_input (&input);
		free_output (&output);
		exit (1);
	  }
	}

	/*
	ARMSED:  This section exists to create the stats part.
	input.ar_file_name could be used as a flag to determine this stuff
	should be run.
	*/
#ifdef ARMSED
	ar_file_in (input.ar_file_name, &output);
	read_basins (input.haf_name, &output);
	valid_basins (input.accum_name, &output);
	free_input (&input);
	if ((output.out_file = fopen (output.file_name, "w")) == NULL) {
		free_output (&output);
		G_fatal_error ("unable to open output file");
		exit (1);
	}
	if (output.do_basin) {
		fprintf (output.out_file, "\n\nThese values are accumulations within the basin itself\n");
		fprintf (output.out_file, "They do not include sub-basins\n\n");
		print_output (&output);
	}
	if (output.do_accum) {
		accum_down (&output);
		fprintf (output.out_file, "\n\nThese values are accumulations of basins and sub-basins\n");
		print_output (&output);
	}
	free_output (&output);

#endif
	/*
	end of ARMSED comment code
	*/

	exit (0);
}
