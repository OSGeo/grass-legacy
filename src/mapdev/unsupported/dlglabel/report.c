#include "dlg.h"

report()
{
	FILE *outfile ;
	FILE *popen() ;
	char buffer[20] ;
	int choice ;

	Old_tty() ;
	clear_screen() ;

	printf("Where would you like the report to be printed?\n") ;
	printf("   1  -  screen\n") ;
	printf("   2  -  /tmp directory\n") ;
	printf("   3  -  printer\n") ;
	gets(buffer) ;
	sscanf(buffer, "%d", &choice) ;

	switch (choice)
	{
		case 1:
			outfile = stderr ;
			break ;
		case 2:
			sprintf(buffer, "/tmp/dlgrpt%d", getpid()) ;
			outfile = fopen(buffer,"w") ;
			if (outfile == NULL)
			{
				printf("Sorry, can't open tmp file: %s\n", buffer) ;
				sleep(2) ;
				New_tty() ;
				return(-1) ;
			}
			break ;
		case 3:
			outfile = popen("lpr", "w") ;
			if (outfile == NULL)
			{
				printf("Sorry, can't open printer.\n") ;
				sleep(2) ;
				New_tty() ;
				return(-1) ;
			}
			printf("Please wait; generating report for printer\n") ;
			break ;
		default:
			outfile = stderr ;
			break ;
	}

	report_nodes(outfile) ;

	report_areas(outfile) ;

	report_lines(outfile) ;

	switch (choice)
	{
		case 1:
			break ;
		case 2:
			fclose(outfile) ;
			break ;
		case 3:
			pclose(outfile) ;
			break ;
	}

	fprintf(stderr,"\n\nDone checking nodes, areas, and lines. Hit RETURN to continue.\n") ;
	gets(buffer) ;

	New_tty() ;
}

report_nodes(outfile) 
	FILE *outfile ;
{
	int i, j, k ;

	fprintf(outfile,"\nChecking NODES\n") ;

	for (i=1; i<=tot_nodes; i++)
	{
		if (node[i].x == 0.0)
			continue ;
	/* For each line, check that line references node correctly */
		for (j=0; j<node[i].n_lines; j++)
		{
			k = node[i].lines[j] ;
			if (k > 0)
			{
				if (line[k].start_node != i)
				{
					fprintf(outfile,"Node %4d claims to be the start node for line %4d\n", i, k) ;
					if (line[k].end_node == i)
						fprintf(outfile,"  Line %4d thinks that node %4d is its end node.\n", k, i) ;
					else
						fprintf(outfile,"  Line %4d claims to start with node %4d and end with node %4d\n", 
							line[k].start_node, line[k].end_node) ;
				}
			}
			else if (k < 0)
			{
				k = abs(k) ;
				if (line[k].end_node != i)
				{
					fprintf(outfile,"Node %4d claims to be the end node for line %4d\n", i, k) ;
					if (line[k].end_node == i)
						fprintf(outfile,"  Line %4d thinks that node %4d is its start node.\n", k, i) ;
					else
						fprintf(outfile,"  Line %4d claims to start with node %4d and end with node %4d\n", 
							line[k].start_node, line[k].end_node) ;
				}
			}
		}
	/* Check that there are n_atts attributes */
	}
}

report_areas(outfile) 
	FILE *outfile ;
{
	int i, j, k ;
	int at_start ;
	int islands ;
	int first_node, cur_node, next_node ;
	int new_node ;
	int first_line, cur_line, next_line, last_line ;

	fprintf(outfile,"\nChecking AREAS\n") ;

	/* For each area */
	for (i=1; i<=tot_areas; i++)
	{
		at_start = 1 ;
		islands = 0 ;

		if (area[i].x == 0.0)
			continue ;
		if (! area[i].n_lines)
		{
			fprintf(outfile,"  Area %4d claims to have NO lines associated with it.\n", i) ;
			fprintf(outfile,"     Ending evaluation of this area.\n") ;
			continue ;
		}

		for (j=0; j<area[i].n_lines; j++)
		{
			next_line = area[i].lines[j] ;
			if (at_start)
			{
				first_line = area[i].lines[j] ;
				if (! line[abs(first_line)].start_node)
				{
					fprintf(outfile,"  Area %4d claims to be associated with non-existing line %4d\n",
						i, abs(first_line) ) ;
					fprintf(outfile,"     Ending evaluation of this area.\n") ;
					break ;
				}

				/* Establish first and current node */
				if (first_line > 0)
				{
					first_node = line[first_line].start_node ;
					cur_node = line[first_line].end_node ;
				}
				else if (first_line < 0)
				{
					first_node = line[abs(first_line)].end_node ;
					cur_node = line[abs(first_line)].start_node ;
				}
				else
				{
					fprintf(outfile,"  Area %4d references illegal node 0.\n", i) ;
					fprintf(outfile,"     Ending evaluation of this area.\n") ;
					break ;
				}
				at_start = 0 ;
			}
			else
			{
				last_line = next_line ;
				next_line = area[i].lines[j] ;
				if (next_line > 0)
				{
					next_node = line[next_line].start_node ;
					new_node = line[next_line].end_node ;
				}
				else if (next_line < 0)
				{
					next_node = line[abs(next_line)].end_node ;
					new_node = line[abs(next_line)].start_node ;
				}
				else   /* We have an island */
				{
					next_node = first_node ;
					next_line = last_line ;
					islands++ ;
					at_start = 1 ;
				}

				if (next_node != cur_node)
				{
					fprintf(outfile,"  Area %4d incorrectly connects lines %4d and %4d\n",
						i, cur_line, next_line) ;
				}

				if (! at_start)
				{
					cur_line = next_line ;
					cur_node = new_node ;
				}
			}
		}
	
	/* Check claimed number of islands */
		if (area[i].n_isles != islands)
		{
			fprintf(outfile,"  Area %4d claims %d islands; only %d found\n",
				i, area[i].n_isles, (islands - 1)) ;
			fprintf(outfile,"    island count being modified\n") ;
			area[i].n_isles = (islands - 1) ;
		}
	/* Check that there are n_atts attributes */
	}
}

report_lines(outfile) 
	FILE *outfile ;
{
	int i, j, k ;
	int atnode ;
	int atarea ;
	int found ;
	int n_lines ;
	char *claimed_area ;
	char *falloc() ;

	fprintf(outfile,"\nChecking LINES\n") ;
	claimed_area = falloc(tot_areas, sizeof(char)) ;

/* Clear array */
	for(i=1; i<=tot_areas; i++)
		claimed_area[i] = 0 ;

/* For each line */
	for (i=1; i<=tot_lines; i++)
	{
	/* Record that areas on either side should exist;  checked later */
		claimed_area[line[i].right_area] = 1 ;
		claimed_area[line[i].left_area] = 1 ;

	/* Check that start node exists and knows about line */
		atnode = line[i].start_node ;
		/* check existence of starting node */
		if (node[atnode].x == 0.0)
		{
			fprintf(outfile,"  Line %4d begins with node %d which doesn't exist.\n",
				i, atnode) ;
		}
		else
		{
			n_lines = node[atnode].n_lines ;
			found = 0 ;
			for (j=0; j<n_lines; j++)
			{
				if (node[atnode].lines[j] == i)
				{
					found = 1 ;
					break ;
				}
			}
			if (!found)
			{
				for (j=0; j<n_lines; j++)
				{
					if (node[atnode].lines[j] == i)
					{
						found = 1 ;
						break ;
					}
				}
				if (!found)
					fprintf(outfile,"  Line %4d begins with node %d which doesn't know it.\n",
					i, atnode) ;
				else
					fprintf(outfile,"  Line %4d begins with node %d which thinks it ends with it.\n",
					i, atnode) ;
			}
		}

	/* Check that end node exists and knows about line */
		atnode = line[i].end_node ;
		/* check existence */
		if (node[atnode].x == 0.0)
		{
			fprintf(outfile,"  Line %4d ends with node %d which doesn't exist.\n",
				i, atnode) ;
		}
		else
		{
			n_lines = node[atnode].n_lines ;
			found = 0 ;
			for (j=0; j<n_lines; j++)
			{
				if (node[atnode].lines[j] == -i)
				{
					found = 1 ;
					break ;
				}
			}
			if (!found)
			{
				for (j=0; j<n_lines; j++)
				{
					if (node[atnode].lines[j] == -i)
					{
						found = 1 ;
						break ;
					}
				}
				if (!found)
					fprintf(outfile,"  Line %4d ends with node %d which doesn't know it.\n",
					i, atnode) ;
				else
					fprintf(outfile,"  Line %4d ends with node %d which thinks it ends with it.\n",
					i, atnode) ;
			}
		}

	/* Check that node coordinates are close to coordinates in line */
	/*   Skip this for now because there can be a visual check on screen */

	/* Check that areas exist and properly reference line */
	
		for (k=0; k<2; k++)
		{
			switch(k)
			{
				case 0:
					atarea = line[i].left_area ;
					break ;
				case 1:
					atarea = line[i].right_area ;
					break ;
			}

			/* check existence */
			if (!atarea)
				continue ;

/*
			if (area[atarea].x == 0.0)
			{
				fprintf(outfile,"  Line %4d borders area %d which doesn't exist.\n",
					i, atarea) ;
			}
			else
*/
			if (area[atarea].x != 0.0)
			{
				n_lines = area[atarea].n_lines ;
				found = 0 ;
				for (j=0; j<n_lines; j++)
				{
					if (abs(area[atarea].lines[j]) == i)
					{
						found = 1 ;
						break ;
					}
				}
				if (!found)
					fprintf(outfile,"  Line %4d borders area %d which doesn't know it.\n",
						i, atarea) ;
			}
		}
	/* Check that there are n_atts attributes */
	}

/* Check for area existence */
	for(i=1; i<=tot_areas; i++)
		if (claimed_area[i])
		{
			if (area[i].x == 0.0)
				fprintf(outfile,"  Area %4d not linked.\n", i) ;
		}

	free(claimed_area) ;
}
