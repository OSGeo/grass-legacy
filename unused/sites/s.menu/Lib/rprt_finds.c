#include "report.h"
#include <stdlib.h>
#include <unistd.h>
#define FD	report->fd
#define OFFSET	report->offset
#define FIELD	report->field

int report_find_layer (REPORT *report,int layer_num)
{
	fseek (FD, OFFSET.layers_next, 0);
	while (report_read_record(report,"layer"))
	{
		if (atoi(FIELD[1]) == layer_num)
		{
			OFFSET.layers_next = OFFSET.next;
			return 1;
		}
	}

	fseek (FD, OFFSET.layers, 0);
	while (report_read_record(report,"layer"))
		if (atoi(FIELD[1]) == layer_num)
		{
			OFFSET.layers_next = OFFSET.next;
			return 1;
		}

	OFFSET.layers_next = OFFSET.layers;
	return 0;
}

int report_find_cat (REPORT *report,int layer_num,int cat)
{
	fseek (FD, OFFSET.cats_next, 0);
	while (report_read_record(report,"cat"))
	{
		if (atoi(FIELD[1]) == layer_num && atoi(FIELD[2]) == cat)
		{
			OFFSET.cats_next = OFFSET.next;
			return 1;
		}
	}

	fseek (FD, OFFSET.cats, 0);
	while (report_read_record(report,"cat"))
		if (atoi(FIELD[1]) == layer_num && atoi(FIELD[2]) == cat)
		{
			OFFSET.cats_next = OFFSET.next;
			return 1;
		}

	OFFSET.cats_next = OFFSET.cats;
	return 0;
}

int report_find_point (REPORT *report,int point_num) 
{
	fseek (FD, OFFSET.points_next, 0);
	while (report_read_record(report,"point"))
	{
		if (atoi(FIELD[1]) == point_num)
		{
			OFFSET.points_next = OFFSET.next;
			return 1;
		}
	}

	fseek (FD, OFFSET.points, 0);
	while (report_read_record(report,"point"))
		if (atoi(FIELD[1]) == point_num)
		{
			OFFSET.points_next = OFFSET.next;
			return 1;
		}

	OFFSET.points_next = OFFSET.points;
	return 0;
}

int report_find_data (REPORT *report,int layer_num,int point_num) 
{
	fseek (FD, OFFSET.data_next, 0);
	while (report_read_record(report,"data"))
	{
		if (atoi(FIELD[1]) == layer_num && atoi(FIELD[2]) == point_num)
		{
			OFFSET.data_next = OFFSET.next;
			return 1;
		}
	}

	fseek (FD, OFFSET.data, 0);
	while (report_read_record(report,"data"))
		if (atoi(FIELD[1]) == layer_num && atoi(FIELD[2]) == point_num)
		{
			OFFSET.data_next = OFFSET.next;
			return 1;
		}

	OFFSET.data_next = OFFSET.data;
	return 0;
}
