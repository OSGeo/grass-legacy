#include "report.h"
#define FD	report->fd
#define OFFSET	report->offset
#define FIELD	report->field

report_find_layer (report, layer_num)

	REPORT *report;
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

report_find_cat (report, layer_num, cat)

	REPORT *report;
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

report_find_point (report, point_num)

	REPORT *report;
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

report_find_data (report, layer_num, point_num)

	REPORT *report;
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
