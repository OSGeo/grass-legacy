#include "report.h"

int report_seek_layers ( REPORT *report)
{
	fseek (report->fd, report->offset.layers, 0);
	return 0;
}

int report_seek_points ( REPORT *report)
{
	fseek (report->fd, report->offset.points, 0);
	return 0;
}

int report_seek_cats ( REPORT *report)
{
	fseek (report->fd, report->offset.cats, 0);
	return 0;
}

int report_seek_data ( REPORT *report)
{
	fseek (report->fd, report->offset.data, 0);
	return 0;
}
