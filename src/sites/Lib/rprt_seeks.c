#include "report.h"

report_seek_layers (report)

	REPORT *report;
{
	fseek (report->fd, report->offset.layers, 0);
}

report_seek_points (report)

	REPORT *report;
{
	fseek (report->fd, report->offset.points, 0);
}

report_seek_cats (report)

	REPORT *report;
{
	fseek (report->fd, report->offset.cats, 0);
}

report_seek_data (report)

	REPORT *report;
{
	fseek (report->fd, report->offset.data, 0);
}
