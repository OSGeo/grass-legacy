/* %W% %G% */
#include <stdio.h>

usage(me)
{
    char buf[300];

    sprintf (buf, "usage: %s el=elevation_map [pi=pit_map] [th=basin_threshold] [ac=accumulation_map] [dr=drainage_pointer_map] [ba=watershed_basin_map] [se=stream_segment_map] [ha=watershed_half_basin_map] [ar=ARMSED_input_file_name]\n[di=display_accumulation_map] [ov=overland_flow_map]", me);
    G_fatal_error (buf);
    exit(1);
}
