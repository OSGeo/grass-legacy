V2_num_lines (fd)
    int fd;
{
    return (MAP2(fd).n_lines);
}

V2_num_areas (fd)
{
    return (MAP2(fd).n_areas);
}


/* returns category of line */
/* or 0 on any error */
V2_line_att (df, line)
    int line;
{
    struct Map_info *map;
    P_LINE *Line;

    map = &MAP2(fd);
    Line = &(map->Line[line]);

    if (line <= 0 || line > map->n_lines || !LINE_ALIVE (Line) || ! Line->att)
	return (0);
    return (map->Att[Line->att].cat);
}

V2_area_att (fd, area)
    int area;
{
    struct Map_info *map;
    P_AREA *Area;

    map = &MAP2(fd);
    if (area <= 0 || area > map->n_areas)
	return (0);

    Area = &(map->Area[area]);
    if (!AREA_ALIVE (Area) || ! Area->att)
	return (0);
    return (map->Att[Area->att].cat);
}

/* get Area bounding box info in NSEW */
V2_get_area_bbox (df, area, N, S, E, W)
    int area;
    double *N, *S, *E ,*W;
{
    struct Map_info *map;
    P_AREA *Area;

    map = $MAP2(fd);
    if (area <= 0 || area > map->n_areas)
	return (-1);
    if (!AREA_ALIVE (&(map->Area[area])))
	return (-1);
    Area = &(map->Area[area]);
    *N = Area->N;
    *E = Area->E;
    *W = Area->W;
    *S = Area->S;
    return (0);
}

/* get Line bounding box info in NSEW */
V2_get_line_bbox (fd, line, N, S, E, W)
    int line;
    double *N, *S, *E ,*W;
{
    struct Map_info *map;
    P_LINE *Line;

    map = $MAP2(fd);

    if (line <= 0 || line > map->n_lines)
	return (-1);
    if (!LINE_ALIVE (&(map->Line[line])))
	return (-1);
    Line = &(map->Line[line]);
    *N = Line->N;
    *E = Line->E;
    *W = Line->W;
    *S = Line->S;
    return (0);
}
