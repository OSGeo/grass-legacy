#include "labels.h"

static int label_skylight(FT_Face face, const char *charset, label_t *label);

label_t * labels_init(struct params *p, int *n_labels)
{
	label_t *labels;
    char   *mapset;
	int legal_types, layer, i=0, error, sql_len;
	size_t label_sz;
	struct Map_info Map;
    struct field_info *fi;
	dbDriver *driver;
	FT_Library library;
	FT_Face face;

	legal_types = Vect_option_to_types(p->type);
	label_sz=100;
	G_debug(1, "Need to allocate %ld bytes of memory",
			sizeof(label_t) * label_sz);
	labels=malloc(sizeof(label_t) * label_sz);
	G_debug(1, "labels=%p", labels);

	if(labels == NULL)
		G_fatal_error(_("Cannot allocate %d bytes of memory"),
					  sizeof(label_t)*label_sz);
		
    /* open vector */	
    mapset = G_find_vector2 ( p->map->answer, NULL) ; 
    if (mapset == NULL)
	G_fatal_error(_("Vector map [%s] not available"), p->map->answer);
	/* open vector for read only */
    Vect_open_old (&Map, p->map->answer, mapset);
		
    /* open database */	
    layer = atoi(p->layer->answer);
    fi = Vect_get_field(&Map, layer);
    if ( fi == NULL )
		G_fatal_error(_("Cannot get layer info for vector map"));
    driver = db_start_driver_open_database ( fi->driver, fi->database );
    if ( driver == NULL ) 
		G_fatal_error(_("Cannot open database %s by driver %s"), 
					  fi->database, fi->driver);
	
	sql_len = strlen(p->column->answer) + strlen(fi->table) + 
		strlen(fi->key) + 30;
	
	/* initialize FT 2 library */
	if(FT_Init_FreeType( &library ))
		G_fatal_error(_("Unable to initialise FreeType"));
	error = FT_New_Face(library, p->font->answer, 0, &face);
	if(error == FT_Err_Unknown_File_Format)
		G_fatal_error(_("Font file format is not supported by FreeType"));
	else if(error) {
		G_fatal_error(_("Font file can not be loaded"));
	}
	/* 1 inch tall letters to ease calculations: 1 inch => 1 map unit */
	if(FT_Set_Char_Size(face, 72*64, 72*64, 72, 72)) 
		G_fatal_error(_("Unable to set font size"));

	/* start reading the map */
	while(1) {
		struct line_pnts *Points;
		struct line_cats *Cats;
		
		dbCursor cursor;
		dbTable  *table;
		dbColumn *column;
		dbString query, value;

		int type, cat, more, nrows;
		char *sql;

		if(i == label_sz) { /* we need more memory */
			label_sz+=100;
			G_debug(1, "Need to resize %p to %ld bytes of memory",
					(void *)labels, sizeof(label_t) * label_sz);
			labels = realloc(labels, sizeof(label_t) * label_sz);
			if(labels == NULL) {
				G_fatal_error(_("Cannot allocate more memory"));
			}
		}

		memset(&labels[i], 0, sizeof(label_t));
		
		Points = Vect_new_line_struct();
		Cats = Vect_new_cats_struct();

		type =  Vect_read_next_line (&Map, Points, Cats);
        if ( type == -1 ) G_fatal_error (_("Cannot read vector"));
        if ( type == -2 ) break;  /* EOF */
		if ( !( legal_types & type) ) continue;

		Vect_cat_get(Cats, layer, &cat);
		if ( cat < 0 ) continue; /* no cat for this field */
	
		sql = G_malloc(sql_len);
		/* Read label from database */
		sprintf(sql, "select %s from %s where %s = %d", p->column->answer, 
				fi->table, fi->key, cat);
		G_debug (3, "SQL: %s", sql);
		db_init_string (&query);
		db_set_string ( &query, sql);
		free(sql);
	
        if (db_open_select_cursor(driver, &query, &cursor, DB_SEQUENTIAL) != DB_OK)
            G_fatal_error (_("Cannot select attribute."));
		db_free_string(&query);
		nrows = db_get_num_rows ( &cursor );
		if ( nrows < 1 ) {
			G_warning (_("No database record for category %d"), cat);
			continue;
		}

		if( db_fetch (&cursor, DB_NEXT, &more) != DB_OK || !more ) continue;

		table = db_get_cursor_table (&cursor);
		column = db_get_table_column(table, 0); /* first column */

		db_init_string (&value);
		db_convert_column_value_to_string (column, &value);
		db_close_cursor(&cursor);

        G_debug (3, "Label: %s", db_get_string(&value));

		/* ignor empty strings */
		if(strlen(db_get_string(&value)) == 0) continue;

		labels[i].text=G_strdup(db_get_string(&value));
		labels[i].cat = cat;
		labels[i].type = type;
		labels[i].shape = Points;

		G_debug (3, "Label [%d]: %s, cat=%d, type=0x%02x",i, labels[i].text,
				 labels[i].cat, labels[i].type);

		/* make a skylight for the text */
		label_skylight(face, p->charset->answer, &labels[i]);

		i++;

		db_free_string(&value);
		Vect_destroy_line_struct(Points);
		Vect_destroy_cats_struct(Cats);
	}

	FT_Done_Face(face);
	FT_Done_FreeType(library);
	db_close_database_shutdown_driver(driver);
	Vect_close(&Map);
	
	*n_labels=i;
	return labels;
	
}

static int label_skylight(FT_Face face, const char *charset, label_t *label)
{
	int i, len;
	double advance=0.0;
	
	len = strlen(label->text);
	label->skylight = Vect_new_line_struct();
	G_debug(3, "Creating skylight for '%s'",label->text);

	for(i=0; i < len; i++) {
		FT_UInt glyph_index;

		glyph_index = FT_Get_Char_Index(face, label->text[i]);
		if(FT_Load_Glyph(face, glyph_index, FT_LOAD_DEFAULT))
			G_warning(_("Cannot load glyph for '%c'"),label->text[i]);

		/* insert the 4 corners of the bounding box */
		{
			struct point {
				double x;
				double y;
			}top_left, top_right, bottom_right, bottom_left;

			G_debug(5, "horiBearingX=%ld horiBearingY=%ld width=%ld hight=%ld advance=%ld",
					face->glyph->metrics.horiBearingX,
					face->glyph->metrics.horiBearingY,
					face->glyph->metrics.width,
					face->glyph->metrics.height,
					face->glyph->metrics.horiAdvance);

			top_left.x = advance;
			top_left.y = face->glyph->metrics.horiBearingY / 64.0;

			top_right.x = advance + face->glyph->metrics.horiAdvance / 64.0; 
			top_right.y = face->glyph->metrics.horiBearingY / 64.0;

			bottom_right.x = advance + face->glyph->metrics.horiAdvance / 64.0;
			bottom_right.y = (face->glyph->metrics.horiBearingY -
							  face->glyph->metrics.height) / 64.0;

			bottom_left.x = advance;
			bottom_left.y = (face->glyph->metrics.horiBearingY -
							 face->glyph->metrics.height) / 64.0;

			if(i==0) {
				G_debug(5, "Character(%d) '%c': Adding UL point (%lf,%lf)",
						i, label->text[i], top_left.x, top_left.y);
				Vect_append_point(label->skylight,
								  top_left.x, top_left.y, 0.0);
				G_debug(5, "Character(%d) '%c': Adding UR point (%lf,%lf)",
						i, label->text[i], top_right.x, top_right.y);
				Vect_append_point(label->skylight,
									   top_right.x, top_right.y, 0.0);
				
				G_debug(5, "Character(%d) '%c': Adding LR point (%lf,%lf)",
						i, label->text[i], bottom_right.x, bottom_right.y);
				Vect_append_point(label->skylight,
									   bottom_right.x, bottom_right.y, 0.0);
				
				G_debug(5, "Character(%d) '%c': Adding LL point (%lf,%lf)",
						i, label->text[i], bottom_left.x, bottom_left.y);
				Vect_append_point(label->skylight,
									   bottom_left.x, bottom_left.y, 0.0);
				Vect_append_point(label->skylight,
								  top_left.x, top_left.y, 0.0);
			}
			else { 
				G_debug(5, "Character(%d) '%c': Adding UL point (%lf,%lf)",
						i, label->text[i], top_left.x, top_left.y);
				Vect_line_insert_point(label->skylight, i*2,
									   top_left.x, top_left.y, 0.0);
				G_debug(5, "Character(%d) '%c': Adding UR point (%lf,%lf)",
						i, label->text[i], top_right.x, top_right.y);
				Vect_line_insert_point(label->skylight, i*2+1,
									   top_right.x, top_right.y, 0.0);
				
				G_debug(5, "Character(%d) '%c': Adding LR point (%lf,%lf)",
						i, label->text[i], bottom_right.x, bottom_right.y);
				Vect_line_insert_point(label->skylight, i*2+2,
									   bottom_right.x, bottom_right.y, 0.0);
				
				G_debug(5, "Character(%d) '%c': Adding LL point (%lf,%lf)",
						i, label->text[i], bottom_left.x, bottom_left.y);
				Vect_line_insert_point(label->skylight, i*2+3,
									   bottom_left.x, bottom_left.y, 0.0);
			}
		
			advance += face->glyph->metrics.horiAdvance / 64.0;
			G_debug(5,"Total advance  %lf", advance);
		}
	}
	return 1;
}

void label_candidates(label_t *labels, int n_labels)
{
	return;
}
#if 0
/* open labels */	
    labels = G_fopen_new ("paint/labels", p->labels->answer);

	FT_UInt glyph_index_dot;
	glyph_index_dot = FT_Get_Char_Index(face, '.');
	if(i==0)) { /* Get kerning for first character and if the vector type is a dot */
			FT_Vector delta;
			FT_Get_Kerning(face, 0, glyph_index, FT_KERNING_MODE_UNFITTED
		}
#endif
