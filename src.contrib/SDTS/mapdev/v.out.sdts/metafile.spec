IDEN_MPDT (8) (YYYY or YYYYMMDD)

IDEN_TITL (40) <overall title for data contained in transfer set. (default is
				 map_name from vector header)>

IDEN_COMT (100) <optional comment to be added to SDTS indentification module>

XREF_HDAT (3) <Geodetic datum for transfer set. no default. normally a 3-char.
			   code, e.g. "North American 1927", but can be a text description>> 

DDDF_GRASS_ENT  <Descriptive definition of entities contained in transfer set>

DDDF_ATTR_NUM  <Definition of dig_atts for this  transfer set. Default is 
				"Integer number in GRASS dig_att assigned to one or more 
				GRASS spatial objects">

DDDF_ATTR_LABEL <Definitionof dig_cats for this transfer set. Default is
				"Label or description in GRASS dig_cats file associated with a
				particular value in the GRASS dig_atts file.">

DDSH_ENT_NAME (18) <Name or label designating entities contained in transfer 
					set. Default is name of vector map being transferred.>


file format:

IDEN_MPDT:1992
IDEN_TITL:SDTS sdts.test sample vector
IDEN_COMT:This is a sample comment taken from the user-supplied metadata file
XREF_HDAT:NAS
DDDF_GRASS_ENT:This is a test, so the entity is a non-thing. and i want to make this as long as i have the patience to write, so just in case we have any problems and here we are over 100 characters. how about that?
DDDF_ATTR_NUM:This is a user supplied attribute number definition
DDDF_ATTR_LABEL:This is a user supplied attribute description definition
DDSH_ENT_NAME:ABSTRACT_OBJS

