program:"r.profile"

title:"Output Values Lying on User-Defined Lines"

commandstring:"map=(name) [result=(rtype)] [width=(value)] line=(line)"

description:"r.profile outputs raster map layer values lying on user-defined lines."

help:"This program allows you to output raster map layer values lying on user-defined lines."
helpwidgetref:"14.reports/14.01.intro:Report Generation"

{

    parameter name
	description:"Raster map to be queried:"
	type:database_element:raster
	optional:false
	input:true
	help:"Enter the name of the raster map to be queried."
	;

    parameter line
	description:"Geographic coordinates:"
	type:character
	optional:false
	help:"Enter the geographic coordinates of the starting and ending points that define each profile line, given as easting and northing coordinate pairs."
	;

    dialog "Result"
	description:"Result"
    {
    parameter rtype
	description:"Type of result to be output:"
	type:enum:"raw,median,average"
	default:"raw"
	optional:true
	help:"Select the type of result to be output."
	;
    }

    dialog "Width"
	description:"Width"
    {
    parameter value
	description:"Profile width in cells:"
	type:enum:"1,3,5,7,9,11,13,15"
	default:"1"
	optional:true
	help:"Select the profile width."
	;
    }
}
