program:"r.in.ll"

title:"Convert Raster Data to UTM-referenced Map"

commandstring:"[-(s)] input=(iname) output=(oname) bpc=(value) corner=(corner),lat=(ldd)[:(lmm)[:(lss)]](ns),lon=(mdd)[:(mmm)[:(mss)]](ew) dimension=(rows),(cols) res=(latres),(lonres) spheroid=(sname)"

description:"r.in.ll converts raster data referenced using latitude and longitude coordinates to a UTM-referenced map layer in GRASS raster format."

help:"This program allows you to convert raster data referenced using latitude and longitude coordinates to a UTM-referenced map layer in GRASS raster format."
helpwidgetref:"08.mapdev/08.05.im.ex:Converting, Importing, and Exporting"

{

    parameter iname
	description:"Name of existing raster map layer:"
	type:database_element:raster
	optional:false
	input:true
	help:"Enter the name of an existing raster map layer."
	;

    parameter oname
	description:"Name of output raster map layer:"
	type:character
	optional:false
	help:"Enter the name of the output raster map layer."
	;

    parameter value
	description:"Number of bytes per cell:"
	type:character
	optional:false
	help:"Enter the number of bytes per cell."
	;

    dialog "Corner"
	description:"Corner"
    {
    parameter corner
	description:"Corner of the input:"
	type:enum:"ne,se,sw,nw"
	optional:false
	help:"Enter the corner of the input."
	;
    }

    dialog "Latitude"
	description:"Latitude"
    {
    parameter ldd
	description:"Latitude Degrees"
	type:integer:"0:90:0"
	optional:false
	default:"0"
	;
    parameter lmm
	description:"Latitude Minutes"
	type:integer:"0:60:0"
	optional:true
	;
    parameter lss
	description:"Latitude Seconds"
	type:integer:"0:60:0"
	optional:true
	;
    parameter ns
	description:"North/South"
	type:logical:"N:S"
	optional:false
	default:true
	;
    }

    dialog "Longitude"
	description:"Longitude"
    {
    parameter mdd
	description:"Longitude Degrees"
	type:integer:"0:90:0"
	optional:false
	default:"0"
	;
    parameter mmm
	description:"Longitude Minutes"
	type:integer:"0:60:0"
	optional:true
	;
    parameter mss
	description:"Longitude Seconds"
	type:integer:"0:60:0"
	optional:true
	;
    parameter ew
	description:"East/West"
	type:logical:"E:W"
	optional:false
	default:true
	;
    }

    dialog "Rows"
	description:"Rows"
    {
    parameter rows
	description:"Number of rows in the input file:"
	type:character
	optional:false
	help:"Enter the number of rows in the input file."
	;
    }

    dialog "Columns"
	description:"Columns"
    {
    parameter cols
	description:"Number of columns in the input file:"
	type:character
	optional:false
	help:"Enter the number of columns in the input file."
	;
    }

    dialog "Resolution"
	description:"Resolution"
    {
    parameter latres
	description:"Latitude resolution:"
	type:integer:"0:10:5"
	optional:false
	help:"Select the proper latitude resolution."
	;
    parameter lonres
	description:"Longitude resolution:"
	type:integer:"0:10:5"
	optional:false
	help:"Select the proper longitude resolution."
	;
    }

    dialog "Spheroid"
	description:"Spheroid"
    {
    parameter sname
	description:"Spheroid:"
	type:enum:"airy,australian,bessel,clark66,everest,grs80,hayford,international,krasovsky,wgs66,wgs72,wgs84"
	optional:false
	;
    }

    dialog "Flag"
	description:"Flag"
    {
    flag s
	key:"s"
	description:"Signed data (high bit means negative value)."
	help:"Click this button to get signed data." 
	;
    }
}
