program:"v.out.arc"

title:"Convert GRASS Vector Files to ARC/INFO"

commandstring:"type=(tname) vect=(vname) arc_prefix=(aname)"

description:"v.out.arc converts GRASS vector files to ARC/INFO's \"Generate\" file format."

help:"This program allows you to convert GRASS vector files to ARC/INFO's \"Generate\" file format."
helpwidgetref:"08.mapdev/08.05.im.ex:Importing and Exporting Data"

{
    parameter tname
	description:"Coverage (feature) type:"
	type:enum:"polygon,line"
	help:"Select the coverage (feature) type."
	optional:false
	input:false
	;

    parameter vname
	description:"GRASS vector file to be converted to ARC/INFO format:"
	type:database_element:vector
	help:"Enter the name of the GRASS vector file to be converted to ARC/INFO format."
	optional:false
	input:true
	;

    parameter aname
	description:"Prefix to be assigned to the ARC/INFO format files:"
	type:character
	help:"Enter the prefix to be assigned to the ARC/INFO format files."
	optional:false
	input:true
	;
}
