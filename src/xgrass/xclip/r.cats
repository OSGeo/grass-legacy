program:"r.cats"

title:"Print Category Values and Labels"

commandstring:"map=(name) [cats=(range)] [fs=\"(fs1)(fs2)(fs3)\"]"

description:"r.cats prints category values and labels associated with user-specified raster map layers."

help:"This program allows you to print category values and labels associated with user-specified raster map layers."
helpwidgetref:"14.reports/14.01.intro:Report Generation"
capture:true

{
    parameter name
	description:"Raster map layer:"
	type:database_element:raster
	input:true
	help:"Enter the name of the raster map layer to used as input."
	optional:false
	;

   parameter range
	description:"Range:"
	type:character
	input:true
	help:"Enter the range."
	optional:true
	;

    parameter fs1
	description:"Field separator (one character only)"
	type:character
	input:false
	optional:true
	help:"Enter a character to be used to separate the output fields."
	precludes:flag:fs2
	precludes:flag:fs3
	;

    flag fs2
	key:"space"
	description:"Use a space as the field separator"
	help:"Select this toggle to use a space as the field separator."
	precludes:parameter:fs1
	precludes:flag:fs3
	;

    flag fs3
	key:"tab"
	description:"Use a tab as the field separator"
	help:"Select this toggle to use a tab as the field separator."
	precludes:parameter:fs1
	precludes:flag:fs2
	;
}
