program:"i.tape.mss.h"

title:"Extract Header Information from LANDSAT MSS"

commandstring:"(tape_drive_name) [>(file_name)]"

description:"i.tape.mss.h extracts header information from LANDSAT Multispectral Scanner (MSS) imagery data stored on half-inch tape."

help:"This program allows you to extract header information from LANDSAT Multispectral Scanner (MSS) imagery data stored on half-inch tape."
helpwidgetref:"09.imagery/09.02.extract:Tape Extraction"

{
    parameter tape_drive_name
	description:"Input file:"
	type:character
	help:"Enter the name of the tape drive containing the desired data."
	optional:false
	input:true
	;

    dialog "Redirection"
	description:"Redirection"
    {
    parameter file_name
	description:"Output file:"
	type:character
	help:"Enter the name of the output file."
	optional:true
	input:true
	;
    }
}
