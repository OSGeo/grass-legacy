program:"v.in.arc"

title:"Convert ARC/INFO Data to GRASS Vector"

commandstring:"[-(n)] type=(typename) lines_in=(lname) [points_in=(pname)] [text_in=(textname)] vector_out=(vname) [idcol=(ivalue)] [catcol=(cvalue)] [attcol=(avalue)]"

description:"v.in.arc converts data in ARC/INFO format to GRASS's vector format, and stores output in the user's current GRASS mapset."

help:"This program allows you to convert data in ARC/INFO format to GRASS's vector format, and stores output in the user's current GRASS mapset."
helpwidgetref:"08.mapdev/08.05.im.ex:Importing and Exporting Data"

{
    parameter typename
	description:"Coverage type:"
	type:enum:"polygon,line"
	help:"Select the coverage type."
	optional:false
	input:true
	;

    parameter lname
	description:"ARC/INFO ungenerate lines file:"
	type:character
	help:"Enter the ARC/INFO ungenerate format file containing line or polygon coordinates."
	optional:false
	input:true
	;

    parameter vname
	description:"Resultant GRASS vector output file:"
	type:character
	help:"Enter the name of the resultant GRASS vector output file."
	optional:false
	input:true
	;

    dialog "Files"
	description:"Files"
    {
    parameter pname
	description:"ARC/INFO ungenerate label-points file:"
	type:character
	help:"Enter the ARC/INFO ungenerate format file containing label-point coordinates."
	optional:true
	input:true
	;
    parameter textname
	description:"ARC/INFO ungenerate label-text file:"
	type:character
	help:"Enter the ARC/INFO ungenerate format file containing category numbers and (optionally) attribute text."
	optional:true
	input:true
	;
    }

    dialog "Colums"
	description:"Columns"
    {
    parameter ivalue
	description:"ID number column in label-text file:"
	type:character
	help:"Enter the ID number column in label-text file."
	optional:true
	input:true
	;
    parameter cvalue
	description:"GRASS category column in label-text file:"
	type:character
	help:"Enter the GRASS category column in label-text file."
	optional:true
	input:true
	;
    parameter avalue
	description:"GRASS attribute column in label-text file:"
	type:character
	help:"Enter the GRASS attribute column in label-text file."
	optional:true
	input:true
	;
    }

    dialog "Flag"
	description:"Flag"
    {
    flag n
	key:"n"
	description:"Neatline."
	help:"Click this button to use the neatline feature." 
	;
    }
}
