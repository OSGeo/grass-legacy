interface_build {
    {v.in.arc} 0
    {Converts data in ARC/INFO format to GRASS's vector format, and stores output in the user's current GRASS mapset.}
    {entry lines_in {ARC/INFO ungenerate lines file:} 0 file}
    {entry points_in {ARC/INFO ungenerate label-points file:} 0 file}
    {entry text_in {ARC/INFO ungenerate label-text file:} 0 file}
    {entry vector_out {Resultant GRASS vector output file:} 0 vector}
    {entry idcol  {Number of label-text column containing line-ID numbers.} 0 ""}
    {entry catcol {Number of label-text column containing category values.} 0 ""}
    {entry attcol {Number of label-text column containing attribute text.}  0 ""}
    {checkbox -n {Neatline.} "" -n}
    {checkbox type {Polygon coverage.} "" polygon}
    {checkbox type {Line coverage.} "" line}
}
