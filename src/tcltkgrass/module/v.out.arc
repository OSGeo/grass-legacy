interface_build {
    {v.out.arc} 0
    {Converts data in GRASS's vector format to ARC/INFO format, and stores output in the user's current GRASS mapset/arc subdirectory.}
    {entry vect {GRASS vector file (input):} 0 vector}
    {entry arc_prefix {Prefix for ARC/INFO output filenames:} 0 file}
    {entry separator {Output field separator [space]:} 0 ""}
    {checkbox type {Polygon coverage.} "" polygon}
    {checkbox type {Line coverage.} "" line}
}
