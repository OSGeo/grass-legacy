interface_build {
    {s.sv} 0
    {Sample semivariagram for sites list.}
    {entry sites {Input sites list to use for semivariogram:} 0 sites}
    {entry lag {Nominal lag distance:} 0 ""}
    {entry lagtol {Tolerance on lag distance [default=1/2 normal distance):} 0 ""}
    {entry direction {Direction of semivariogram [default=omnidirectional]:} 0 ""}
    {entry angtol {Angular tolerance on direction:} 0 ""}
    {entry graph {Output results to GNUPlot *.gp and *.dat format (implies -p; print to screen if blank):} 0 file}
    {entry field {Number of sites list field to use for calculation [default=1]:} 0 ""}
    {checkbox -p {Save output to GNUplot files.} "" -p}
    {checkbox -q {Run quietly.} "" -q}
}
