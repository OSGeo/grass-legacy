#
# Program : gis_intro.tcl
# 
#


proc copyrightWindow { the_message } {

  # Window manager configurations

  wm geometry . +100+100
  wm title . {Copyright}


  # ---------------------------
  # build .frame0
  # ---------------------------

  frame .frame0 \
    -borderwidth 0 \
    -relief raised

  message .frame0.message \
    -relief flat \
    -anchor nw \
    -justify center \
    -text $the_message

  pack append .frame0 \
    .frame0.message { top fill expand }


  # ---------------------------
  # build .frame1
  # ---------------------------

  frame .frame1 \
    -borderwidth 0 \
    -relief raised

  button .frame1.button \
    -text "OK" \
    -padx 10 \
    -command "destroy ."

  pack append .frame1 \
    .frame1.button { top frame center }

  pack append . \
       .frame0 { top fill expand } \
       .frame1 { bottom frame center expand } 


  bind . <Return> {
     eval .frame1.button invoke
  }

  grab .
  tkwait window . 
}


copyrightWindow {
TCL/TK GRASS
An open graphical user interface for GRASS



developed by

L.A.S.
Logiciels et Applications Scientifiques Inc.
Montreal, Canada
tel.: (514) 858-1104
fax: (514) 389-9373
email: gc@copernic.lasinc.qc.ca



funding for the development of this interface:

Hydro-Quebec 
Vice-presidence Environnement
Montreal, Canada
scientific authority: Laurent Girouard and Guy Moisan

National Defense Canada
Defense Research Establishment Valcartier
Courcelette, Canada
scientific authority: Marc Parenteau and Dominic Roy

Natural Resources Canada
Petawawa National Forestry Institute
Chalk River, Canada
scientific authority: Thomas Moore
}

