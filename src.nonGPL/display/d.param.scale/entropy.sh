r.mapcalc << EOF
pplne = (feat9 ==0) + (feat15==0) + (feat21 == 0) + (feat27 == 0) + (feat33 == 0) + (feat45 == 0)
ppit  = (feat9 ==1) + (feat15==1) + (feat21 == 1) + (feat27 == 1) + (feat33 == 1) + (feat45 == 1)
pchan = (feat9 ==2) + (feat15==2) + (feat21 == 2) + (feat27 == 2) + (feat33 == 2) + (feat45 == 2)
ppass = (feat9 ==3) + (feat15==3) + (feat21 == 3) + (feat27 == 3) + (feat33 == 3) + (feat45 == 3)
pridg = (feat9 ==4) + (feat15==4) + (feat21 == 4) + (feat27 == 4) + (feat33 == 4) + (feat45 == 4)
ppeak = (feat9 ==5) + (feat15==5) + (feat21 == 5) + (feat27 == 5) + (feat33 == 5) + (feat45 == 5)
entrop = 100*((pplne/6.0)*log(.001 + pplne/6.0)+ (ppit/6.0)*log(.001 + ppit/6.0) + \
	      (pchan/6.0)*log(.001 + pchan/6.0)+(ppass/6.0)*log(.001 + ppass/6.0)+ \
	      (pridg/6.0)*log(.001 + pridg/6.0)+(ppeak/6.0)*log(.001 + ppeak/6.0)) \
	  / log(1.0/6.0)
mode = 	(max(pplne,ppit,pchan,ppass,pridg,ppeak)==pplne)*0 + \
	(max(pplne,ppit,pchan,ppass,pridg,ppeak)==ppit)*1 + \
	(max(pplne,ppit,pchan,ppass,pridg,ppeak)==pchan)*2 + \
	(max(pplne,ppit,pchan,ppass,pridg,ppeak)==ppass)*3 + \
	(max(pplne,ppit,pchan,ppass,pridg,ppeak)==pridg)*4 + \
	(max(pplne,ppit,pchan,ppass,pridg,ppeak)==ppeak)*5
EOF
