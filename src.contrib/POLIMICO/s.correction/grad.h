double gradInterpolate(double x,	//------X osservata
		       double y,	//------Y osservata
		       double deltaX,	//------passo spline direzione E
		       double deltaY,	//------passo spline direzione N
		       int xNum,	//------numero di spline in direz E
		       int yNum,	//------numero di spline in direz N
		       double xMin,	//------limite Est della regione
		       double yMin,	//------limite Sud della regione
		       double *parVect)	//------vettore dei parametri stimati dall'interp. bilineare
