void pntAssemble( lineList *l1, pntDictionary *pd0 ) {
  pntDictionary *newDict1, *newDict2;
  int i0, j0, k0, pntCount, totalpnts, lCount;
  int first, second, validpnts, nodepnts;
  int isDifferent;
  pntCount = 0;
  /* Count the points. Is it more efficient to get this directly from
     the shape file and import the result?
  */
  totalpnts = 0;
  for(i0 = 0; i0 < l1->numLines; ++i0 ) {
    for(j0 = 0 ; j0 < l1->lines[i0].numParts; ++j0 ) {
      for( k0 = 0; k0 < l1->lines[i0].parts[j0].numPoints; ++k0 ) {
	totalpnts++;
      }
    }
  }
  /* Write all the points */
  pd0->points = (pntDescript *)malloc( totalpnts * sizeof(pntDescript));
  for(i0 = 0; i0 < l1[i0].numLines; ++i0 ) {
    for(j0 = 0 ; j0 < l1->lines[i0].numParts; ++j0 ) {
      for( k0 = 0; k0 < l1->lines[i0].parts[j0].numPoints; ++k0 ) {
	pd0->points[pntCount] = l1->lines[i0].parts[j0].linepnts[k0];
	/* If it's at the start or end of a part it's a node */
	if( k0 == 0 || k0 == l1->lines[i0].parts[j0].numPoints - 1 ) pd0->points[pntCount].isnode = 1;
	pntCount++;
      }
    }
  }
  /* Sort the points N->S then W->E */
  qsort( pd0->points, pntCount, sizeof( pntDescript ), pntsort ); 
  /* Replace this - it is GNU specific   */
  /* qsort_points( pd0->points, pntCount ); */
  newDict1 = (pntDictionary *)malloc( sizeof(pntDictionary) );
  newDict2 = (pntDictionary *)malloc( sizeof(pntDictionary) );
  newDict1->points = (pntDescript *)malloc( totalpnts * sizeof(pntDescript));
  /* Count duplicates, remove duplicates, and if count is >2 => it is a node */
  /* This catches all nodes except nodes on the world margin. Get them later */
  first = 0;
  validpnts = 0;
  while( first < totalpnts ) {
    if( first == totalpnts - 1 ) {
      newDict1->points[validpnts++] = pd0->points[totalpnts - 1];
      first++;
      continue;
    }
    else {
      isDifferent = 0;
      second = first;
      lCount = 0;
      while( isDifferent == 0 ) {
	second++;
	lCount++;
	isDifferent = pntsCoincide( &pd0->points[first], &pd0->points[second] );
      }
      newDict1->points[validpnts++] = pd0->points[first];
      if( lCount > 2 ) newDict1->points[validpnts].isnode = 1;
      first = second;
    }
  }
  /* Keep known nodes, discard the rext */
  newDict1->points = (pntDescript *)realloc( newDict1->points, 
						    validpnts * sizeof( pntDescript ) );

  free( pd0->points );
  newDict2->points = ( pntDescript *)malloc( validpnts * sizeof(pntDescript) );
  
  nodepnts  = 0;
  for( i0 = 0; i0 < validpnts; ++i0 ) {
    if( newDict1->points[i0].isnode == 1 )
      newDict2->points[nodepnts++] = newDict1->points[i0];
  }

  pd0->points = (pntDescript *)realloc(newDict2->points, 
					      nodepnts * sizeof( pntDescript ) );
  pd0->Size = nodepnts;

  free( newDict1->points );
  free( newDict1 );
  free( newDict2->points );
  free( newDict2 );

}


/* +++++++++++++++++++++++++++++++++++++ */

/* Segment from shp2dig using remind* flags: */

      /* Is an intersect to be determined? - if so determine it */
      if( i0 == 0 && fabs( posvecy_old ) < HORIZON_WIDTH ) {
	if( posvecy > 0 ) remindEnd = 1; /* heading North */
	else remindEnd = -1;             /* heading South */
      }
      else if( i0 == partd->numPoints - 2 && remindEnd == 1 ) {
	if( remindDirect == 1 || posvecy_old < 0 ) {
	  /* Got an intersect. It is the first (last) point */
	  pd0[nIntersects].duff = 0;
	  pd0[nIntersects].isnode = 0;
	  pd0[nIntersects].xPosn = partd->linepnts[0].xPosn;
	  pd0[nIntersects].yPosn = partd->linepnts[0].yPosn;
	  nIntersects++;
	}
      }
      else if( i0 == partd->numPoints - 2 && remindEnd == -1 ) {
	if( remindDirect == -1 || posvecy_old > 0 ) {
	  /* Got an intersect. It is the first (last) point */
	  pd0[nIntersects].duff = 0;
	  pd0[nIntersects].isnode = 0;
	  pd0[nIntersects].xPosn = partd->linepnts[0].xPosn;
	  pd0[nIntersects].yPosn = partd->linepnts[0].yPosn;
	  nIntersects++;
	}
      }
      else if( !remindDirect && fabs( posvecy ) < HORIZON_WIDTH ) {
	if( posvecy > 0 ) remindDirect = 1; /* heading North */
	else remindDirect = -1;             /* heading South */
      }
      else if( remindDirect && fabs( posvecy ) < HORIZON_WIDTH ) {
	/* Do nothing */
      }
      else if( (remindDirect == -1 && posvecy < 0) 
	       || (remindDirect == 1 && posvecy > 0) ) { 
	/* Found an intersect. It's equal to the current base point */

	/* Unset flag */
	remindDirect = 0;

	/* Register intersect */
	pd0[nIntersects].duff = 0;
	pd0[nIntersects].isnode = 0;
	pd0[nIntersects].xPosn = partd->linepnts[i0].xPosn;
	pd0[nIntersects].yPosn = partd->linepnts[i0].yPosn;
	/* Don't bother about M and Z */

	nIntersects++;
      }
      else if( (remindDirect == 1 && posvecy < 0) 
	       || (remindDirect == -1 && posvecy > 0) ) {
	/* It's reflected. Not an intersect, but unset flag */
	remindDirect = 0;
      }
