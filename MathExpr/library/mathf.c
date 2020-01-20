
/**************************************************************
  MATHF.C
 **************************************************************/

#include <math.h>

//  обратные тригонометрические
double ZAcos ( double x ) { return( acos( x ) ); }
double ZAsin ( double x ) { return( asin( x ) ); }
double ZAtan ( double x ) { return( atan( x ) ); }
double ZActg ( double x ) { return( 0 /*???*/ ); }
//  тригонометрические
double ZCos  ( double x ) { return( cos( x ) ); }
double ZSin  ( double x ) { return( sin( x ) ); }
double ZTan  ( double x ) { return( tan( x ) );   }
//  котангенс
double ZCtg  ( double x ) { return( 0 /*???*/ );   }
//  гиперболические
double ZCosh ( double x ) { return( cosh( x ) ); }
double ZSinh ( double x ) { return( sinh( x ) ); }
double ZTanh ( double x ) { return( tanh( x ) );  }
//  гиперб. котангенс
double ZCth ( double x ) { return( 0 /*???*/ );  }
//  секанс
double ZSch ( double x ) { return( 0 /*???*/ );  }
//  косеканс
double ZCsch ( double x ) { return( 0 /*???*/ );  }
//  обратные гиперболические
//  ареа-синус
double ZArsh ( double x ) { return( 0 /*???*/ );  }
//  ареа-косинус
double ZArch ( double x ) { return( 0 /*???*/ );  }
//  ареа-тангенс
double ZArth ( double x ) { return( 0 /*???*/ );  }
//  ареа-котангенс
double ZArcth ( double x ) { return( 0 /*???*/ );  }
//  прочие
double ZSqrt ( double x ) { return( sqrt( x ) ); }

double ZExp  ( double x ) { return( exp( x ) ); }
double ZLog  ( double x ) { return( log( x ) ); }
double ZLog10( double x ) { return( log10( x ) ); }

double ZCeil ( double x ) { return( ceil( x ) );  }
double ZFabs ( double x ) { return( fabs( x ) );  }
double ZFloor( double x ) { return( floor( x ) ); }
//  возведение в степень
double ZPow  ( double x, double y ) { return( pow( x, y ) ); }

/*******************  END OF FILE MATHF.Y  ********************/

