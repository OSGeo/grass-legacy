/*-
 * Copyright (C) 1994. James Darrell McCauley.
 *
 * Author: James Darrell McCauley darrell@mccauley-usa.com
 * 	                          http://mccauley-usa.com/
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
 *
 * $Id$
 *
 * this program transform data from standard input in the
 * form of: x1 y1
 *          x2 y2
 *          x3 y3
 *          ...
 *          xn yn
 * where y is a cumulative frequency [0,1.0),
 * to a form suitable to making normal probability plots.
 * (That is, it computes the inverse from a normal curve,
 * transforming area under the curve to a z-value.)
 *
 * Some useful gnuplot commands for plotting the transformed
 * data are:
 *   set nozeroaxis
 *   set yrange [-3.5:3.5]
 *   set ytics ("1" -2.32647, "2" -2.05386, "5" -1.64494, \
 *         "10" -1.28165, "20" -0.841735, "50" 0, \
 *         "80" 0.841735, "90" 1.28165, "95" 1.64494, \
 *         "98" 2.05386, "99" 2.32647)
 */

#include<stdio.h>
#include<math.h>

#ifdef TESTING

main ()
{
  double x, f, p, inverse_normal ();

  while (scanf ("%lf %lf", &x, &p) != EOF)
    fprintf (stdout,"%g %g\n", x, inverse_normal (p));
}

#endif TESTING

double inverse_normal (p)
  double p;

/*-
 * Calculates the inverse of the standard Gaussian cumulative distribution
 * function, $\Phi^{-1}$. Let $y=-\ln\left(2p\right)\/$ and the following
 * approximation yields the upper-tail quantiles:
 *
 *
 * \begin{displaymath}
 * \Phi^{-1}\left(1-p\right) \approx
 * \sqrt{\frac{\left[\left(4y+100\right)y+205\right]y^2}
 *            {\left[\left(2y+56\right)y+192\right]y+131}}.
 * \end{displaymath}
 *
 * This reportedly has an error no larger than $1.3\times10^{-4}\/$ when
 * $10^{-7} < p < \frac{1}{2}.$ This approximation was taken from 
 * Derenzo~\cite{derenzo77} cited by Hoaglin~\cite{hoaglin85b}.
 */
{
  double y;

  if (p < 0.5 && p >= 0.0)
  {
    y = -log (2.0 * p);
    return -sqrt ((((4.0 * y + 100.0) * y + 205.0) * y * y)
		  / (((2 * y + 56.0) * y + 192.0) * y + 131.0));
  }
  else if (p >= 0.5 && p < 1.0)
  {
    y = -log (2.0 * (1.0 - p));
    return sqrt ((((4.0 * y + 100.0) * y + 205.0) * y * y) /
		 (((2 * y + 56.0) * y + 192.0) * y + 131.0));
  }
  else
    return -999;
}

double stdnorm (z)
  double z;
/*-
 * Calculates $p\/$ values from $z\/$ for standard Gaussian distribution
 * by letting $y=|z|/\sqrt{2}\/$ and using
 * \begin{displaymath}
 * \Phi\left(z\right)\approx
 * \frac{1}{2} +
 * \frac{1}{2} \left(1+c_1y+c_2y^2+c_3y^3+c_4y^4+c_5y^5\right)^{-8}
 * \end{displaymath}
 * where 
 * \begin{displaymath}
 * \mbox{\bf c}=\left[\begin{array}{r}
 *                     0.14112821 \\
 *                     0.08864027 \\
 *                     0.02743349 \\
 *                    -0.00039446 \\ 
 *                     0.00328975
 *                    \end{array}\right]. 
 * \end{displaymath}
 * Accuracy is unknown~\cite{groeneveld79}.
 */
{
  static double c[5] = {0.14112821, 0.08864027,
  0.02743349, -0.00039446, 0.00328975};
  double f, p;

  z = fabs (z) / sqrt (2.0);
  f = 1.0 - pow (1. + z * (c[0] + z * (c[1] + z *
		 (c[2] + z * (c[3] + z * c[4])))), -8.0);
  p = .5 + .5 * f;
  return p;
}
