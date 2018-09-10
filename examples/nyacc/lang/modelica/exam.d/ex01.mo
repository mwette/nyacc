/* ex01.mo
 *
 * Copyright (C) 2015 Matthew R. Wette
 * 
 * Copying and distribution of this file, with or without modification,
 * are permitted in any medium without royalty provided the copyright
 * notice and this notice are preserved.  This file is offered as-is,
 * without any warranty.
 */

package Demo

  model Circ
    Ground gnd;
    Load load;
    Resistor res;
  equation
    connect(load.p, gnd.p);
    connect(res.p, gnd.p);
  end Circ;

end Demo;
