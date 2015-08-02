// ex1.mo

package Demo

  model Circuit
    Ground ground;
    Load load;
    Resistor resistor;
  equation
    connect(load.p, ground.p);
    connect(resistor.p, ground.p);
  end Circuit;

end Demo;
