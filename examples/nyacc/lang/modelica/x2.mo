type Voltage = Real;

connector Pin
  Voltage v;
  flow Current i;
end Pin;

partial class TwoPin
  Pin p, n;
  Voltage v;
  Current i;
equation
  v = p.v - n.v;
  0 = p.i + n.i;
  i = p.i;
end TwoPin;

class Resistor
  extends TwoPin;
  parameter Real R;
equation
  R*i = v;
end Resistor;

class Capacitor
  extends TwoPin;
  parameter Real C;
equation
  C*der(v) = i;
end Capacitor;

class Inductor
  extends TwoPin;
  parameter Real L;
equation
  v = L*der(i);
end Inductor;

class VsourceAC
  extends TwoPin;
  parameter Voltage VA;
  parameter Real f;
  constant Real PI = 3.14159;
equation
  v = VA*sin(2*PI*f*time);
end VsourceAC;

class Ground
  Pin p;
equation
  p.v = 0;
end Ground;

class SimpleCircuit
  Resistor R1(R=10);
  Capacitor C(C=0.01);
  Resistor R2(R=100);
  Inductor I(L=0.01);
  VsourceAC AC;
  Ground G;
equation
  connect(AC.p, R1.p);
  connect(R1.n, C.p);
  connect(C. n, AC.n);
  connect(R1. p, R2.p);
  connect(R2.n, L.p);
  connect(L.n, C.n);
  connect(AC.n, G.p);
end SimpleCircuit;

