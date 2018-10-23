class Pendulum "Planar Pendulum"
  constant Real PI=3.141592653589793;
  parameter Real m=l, g=9.81, L=0.5;
  Real F;
  output Real x(start=0.5), y(start=0);
  output Real vx, vy;
equation
  m*der(vx) = -(x/L)*F;
  m*der(vy) = -(y/L)*F - m*g;
  der(x) = vx;
  der(y) = vy;
  x^2 + y^2 = L^2;
end Pendulum;
