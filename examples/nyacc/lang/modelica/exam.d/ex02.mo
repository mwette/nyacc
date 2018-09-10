// Lecture 1 demo

model MovingMass1
  parameter Real m = 2;
  parameter Real f = 6;
  Real s;
  Real v;
equation
  v = der(s);
  m*der(v) = f;
end MovingMass1;

// --- last line ---

