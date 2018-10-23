// ex02.mo

model MovingMass1		"moving mass"
  parameter Real m = 2		"mass of block";
  parameter Real f = 6		"force";
  Real s			"position";
  Real v			"velocity";
equation
  v = der(s);
  m*der(v) = f;
end MovingMass1;

// --- last line ---

