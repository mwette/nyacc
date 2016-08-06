% ex03a.m - cover file

param = struct('Kp',1.0, 'Ki',2.0, 'Kd',0.1, 'dt',0.1); 
state = struct('x', zeros(2,1)); % comment
con = struct('param',param, 'state',state);

isig = 1.0;
%osig = ex03b(con, isig);

% --- last line ---
