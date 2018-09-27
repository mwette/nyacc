% nyacc/lang/octave/oct_02.m 

% Copyright (C) 2018 Matthew R. Wette
% 
% Copying and distribution of this file, with or without modification,
% are permitted in any medium without royalty provided the copyright
% notice and this notice are preserved.  This file is offered as-is,
% without any warranty.

function y = oct_02(x, f)
   y = x;
   if f == "f01"
      [m, n, o] = f01(x);
      y = m + n + o;
   end
end

function [d,e,f] = f01(x)
   d = x;
   e = x + 10;
   f = x + 100;
end

% --- last line ---
