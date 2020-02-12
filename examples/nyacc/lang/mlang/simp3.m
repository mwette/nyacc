% simp3.m - a simple demo file

%{
/* C decl's for mltoc */
double simp3(double a, double b);
static double inv(double x);
%}

function c = simp3(a, b)
% SIMP3 - illustrate basic function in mlang
% a: scalar
% b: scalar
% c: returned scalar sum
   ;
   % Just do something simple.
   c = a + b;
end

