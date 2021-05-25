

%{
/* C decl for simp4. */
#include "mlang.h"
void simp4(ml_dvec_t a, ml_dvec_t *x);
%}

function x = simp4(a)
  x = a(:);
end
