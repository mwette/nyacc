function osig = ex03b(con, isig)
% PID controller

%~ con : struct
%~ isig: double 
%~ osig: double 

%% %#include "ex03b.h" 

  param = con.param;
  state = con.state;
  Kp = param.Kp;
  Ki = param.Ki;
  Kd = param.Kd;
  dt = param.dt;

  state(0) = state(0) + dt*isig;
  deriv = (isig - state(1))/dt;
  state(1) = isig;
  osig = Kp*isig + Ki*state(0) + Kd*deriv;
end
