function osig = ex03b(con, isig)
% PID controller

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
