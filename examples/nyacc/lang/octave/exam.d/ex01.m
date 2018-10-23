function osig = ex01(state, isig)
% PID -
% more docs

  global param
  state = [ 1.0, 2.0 ]';
  Kp = 1.0; Ki = 2.0; Kd = 3.0; dt = 1.0;
  desc = 'hello, world';
  A = [ 0.0, 1.0; -2.0, -3.0 ];

  state(0) = state(0) + dt*isig;
  deriv = (isig - state(1))/dt;
  state(1) = isig;
  osig = Kp*isig + Ki*state(0) + Kd*deriv;
  for i = 1:5
    % increment
    k = k + 1;
  end
  return;
end
