function osig = ex03b(state, isig)
% PID controller
  state.x(0) = state.x(0) + dt*isig;
end
