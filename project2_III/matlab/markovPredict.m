function pred  = markovPredict( init, trans, t )
%markovPredict takes 
% init: a probability vector describing initial state
% trans: state transition matrix
% t: the number of time steps ahead to predict
% and produces pred: probability vector describing state at time t

pred = init;
for i = 1:t
    pred = pred * trans;
end
end

