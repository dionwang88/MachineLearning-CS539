function [lik,loglik]=calcLikelihood(s,init,trans)
% calculate likelihood of the sequence
lik = init(s(1));
for i = 2:length(s)
    lik = lik * trans(s(i-1),s(i));
end

%calculate log likelihood of the sequence
loglik =log(init(s(1)));
for i = 2:length(s)
    loglik = loglik + log(trans(s(i-1),s(i)));
end