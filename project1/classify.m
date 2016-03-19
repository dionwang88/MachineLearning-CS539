function [ g ] = classify( x, M, S, P_hat )
% Calculate the discriminant function to classify the input x
% based on the mean, standard deviation, and prior probability 
% getting from MLE
    for i = 1: length(M)
       m = M(i);s = S(i); p_hat = P_hat(i);
       g = -log(s) - ((x - m)^2/(2*s^2)) + log(p_hat);
       D(i) = g;
    end
    G = max(D);
    for i = 1:length(D)
       if G == D(i)
           g = i;
       end
    end
end
