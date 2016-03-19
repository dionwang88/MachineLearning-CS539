function [ c ] = multivariantClassify(x, sample)
    c = 0;
    C1 = sample(1:1000,1:20);
    C2 = sample(1001:1800,1:20);
    N1 = length(C1);
    N2 = length(C2);
    p1_hat = N1/(N1+N2);
    p2_hat = N2/(N1+N2);
    [m1, s1] = estimateMultivariate(C1);
    [m2, s2] = estimateMultivariate(C2);
    
    g(1) = multivarDiscriminant(x,m1,s1,p1_hat);
    g(2) = multivarDiscriminant(x,m2,s2,p2_hat);
    
    if g(1) > g(2)
        c = 1;
    else
        c = 2;
    end
end

function g = multivarDiscriminant(x,m,s,p_hat)
    g = -1/2*log(det(s)) - 1/2*(x-m)*inv(s)*(x-m)' + log(p_hat);
end

