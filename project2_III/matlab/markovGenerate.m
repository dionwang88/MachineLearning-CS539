function [s,ss] = markovGenerate(len,init,trans,states )
%markovGenerate takes
% l: length of desired sequence
% init: probability vector
% trans: the transition probability matrix
% states: list of names of the states
% and produces a sequence of l states

% generate an initial state based on init
s = zeros(len,1);                       
s(1) = rGenerate(init);
for i = 2:len
    s(i) = rGenerate(trans(s(i-1),:));
end
ss=states(s);
ss = ss';
end

function  ind = rGenerate(ddist)
 r = rand();
 rsum = 0;
 for i = 1:length(ddist)
    rsum = rsum + ddist(i);
    %fprintf(1,'rsum= %.4f ddist = %.4f, r = %.4f i = %d \n',rsum,ddist(i),r,i);
    if rsum >= r
       ind = i;
       return 
    end
 end    
 ind = i
end
