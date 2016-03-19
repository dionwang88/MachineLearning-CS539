function [sampleMean, sampleVar] = estimateMultivariate(sample)
    sampleMean = zeros(1,20);
    sampleVar = zeros(20,20);
    N = length(sample);
    for i = 1:20
       sampleMean(i) = sum(sample(:,i))/N;
    end
    for i = 1:20
       for j = 1:20
          sampleVar(i,j) = sum((sample(:,i) - sampleMean(i)).*(sample(:,j) - sampleMean(j)))/N; 
       end
    end
end