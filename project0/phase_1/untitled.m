opttra = csvread('optdigits.tra');
opttes = csvread('optdigits.tes');

train = opttra(:,1:64);
test = opttes(:,1:64);
cl = opttra(:,65);

k = 10;
Sigma = {'diagonal','full'};
nSigma = numel(Sigma);
SharedCovariance = {true,false};
SCtext = {'true','false'};
nSC = numel(SharedCovariance);

options = statset('MaxIter',1000); % Increase number of EM iterations

for i = 1:nSigma;
    for j = 1:nSC;
        gmfit = fitgmdist(train,k,'CovarianceType',Sigma{i},'SharedCovariance',SharedCovariance{j},'Options',options);
        clusterX = cluster(gmfit,train);
        [aic,bic] = aicbic(clusterX,length(train),length(train));
        aic(1,:)
        bic(1,:)
    end
end

gmfit = fitgmdist(train,k,'CovarianceType','diagonal','SharedCovariance',true,'Options',options);
cluster1 = cluster(gmfit,train);
[aic,bic] = aicbic(cluster1,length(train),length(train));
aic(1,:)
bic(1,:)
