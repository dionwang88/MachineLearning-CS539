% a simple CpG Markov model
states = ['A';'C';'G';'T'];
init_cpg = [0.25 0.25 0.25 0.25];
trans_cpg = [0.18 0.274 0.426 0.120;0.171 0.368 0.274 0.188;
    0.161 0.339 0.375 0.125; 0.079 0.355 0.384 0.182];
[s_cpg,ss_cpg] = markovGenerate(20,init_cpg,trans_cpg,states);
[lik_cpg,loglik_cpg] = calcLikelihood(s_cpg,init_cpg,trans_cpg);
fprintf(1,'CpG sequence = %s, loglik = %.4f \n',ss_cpg,loglik_cpg);

% a simple non CpG markov model
init_ncpg = [0.25 0.25 0.25 0.25];
trans_ncpg = [0.3 0.205 0.285 0.210;0.322 0.298 0.078 0.302;
    0.248 0.246 0.298 0.208; 0.177 0.239 0.292 0.292];
[s_ncpg,ss_ncpg] = markovGenerate(20,init_cpg,trans_cpg,states);
[lik_ncpg,loglik_ncpg] = calcLikelihood(s_ncpg,init_ncpg,trans_ncpg);
fprintf(1,'non CpG sequence = %s, loglik = %.4f \n',ss_ncpg,loglik_ncpg);

% log odds ratio
lodds1 = logOdds(s_cpg,trans_cpg,trans_ncpg);
lodds2 = logOdds(s_cpg,trans_ncpg,trans_cpg);
fprintf('Sequence %s log odds for CpG = %.4f log odds for nCpG = %.4f\n',ss_cpg,lodds1,lodds2);