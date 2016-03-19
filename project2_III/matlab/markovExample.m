% a simple markov chain example
states = ['a' 'b' 'c']';
init = [0.5 0.5 0.3];
trans = [0.4 0.3 0.3; 0.2 0.6 0.2; 0.1 0.1 0.8];
for i = 1:100
    [s,ss] = markovGenerate(1000,init,trans,states);
    [lik,loglik] = calcLikelihood(s,init,trans);
    fprintf(1,'sequence = %s, lik = %.4f, loglik = %.4f \n',ss,lik,loglik);
end

pred = markovPredict(init, trans, 1000);
