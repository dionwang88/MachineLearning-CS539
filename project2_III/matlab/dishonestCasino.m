% the dishonest casino example
states = ['F';'L'];
symbols = ['h';'t'];
trans = [0.95 0.05; 0.1 0.9];
emis = [0.5 0.5;0.1 0.9];

% generate a sequence
[oseq,hseq] = hmmgenerate(50,trans,emis,'Symbols',symbols,'Statenames',states);
fprintf(1,'Observed sequence     = %s\n',oseq);
fprintf(1,'Hidden state sequence = %s\n',hseq);

% posterior decoding
[pstates,logpseq,fwd,bkwd,S] = hmmdecode(oseq,trans,emis,'Symbols',symbols);
for i = 1:length(oseq)
   if pstates(1,i)>pstates(2,i)
      pseq(i)=1;
   else
      pseq(i)=2;
   end
end
fprintf(1,'Posterior decoding    = %s\n',states(pseq));
% viterbi decoding
vseq = hmmviterbi(oseq,trans,emis,'Statenames',states,'Symbols',symbols);
fprintf(1,'Viterbi decoded seq   = %s\n',vseq);