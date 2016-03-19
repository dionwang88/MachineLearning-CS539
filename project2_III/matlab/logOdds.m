function lodds = logOdds(s,trans_a,trans_b)
lodds = 0;
for i = 2:length(s)
    lodds = lodds + log(trans_a(s(i-1),s(i))/trans_b(s(i-1),s(i)));
end