p = [1 0];
a = [0.9 0.1; 0.5 0.5];
for i = 1:20
    fprintf(1,'Day %d: P(w=sunny)=%.4f P(w=rainy)=%.4f',i,p(1),p(2));
    fprintf(1,'\n');
    p = p * a;
end

