function  ind = rGenerate(ddist)
 r = rand();
 rsum = 0;
 for i = 1:length(ddist)
    rsum = rsum + ddist(i);
    fprintf(1,'rsum= %.4f ddist = %.4f, r = %.4f i = %d \n',rsum,ddist(i),r,i);
    if rsum >= r
       ind = i;
       return 
    end
 end    
 ind = i
end