
function [crossmatrix,precision] = validateClass(validation, m, s, P_hat)
    crossmatrix = zeros(3,3);
    precision = [0,0,0];
    for i = 1:length(validation)
       x = validation(i,1);
       c_predict = classify(x, m, s, P_hat);
       c_actual = 0;
       for j = 2:4
           if validation(i,j) == 1
               c_actual = j-1;
           end
       end
       crossmatrix(c_predict, c_actual) = crossmatrix(c_predict, c_actual) + 1; 
    end
    
    for i = 1:3
        precision(i) = crossmatrix(i,i)/sum(crossmatrix(i,:));
    end
end

