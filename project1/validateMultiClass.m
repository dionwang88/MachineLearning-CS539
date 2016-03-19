function [crossmatrix,accuracy] = validateMultiClass(x,all_samples)
    crossmatrix = zeros(2,2);
    accuracy = [0,0];
    for row = 1:length(x)
        c_predict = multivariantClassify(x(row,1:20),all_samples);
        for col = 21:22
           if x(row,col) == 1
               c_actual = col - 20;
           end
        end
        crossmatrix(c_predict, c_actual) = crossmatrix(c_predict, c_actual) + 1;
    end
    
    for i = 1:length(accuracy)
        accuracy(i) = crossmatrix(i,i)/ sum(crossmatrix(i,:));
    end
end

