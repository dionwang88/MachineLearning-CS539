function plot_likelihood( xval, m, s )
    Y = zeros(length(xval),length(xval),length(xval));
    for i = 1:3
        Y(:,i) = normpdf(xval,m(i),s(i));
    end
    plot(xval, Y(:,1),xval, Y(:,2),xval, Y(:,3));
end

