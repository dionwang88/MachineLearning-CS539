function plot_posterior(xval , m, s, P_hat)
    yval = zeros(length(xval),length(xval),length(xval));
    for i = 1:length(xval)
        px = 0.0;
        for j = 1:length(m)
            px = px + normpdf(xval(i),m(j),s(j))*P_hat(j);
        end
        for j = 1:length(m)
           yval(i,j) =  normpdf(xval(i),m(j),s(j))*P_hat(j)/px;
        end
    end
    plot(xval, yval(:,1),xval, yval(:,2),xval, yval(:,3));
end
