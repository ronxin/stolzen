function [J, grad] = lrCostFunction(theta, X, y, lambda)
%LRCOSTFUNCTION Compute cost and gradient for logistic regression with 
%regularization
%   J = LRCOSTFUNCTION(theta, X, y, lambda) computes the cost of using
%   theta as the parameter for regularized logistic regression and the
%   gradient of the cost w.r.t. to the parameters. 

m = length(y); % number of training examples

h_x = sigmoid(X * theta);

J_norm = -(y' * log(h_x) + (1 - y)' * log(1 - h_x)) / m;
J = J_norm + lambda / (2 * m) * sum(theta(2:end) .^ 2);

grad_norm = X' * (h_x - y) ./ m;
reg_term = lambda / m .* theta;
reg_term(1) = 0;
grad = grad_norm + reg_term;

grad = grad(:);

end
