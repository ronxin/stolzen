function [J, grad] = linearRegCostFunction(X, y, theta, lambda)
%LINEARREGCOSTFUNCTION Compute cost and gradient for regularized linear 
%regression with multiple variables
%   [J, grad] = LINEARREGCOSTFUNCTION(X, y, theta, lambda) computes the 
%   cost of using theta as the parameter for linear regression to fit the 
%   data points in X and y. Returns the cost in J and the gradient in grad

% Initialize some useful values
m = length(y); % number of training examples

% You need to return the following variables correctly 
J = 0;
grad = zeros(size(theta));

% ====================== YOUR CODE HERE ======================
% Instructions: Compute the cost and gradient of regularized linear 
%               regression for a particular choice of theta.
%
%               You should set J to the cost and grad to the gradient.
%

h_x = X * theta;
J_norm = sum((h_x - y) .^ 2) / (2 * m);
J = J_norm + lambda / (2 * m) * sum(theta(2:end) .^ 2);


grad_norm = X' * (h_x - y) ./ m;
reg_term = lambda / m .* theta;
reg_term(1) = 0;
grad = grad_norm + reg_term;


% =========================================================================

grad = grad(:);

end
