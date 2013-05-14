function [all_theta] = oneVsAll(X, y, num_labels, lambda)
%ONEVSALL trains multiple logistic regression classifiers and returns all
%the classifiers in a matrix all_theta, where the i-th row of all_theta 
%corresponds to the classifier for label i
%   [all_theta] = ONEVSALL(X, y, num_labels, lambda) trains num_labels
%   logisitc regression classifiers and returns each of these classifiers
%   in a matrix all_theta, where the i-th row of all_theta corresponds 
%   to the classifier for label i

% Some useful variables
m = size(X, 1);
n = size(X, 2);

K = num_labels;

all_theta = zeros(K, n + 1);


% Add ones to the X data matrix
X = [ones(m, 1) X];

% ====================== YOUR CODE HERE ======================
% Instructions: You should complete the following code to train num_labels
%               logistic regression classifiers with regularization
%               parameter lambda. 
%

options = optimset('GradObj', 'on', 'MaxIter', 50);
initial_theta = zeros(n + 1, 1);

for i = 1:K,
	
	[theta] = ...
		fmincg(@(t)(lrCostFunction(t, X, y == i, lambda)), initial_theta, options);

	all_theta(i, :) = theta';

end;



% =========================================================================


end
