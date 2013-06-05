function [C, sigma] = dataset3Params(X, y, Xval, yval)
%EX6PARAMS returns your choice of C and sigma for Part 3 of the exercise
%where you select the optimal (C, sigma) learning parameters to use for SVM
%with RBF kernel
%   [C, sigma] = EX6PARAMS(X, y, Xval, yval) returns your choice of C and 
%   sigma. You should complete this function to return the optimal C and 
%   sigma based on a cross-validation set.
%

C_pos = [0.01 0.03 0.1 0.3 1 3];% 10 30];
sigma_pos = [0.01 0.03 0.1 0.3 1 3];% 10 30] ;

% You need to return the following variables correctly.
C = 1;
sigma = 0.3;

% ====================== YOUR CODE HERE ======================
% Instructions: Fill in this function to return the optimal C and sigma
%               learning parameters found using the cross validation set.
%               You can use svmPredict to predict the labels on the cross
%               validation set. For example, 
%                   predictions = svmPredict(model, Xval);
%               will return the predictions on the cross validation set.
%
%  Note: You can compute the prediction error using 
%        mean(double(predictions ~= yval))
%

best = Inf;

for next_C = C_pos,
	for next_sigma = sigma_pos,
		fprintf('Trying C=%f and sigma=%f\n', next_C, next_sigma);

		model = svmTrain(X, y, next_C, @(x1, x2) gaussianKernel(x1, x2, next_sigma)); 

		pred = svmPredict(model, Xval);
		current = mean(double(pred ~= yval));

		if (best > current),
			C = next_C;
			sigma = next_sigma;
			best = current;
			fprintf('Found new best C=%f and sigma=%f, value is %f\n', C, sigma, best);
		end;
	end;
end;

% C = 1.0;
% sigma = 0.1;

fprintf('The best C=%f and sigma=%f\n', C, sigma);

% =========================================================================

end
