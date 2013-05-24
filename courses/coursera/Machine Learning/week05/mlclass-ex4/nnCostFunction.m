function [J grad] = nnCostFunction(nn_params, ...
                                   input_layer_size, ...
                                   hidden_layer_size, ...
                                   num_labels, ...
                                   X, y, lambda)
%NNCOSTFUNCTION Implements the neural network cost function for a two layer
%neural network which performs classification
%   [J grad] = NNCOSTFUNCTON(nn_params, hidden_layer_size, num_labels, ...
%   X, y, lambda) computes the cost and gradient of the neural network. The
%   parameters for the neural network are "unrolled" into the vector
%   nn_params and need to be converted back into the weight matrices. 
% 
%   The returned parameter grad should be a "unrolled" vector of the
%   partial derivatives of the neural network.
%

% Reshape nn_params back into the parameters Theta1 and Theta2, the weight matrices
% for our 2 layer neural network
Theta1 = reshape(nn_params(1:hidden_layer_size * (input_layer_size + 1)), ...
                 hidden_layer_size, (input_layer_size + 1));

Theta2 = reshape(nn_params((1 + (hidden_layer_size * (input_layer_size + 1))):end), ...
                 num_labels, (hidden_layer_size + 1));

% Setup some useful variables
m = size(X, 1);
         
% You need to return the following variables correctly 
Theta1_grad = zeros(size(Theta1));
Theta2_grad = zeros(size(Theta2));

% ====================== YOUR CODE HERE ======================
% Instructions: You should complete the code by working through the
%               following parts.
%
% Part 1: Feedforward the neural network and return the cost in the
%         variable J. After implementing Part 1, you can verify that your
%         cost function computation is correct by verifying the cost
%         computed in ex4.m
%
% Part 2: Implement the backpropagation algorithm to compute the gradients
%         Theta1_grad and Theta2_grad. You should return the partial derivatives of
%         the cost function with respect to Theta1 and Theta2 in Theta1_grad and
%         Theta2_grad, respectively. After implementing Part 2, you can check
%         that your implementation is correct by running checkNNGradients
%
%         Note: The vector y passed into the function is a vector of labels
%               containing values from 1..K. You need to map this vector into a 
%               binary vector of 1's and 0's to be used with the neural network
%               cost function.
%
%         Hint: We recommend implementing backpropagation using a for-loop
%               over the training examples if you are implementing it for the 
%               first time.
%
% Part 3: Implement regularization with the cost function and gradients.
%
%         Hint: You can implement this around the code for
%               backpropagation. That is, you can compute the gradients for
%               the regularization separately and then add them to Theta1_grad
%               and Theta2_grad from Part 2.
%

% Feedforward

K = num_labels;

% J = 0;
% for j = 1:m,
%     y_i = zeros(1, K);
%     y_i(y(j)) = 1;

%     x_i = X(j, :);

%     a1 = [1; x_i'];
%     z2 = Theta1 * a1;
%     a2 = sigmoid(z2);

%     a2 = [1; a2];
%     z3 = Theta2 * a2;
%     h_x = sigmoid(z3);

%     cost = - y_i * log(h_x) - (1 - y_i) * log(1 - h_x); 

%     J = J + cost;
% end;

% J = J / m;

a1 = [ones(m, 1) X];
z2 = a1 * Theta1';
a2 = sigmoid(z2);

a2 = [ones(m, 1) a2];
z3 = a2 * Theta2';
h_x = sigmoid(z3);

y1 = eye(K)(y, :);

cost = - y1 .* log(h_x) - (1 - y1) .* log(1 - h_x);
cost = sum(sum(cost)) / m;

regularization = lambda * (sum(sum(Theta1(:, 2:end) .^ 2)) + sum(sum(Theta2(:, 2:end) .^ 2))) / (2 * m);

J = cost + regularization;


% -------------------------------------------------------------

% Backpropagation

Delta_1 = 0;
Delta_2 = 0;

for k = 1:m,
    x_i = X(k, :);

    a1 = [1; x_i'];
    z2 = Theta1 * a1;
    a2 = sigmoid(z2);

    a2 = [1; a2];
    z3 = Theta2 * a2;
    h_x = sigmoid(z3);

    y_i = ([1:K] == y(k))';
    delta_3 = h_x - y_i;

    d_2_with_bias = Theta2' * delta_3;
    delta_2 = d_2_with_bias(2:end) .* sigmoidGradient(z2);

    Delta_1 = Delta_1 + delta_2 * a1';
    Delta_2 = Delta_2 + delta_3 * a2';
end;

reg_D1 = lambda / m * Theta1;
reg_D1(:, 1) = 0;
Theta1_grad = Delta_1 / m + reg_D1;

reg_D2 = lambda / m * Theta2;
reg_D2(:, 1) = 0;
Theta2_grad = Delta_2 / m + reg_D2;


% =========================================================================

% Unroll gradients
grad = [Theta1_grad(:) ; Theta2_grad(:)];


end
