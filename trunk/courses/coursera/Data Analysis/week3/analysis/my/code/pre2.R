#
# https://class.coursera.org/dataanalysis-001/forum/thread?thread_id=1397
#
# Katie - the steps are as follows:
#   
#   You decide on the predictand and predictor variables and run 
# a linear regression fit. Assuming you start with a raw variables, 
# the process usually is to determine if the relationship is linear 
# or otherwise. Residuals plot can indicate . that.
# 
# If you decide that either the predictor and/or the predictand 
# needs transformation, you do so. By the way, here you can apply 
# either linear transformation or non-linear. Linear xforms are of 
# the type (constant)*(variable), (variable)/(constant), 
# or (variable) + (constant). All other transformations are non-linear.
# 
# Post transformation, you will re-run the linear regression model. 
# Plot the residuals and see their distribution. They should be random.
# 
# At this point (an in fact even on the raw data), you may also 
# evaluate the r^2. The corresponding R code is: 
#   summary(<linear model>)$r.squared, where the <linear model> 
#   is the R object obtained from running the lm function.
# 
# Here is an example, to illustrate the point. If you run a linear 
# regression on Interest Rate and say monthly income, and compare 
# the r^2 of this with the r^2 for regression on Interest rate and 
# FICO, you will see that one r^2 is larger than the other indicating 
# a closer degree of correlation between Interest rate and this variable.
# A visual exploration of their plots will also show this correlation.
# 
# Hope this helps!