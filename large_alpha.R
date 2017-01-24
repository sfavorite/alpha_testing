# Linear Regression with Gradient Descent 
# Two examples: straight line, scattered data points

plotMinimization <- function(point, iter) {
    points(iter, point, col="red")
}

# Cost function using linear regression model
computeCost <- function(X, y, theta)
{
    m <- length(y)
    
    J <- 0
    
    # Compute slope intercpet 
    # y <- y_intercept + slope * x
    # y is called h since it is our hypothesis
    # theta is the slope and intercept
    h <- X %*% theta
    error <- h - y
    error_sqr <- error ^ 2
    error_sum <- sum(error_sqr)
    J = (1/2/m) * error_sum
    J
    
}

gradientDescent <- function(X, y, theta, alpha, num_iters)
{
    m <- length(y)
    J_history <- list(0, num_iters)
    
    for (iter in 1:num_iters)
    {
        h = X %*% theta
        theta = theta - alpha/m*(t(X) %*% (h - y))
        J_history[iter] = computeCost(X, y, theta)
        plotMinimization(J_history[iter], iter);
        
    }
    theta
}

minimize <- function(iterations, X, y, m, theta, alpha)
{
    
    # Plot minimization
    current_cost <- computeCost(X, y, theta)
    if (current_cost > iterations) {
        largest <- current_cost
    } else {
        largest <- iterations
    }
    plot(0:largest, 0:largest, type="n", xlab = "# Iterations", ylab="J of theta")
    points(0, current_cost, col="blue")
    theta <- gradientDescent(X, y, theta, alpha, iterations)
    print("Found theta: ")
    print(theta[1])
    print(theta[2])
    
    return(theta)
}

predict <- function(alpha){
    # Run minimization with a straight line
    X <- c(1, 2, 3, 4, 5, 6, 7)
    y <- c(1, 2, 3, 4, 5, 6, 7)
    # Get number of test cases
    m <- length(y)
    # Theta is the weights of our learning 
    # algorithm
    theta <- matrix(0, 2, 1)
    # Add a column with value 1 for multiplication
    X <- cbind(1, X)
    # How many iterations
    iterations <- 20
    # Run minimization
    theta <- minimize(iterations, X, y, m, theta, alpha)

}

# Test with a small learning rate
predict(0.001)
# Test with a large learning rate
predict(0.1)
# Just right 
predict(0.01)
