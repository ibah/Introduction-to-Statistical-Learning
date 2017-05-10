# Simulations

# a pair of correlated uniform variables
# http://stats.stackexchange.com/questions/66610/generate-pairs-of-random-numbers-uniformly-distributed-and-correlated

## Initialization and parameters 
set.seed(123)
r <- 0.6                            # Target (Spearman) correlation
n <- 500                            # Number of samples

## Functions
gen.gauss.cop <- function(r, n){
    rho <- 2 * sin(r * pi/6)        # Pearson correlation
    P <- toeplitz(c(1, rho))        # Correlation matrix
    d <- nrow(P)                    # Dimension
    ## Generate sample
    U <- pnorm(matrix(rnorm(n*d), ncol = d) %*% chol(P))
    return(U)
}

## Data generation and visualization
U <- gen.gauss.cop(r = r, n = n)
pairs(U, diag.panel = function(x){
    h <- hist(x, plot = FALSE)
    rect(head(h$breaks, -1), 0, tail(h$breaks, -1), h$counts/max(h$counts))})

cor(U)[1, 2]

## Simulation
set.seed(921)
r <- 0.6                                                # Target correlation
n <- c(10, 50, 100, 500, 1000, 5000); names(n) <- n     # Number of samples
S <- 1000                                               # Number of simulations

res <- sapply(n,
              function(n, r, S){
                  replicate(S, cor(gen.gauss.cop(r, n))[1, 2])
              }, 
              r = r, S = S)
boxplot(res, xlab = "Sample size", ylab = "Correlation")
abline(h = r, col = "red")

# checking

df = as.data.frame(U)
colnames(df) <- c('x','y')
head(df)
fit <- lm (y~x, df)
fit
plot(df)
abline(fit) # see the regression to the mean, the line is kind of tilted to the horizontal







