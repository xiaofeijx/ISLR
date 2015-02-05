n#chapter2

set.seed(11)
## create a data frame of 3 columns and 10,000 rows filled with 
## random uniform data
cube <- data.frame(matrix(runif(3*10000, min = 0, 1), ncol=3))

## get the max and min for each row
cube[, "max"] <- apply(cube, MARGIN = 1, max) 
cube[, "min"] <- apply(cube, MARGIN = 1, min) 

## add a column to mark if the row is considered in the boundary
## initially this column will be filled with 0 (not boundary)
cube$boundary <-0

## set boundary to 1 if min is less than .05 
cube$boundary[cube$min < .05] <-1
##  or max greater than .95
cube$boundary[cube$max > .95] <-1
sum(cube$boundary)

## Expected answer
10000 * (1-.9^3)

##--------------------------------------------------------

set.seed(11)  

## create a data frame of 50 columns and 10,000 rows filled with 
## random uniform data
hypercube <- data.frame(matrix(runif(50*10000, min = 0, 1), ncol=50))

## get the max and min for each row
hypercube[, "max"] <- apply(hypercube, MARGIN = 1, max) 
hypercube[, "min"] <- apply(hypercube, MARGIN = 1, min) 


## add a column to mark if the row is considered in the boundary
## initially this column will be filled with 0 (not boundary)
hypercube$boundary <-0

## set boundary to 1 if min is less than .05 
hypercube$boundary[hypercube$min < .05] <-1
##  or max greater than .95
hypercube$boundary[hypercube$max > .95] <-1
sum(hypercube$boundary)

## Expected answer
10000 * (1-.9^50)


set.seed(37)

# Generate sample data
n <- 20
x0 <- sort(runif(n, min=0, max=10))
y0 <- 0.5 * x0 + rnorm(n, sd=0.2)

Estimate <- function(x, neighborhood=2){
  # Estimates x as the average value of y within neighborhood
  # Note this implementation is not k nearest neighbors,
  # since it uses a neighborhood of fixed size.
  mean(y0[abs(x - x0) < neighborhood])
}

x <- seq(0, 10, length.out=2000)
yhat <- sapply(x, Estimate)

png('neighbor.png')
plot(x, yhat, col='green')
points(x0, y0)
dev.off()