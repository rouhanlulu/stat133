#################################################################################
#### Functions for BML Simulation Study


#### Initialization function.
## Input : size of grid [r and c] and density [p]
## Output : A matrix [m] with entries 0 (no cars) 1 (red cars) or 2 (blue cars)
## that stores the state of the system (i.e. location of red and blue cars)

bml.init <- function(r,c,p){
  matrix(sample(c(0,1,2), r*c, prob = c(1-p, p/2, p/2), replace = T), nrow = r)
}


#### Function to move the system one step (east and north)
## Input : a matrix [m] of the same type as the output from bml.init()
## Output : TWO variables, the updated [m] and a logical variable
## [grid.new] which should be TRUE if the system changed, FALSE otherwise.

## NOTE : the function should move the red cars once and the blue cars once,
## you can write extra functions that do just a step north or just a step east.

move_east <- function(m) {
  if(ncol(m)==1) {
    return(m)
  }
  blocked <- m[, c(2:ncol(m), 1)]!=0
  red_cars <- m*(m==1)
  m*(m!=1) + red_cars*blocked + (red_cars*!blocked)[,c(ncol(m), 1:(ncol(m)-1))] 
}

move_north <- function(m) {
  if(nrow(m)==1) {
    return(m)
  }
  blocked <- m[c(nrow(m), 1:(nrow(m)-1)),]!=0
  blue_cars <- m*(m==2)
  m*(m!=2) + blue_cars*blocked + (blue_cars*!blocked)[c(2:nrow(m), 1), ] 
}

bml.step <- function(m){
  m.new <- move_north(move_east(m))
  return(list(m.new, !all(m==m.new)))
}
#### Function to do a simulation for a given set of input parameters
## Input : size of grid [r and c] and density [p]
## Output : *up to you* (e.g. number of steps taken, did you hit gridlock, ...)

bml.sim <- function(r, c, p){
  m=bml.init(r,c,p)
  gridlock = F
  i= 0
  max.iter = 10000
  while (!gridlock){
    output=bml.step(m)
    m = output[[1]]
    gridlock = !output[[2]]
    i= i+1
    if (i>=max.iter){
      warning ("max iteration numbers reached")
      return (max.iter)
      break
    }
  }
  return (i)
}