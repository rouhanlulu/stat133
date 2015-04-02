#################################################################################
#### BML Simulation Study

#### Put in this file the code to run the BML simulation study for a set of input parameters.
#### Save some of the output data into an R object and use save() to save it to disk for reference
#### when you write up your results.
#### The output can e.g. be how many steps the system took until it hit gridlock or
#### how many steps you observered before concluding that it is in a free flowing state.
source('~/src/stat133/assignments/hw5/bml_functions.R')

#averages and SDs of two different sized matirces with three different densities
#10*10 Matirx p=(0.21, 0.31, 0.61)

matrix1=mean(replicate(100,bml.sim(10,10,0.21)))
matrix11=mean(replicate(100,bml.sim(10,10,0.31)))
matrix111=mean(replicate(100,bml.sim(10,10,0.61)))

smsd1 = sd (replicate(100,bml.sim(10,10,0.21)))
smsd2 = sd (replicate(100,bml.sim(10,10,0.31)))
smsd3 = sd (replicate(100,bml.sim(10,10,0.61)))

#100*100 Matrix p=(0.21, 0.31, 0.61)

matrix2= mean(replicate(100,bml.sim(100,100,0.21)))
matrix22= mean(replicate(100,bml.sim(100,100,0.31)))
matrix222= mean(replicate(100,bml.sim(100,100,0.61)))

bmsd1 = sd(replicate(100,bml.sim(100,100,0.21)))
bmsd2 = sd(replicate(100,bml.sim(100,100,0.31)))
bmsd3 = sd(replicate(100,bml.sim(100,100,0.61)))
#Plot
bml.sim.m <- function(r, c, p){
  m=bml.init(r,c,p)
  gridlock = F
  i = 0
  max = 10000
  while (!gridlock){
    output=bml.step(m)
    m = output[[1]]
    gridlock = !output[[2]]
    i= i+1
    if (i>=max){
      warning ("max iteration reached")
      return (0)
      break
    }
  }
  return (m)
}

####initial image
image(bml.init(10,10,0.6),col=c("white","red","blue"),main="init for 10*10, p=0.6")
image(bml.init(100,100,0.6),col=c("white","red","blue"),main="init for 100*100, p=0.6")

####gridlock image

image(t(apply(bml.sim.m(10,10,0.6),2,rev)),col=c("white","red","blue"), 
      main = "gridlock for 10*10, p=0.6")
image(t(apply(bml.sim.m(100,100,0.6),2,rev)),col=c("white","red","blue"), 
      main = "gridlock for 100*100, p=0.6")

# for the "steps vs. densities" plot
diff.p= seq(from = 0.2,to = 1,by = 0.1)

z=apply(replicate(10,sapply(diff.p, function(p) bml.sim(10,10,p))),1,mean)
plot(diff.p, z, ylim=c(0,10000),main="steps VS density for 10*10", 
     xlab ="density p", ylab= "avg # of steps", abline(v=0.32, col="red"))

y=apply(replicate(10,sapply(diff.p, function(p) bml.sim(50,50,p))),1,mean)
plot(diff.p, y, ylim=c(0,10000),main="steps VS density for 50*50", 
     xlab ="density p", ylab= "avg # of steps", abline(v=0.32, col="red"))

x=apply(replicate(10,sapply(diff.p, function(p) bml.sim(100,100,p))), 1, mean)
plot(diff.p, x, ylim=c(0,10000),main="steps VS density for 100*100", 
     xlab ="density p", ylab= "avg # of steps", abline(v=0.32, col="red"))
