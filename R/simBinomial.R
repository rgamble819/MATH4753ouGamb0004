#' Creates plots of the experiment described in parameters
#'
#' @param iter # of experiments to run
#' @param n # of trials to run
#' @param p The probability of the distribution
#' @examples
#' simBinomial(100,10,0.5)
#'
#' Runs 100 experiments with 10 trials. Symmetrically distributed round 5.
#' @export
simBinomial <- function(iter=100,n=10, p=0.5){
  #Initial sample Matrix that is initialized with NAs.
  sample.mat=matrix(NA,nr=n,nc=iter, byrow=TRUE)

  #Vector to contain successes.
  succ=c()

  #Iterate through trials.
  for( i in 1:iter)
  {
    #Fill sample matrix
    sample.mat[,i]=sample(c(1,0),n,replace=TRUE, prob=c(p,1-p))
    #Sum of successes.
    succ[i]=sum(sample.mat[,i])
  }

  #Table of successes.
  succ.tab=table(factor(succ,levels=0:n))

  #Create barplot.
  barplot(succ.tab/(iter), col=rainbow(n+1), main="Binomial simulation", xlab="Number of successes")
  if(i==10000)
  {
    print(succ.tab/iter)
  }
}

#' White and black balls in urn without replacement
#'
#' @param iter # of experiments to run
#' @param N # of balls
#' @param r number of white balls
#' @param n # of balls drawn
#' @examples
#' simHyper(iter=100,N=20, r=12, n=5)
#'
#' Runs 100 experiments, selecting 5 balls without replacement each time. Plot is generated with frequency of white balls drawn.
#' @export
simHyper=function(iter,N=20,r=12,n=5){
  # make a matrix to hold the samples
  #initially filled with NA's
  sam.mat=matrix(NA,nr=n,nc=iter, byrow=TRUE)
  #Make a vector to hold the number of successes over the trials
  succ=c()
  for( i in 1:iter){
    #Fill each column with a new sample
    sam.mat[,i]=sample(rep(c(1,0),c(r,N-r)),n,replace=FALSE)
    #Calculate a statistic from the sample (this case it is the sum)
    succ[i]=sum(sam.mat[,i])
  }
  #Make a table of successes
  succ.tab=table(factor(succ,levels=0:n))
  #Make a barplot of the proportions
  barplot(succ.tab/(iter), col=rainbow(n+1), main="HYPERGEOMETRIC simulation", xlab="Number of successes")
  if(iter==10000)
  {
    print(succ.tab/iter)
  }
}

#' White and black balls in urn without replacement
#'
#' @param n
#' @param iter
#' @param time
#' @examples
#' simSample(n=5,iter=100,time=0.5)
#'
#' Run the experiment 100 times,
#' @export
simSample=function(n,iter=10,time=0.5){
  for( i in 1:iter){
    #make a sample
    s=sample(1:10,n,replace=TRUE)
    # turn the sample into a factor
    sf=factor(s,levels=1:10)
    #make a barplot
    if(i==1){
   barplot(table(sf)/n,beside=TRUE,col=rainbow(10), main=paste("Example sample()", " iteration ", i, " n= ", n,sep="") , ylim=c(0,0.2))

}
    #release the table
    Sys.sleep(time)
  }
}
