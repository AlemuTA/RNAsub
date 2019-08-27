#' Calculates the binomial probabilities
#'
#' @param counts see @param counts of RNAsub
#' @param prop see @param prop of RNAsub
#' @param target.LS see @param target.LS of RNAsub
#' @param target.total.count see @param target.total.count of RNAsub
#'
#' @return a vector of binomial probabilities
#' 
calcBinomProb <- function(counts, prop, target.LS, target.total.count){
  if(!is.null(prop) & is.null(target.LS) & is.null(target.total.count)){
    binom.prob <- prop
  }else if(is.null(prop) & !is.null(target.LS) & is.null(target.total.count)){
    binom.prob <- target.LS/colSums(counts)
  }else if(is.null(prop) & is.null(target.LS) & !is.null(target.total.count)){
    binom.prob <- target.total.count/sum(counts)
  }
  
  if(any(is.na(binom.prob))){
    stop("Binomial probability contains missing value.")
  }
  if(any(binom.prob>1) | any(binom.prob<0)){
    warning("Binomial probability is out of the range [0, 1]. Probabilities less than 0
            are scaled to 0, and probabilities more than 1 are scaled to 1.")
    binom.prob[binom.prob<0] <- 0
    binom.prob[binom.prob>1] <- 1
  }
  
  return(binom.prob)
}