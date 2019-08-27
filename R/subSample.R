 
#' subsample
#'
#' @param counts.cc counts[, cc]
#' @param cc column number
#' @param n.samples number of samples
#' @param binom.prob binomial probability
#' @param  seed see @param seed in ?RNAsub
#' @param ... additional arguments for rbinom or sapply
#' @return a vector of subsampled counts
#'  
#' 
subsample <- function(counts.cc, cc, n.samples, binom.prob, seed, ...){
  if(length(binom.prob)==1){
    smpled <- sapply(seq_len(length(counts.cc)), function(rr){ 
      set.seed(seed+cc+rr+cc*rr)
      stats::rbinom(1, size = as.integer(counts.cc[rr]), prob = as.numeric(binom.prob))
    })
  }else if(length(binom.prob)==n.samples){
    smpled <- sapply(seq_len(length(counts.cc)), function(rr){
      set.seed(seed+cc+rr+cc*rr)
      stats::rbinom(1, size = as.integer(counts.cc[rr]), prob = as.numeric(binom.prob[cc]))
    })
  }
  else{
    stop("ERROR: Possible cause of error: wrong input.")
  }
  return(smpled)
}