#' Check the validty of user inputs
#'
#' @param counts see @param counts of RNAsub
#' @param prop see @param prop of RNAsub
#' @param target.LS see @param target.LS of RNAsub
#' @param target.total.count see @param target.total.count of RNAsub
#'
#' @return Error or warning messages if inputs are not the way they are required to be.
#' 
#' 
#' 
checkInputs <- function(counts, prop, target.LS, target.total.count){
  
  all.inps <- c(is.null(prop), is.null(target.LS), is.null(target.total.count))
  
  if(any(is.na(counts))){
    stop("The count matrix contains missing (NA) values.")
  } 
  
  if(all(all.inps)){
    stop("Please specify either fraction of reads to be sampled (using prop), desired library size(s) (using target.LS), or the desired total read counts (using target.total.count). Only one must be specified.")
  }else if(sum(!all.inps)!=1){
    stop("Specify only one of the followings:  (1) fraction of reads to be sampled (using prop), (2) desired library size(s) (using target.LS), and (3) the desired total read counts (using target.total.count).")
  }
  
  if(!is.null(prop)){
    if(!(class(prop) %in% c("integer", "numeric"))){
      stop("The fraction of reads to be sampled (prop) can only be a numeric value between 0 and 1.")
    }else if(!((length(prop) == ncol(counts)) | (length(prop) == 1))){
      stop("A single fraction of reads to be sampled (prop) can be specified for every count. Alternatively, specify the fraction (prop) for each sample, in which the length of prop is equal to the number of samples in the original data.")
    }
  }
  
  if(!is.null(target.LS)){
    if(!(class(target.LS) %in% c("integer", "numeric"))){
      stop("The desired library size(s) (target.LS) can only be a numeric value.")
    }else if(!((length(target.LS) == ncol(counts)) | (length(target.LS) == 1))){
      stop("A single desired library size (target.LS) can be specified for every sample. Alternatively, specify the desired library sizes (target.LS) for each sample, in which the length of target.LS is equal to the number of samples in the original data.")
    }
  }
  
  if(!is.null(target.total.count)){
    if(!(class(target.total.count) %in% c("integer", "numeric"))){
      stop("The desired total read counts (target.total.count) can only be a numeric value.")
    }else if(length(target.total.count) != 1){
      stop("Only a single desired total read counts (target.total.count) is acceptable.")
    }
  }
  
}