#' @title Example RNA-seq data
#' @description  creates a synthetic data from negative binomial distribution
#'
#' @param G number of genes
#' @param n number of samples
#' @param disp over-dispersion parameter for NB distribution
#' @param rlt.abund relative abundances
#' @param L library sizes
#' @param L.param log.normal distribution parameters for library sizes (if L is not NULL)
#' @param ... other arguments
#'
#' @return a matrix of counts with dimension Gxn
#'
#' @export
#'
#' @examples
#' # default
#' counts <- creat.exampl.RNAseq.data()
#' dim(counts)
#' head(counts[, 1:5])
#'
#' #custom
#' counts <- creat.exampl.RNAseq.data(G=1000, n=5, L.param = list(log.mean=10, scale=0.1))
#' dim(counts)
#' head(counts[, 1:5])
#'
#' @importFrom stats rlnorm runif rnbinom
creat.exampl.RNAseq.data <- function(G=10000, n=10, disp=0.8, rlt.abund=NULL, L=NULL,
                                     L.param=list(log.mean=15, scale=0.1), ...){
  if(is.null(L)){
    L <- round(rlnorm(n, L.param[["log.mean"]], L.param[["scale"]]))
  }
  else{
    if(length(L) != n) stop("Invalid input!")
  }

  if(is.null(rlt.abund)){
    rlt.abund <- runif(G)
    rlt.abund <- rlt.abund/sum(rlt.abund)
  }
  else{
    if(length(rlt.abund) != G) stop("Invalid input!")
  }

  mean.expr <- crossprod(t(rlt.abund), L)

  counts    <- sapply(1:n, function(i){
    sapply(1:G, function(g){
      rnbinom(1, mu=mean.expr[g, i], size = 1/disp)
    })
  })

  colnames(counts) <- paste0("sample_", 1:n)
  rownames(counts) <- paste0("gene_", 1:G)
  counts
}
