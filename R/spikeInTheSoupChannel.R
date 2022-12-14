#' Sample spike-in/soup Droplets from the Soup channel
#'
#' @export
#' @param sc A \code{SoupChannel} object.
#' @param target.spikein.droplet.count per-Droplet read depth to sample
#' @param seed Random seed, to ensure reproducibility.
#' @return Spike-in / soup sampled droplets
#' @examples
#' sc = load10X(system.file(package = "SoupX", "extdata/toyData"), keepDroplets=TRUE)
#' expected.count = round(median(Matrix::colSums(sc$toc)))
#' si = spikeInTheSoupChannel(sc,target.spikein.droplet.count= expected.count)
#' plot(sort(Matrix::colSums(si)))
#' removeNoncorrectedDroplets(sc)
spikeInTheSoupChannel <- function(sc, target.spikein.droplet.count = 5000, seed=1) {
  soup <- sc$tod[,colnames(sc$tod) %in% sc$soupDroplets]
  n.spike.ins <- max(1, round(sum(soup) / target.spikein.droplet.count))
  
  sel <- rep(1:n.spike.ins, (ncol(soup) %/% n.spike.ins) + 1 )
  set.seed(seed)
  sel <- sel[sample(1:ncol(soup))]

  f <- function(k, soup, sel) {
    return(Matrix::rowSums(soup[,sel == k]))
  }
  si = Matrix::Matrix(do.call(cbind, pbapply::pblapply(1:n.spike.ins, f, soup, sel)),sparse=T) # sample the colSum for each spikein droplet, and make a sparse matrix
  colnames(si) <- paste0("Soup.Spikein.",1:n.spike.ins)
  
  message(paste0("Generated n=",n.spike.ins, " soup/spike-in droplets with a median read count k=",median(median(Matrix::colSums(si)))))
  return(si)
}
