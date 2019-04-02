#' kxk_test
#'
#' Uses a 100 000 simulation of randomly sampled genes from a 13000 gene pool to
#' obtain estimates of links by random selection of genes.
#' This estimates are valid only for 1000 receptors and 1000 ligands.
#'
#' @param n The number of links found
#' @return Prints and estimate of the probability o
#' @export
#' @example
#' zelda::kxk_test(15)

kxk_test <- function(n){
  data("kxk_simulation")
  p_value <- sum(number_of_links >= n)/length(number_of_links)
  print( paste0('The estimated p-value (probability of occurrence of a equal or more extreme value under the null) for ', n, ' links is of ', p_value, '.') )
}

