#' Crossing Probability
#'
#' @description
#' Calculates the boundary crossing probability for a sequence of uniformly
#' distributed random variables for either one- or two-sided boundaries.
#' \loadmathjax
#'
#' @details
#' For \mjeqn{n}{n} i.i.d. uniformly distributed random variables
#' \mjeqn{X_1, ..., X_n}{X1, ..., Xn}, for which \mjeqn{F}{F} is the
#' (continuous) cumulative distribution function, and
#' their order statistics \mjeqn{X_{(1)}, ..., X_{(n)}}{X(1), ..., X(n)}
#' the non-crossing probability between the lower boundaries
#'  \mjeqn{\alpha_1, ..., \alpha_n}{alpha_1, ..., alpha_n} and upper boundaries
#'  \mjeqn{\beta_1, ..., \beta_n}{beta_1, ..., beta_n} is:
#'
#' \mjdeqn{\pi_\text{noncross}=P(\forall i : \alpha_i \leq X_{(i)} \leq \beta_i)
#' .}{pi_noncross=P(forall i: alpha_i < X(i) < beta_i ).}
#'
#' The same non-crossing probability can also be determined by the
#' order statistics \mjeqn{U_{(1)}, ..., U_{(n)}}{U(1), ..., U(n)} of
#'  \mjeqn{U(0,1)}{U(0,1)} distributed random variables.
#' Accordingly the transformed boundaries are
#' given by the distribution function
#' \mjeqn{F}{F}: \mjeqn{b_i=F(\alpha_i)}{bi=F(alpha_i)} and
#' \mjeqn{B_i=F(\beta_i)}{bi=F(beta_i)}, i. e.:
#'
#' \mjdeqn{\pi_\text{noncross}=P(\forall i : b_i \leq U_{(i)} \leq B_i)
#' .}{pi_noncross=P(forall i: alpha_i < X(i) < beta_i ).}
#'
#' This package provides algorithms to calculate the crossing probability
#' \mjeqn{\pi_\text{cross}=1-\pi_\text{noncross}}{p_cross = 1-pi_noncross}.
#' See \insertCite{NEWEST;textual}{crossingprobability} for
#' more details.
#'
#' We provide an \code{Rcpp} interface to the following algorithms from
#' the C++ library \code{"crossing-probability"} by Amit Moscovich
#' \insertCite{repo}{crossingprobability}:
#'
#' \itemize{
#'  \item{\code{"ecdf1-mns2016"}}{\mjeqn{O(n^2)}{O(n^2)}
#'       \insertCite{MNS2016}{crossingprobability}}
#'  \item{\code{"ecdf1-new"}}{\mjeqn{O(n^2)}{O(n^2)}
#'       \insertCite{NEWEST}{crossingprobability}}
#'  \item{\code{"ecdf2-ks2001"}}{\mjeqn{O(n^3)}{O(n^3)}
#'       \insertCite{KS2001}{crossingprobability}}
#'  \item{\code{"ecdf2-mn2017"}}{\mjeqn{O(n^2 \log{n})}{O(n^2 log(n))}
#'       \insertCite{MN2017}{crossingprobability}}
#' }
#'
#' The default \code{"auto"} selects \code{"ecdf1-new"} for one-sided
#' boundaries and \code{"ecdf2-mn2017"} for two-sided uses. Note that
#' the crossing-probability is returned and not the non-crossing-probability.
#'
#' The option \code{"ecdf1-mns2016"} is not recommended for large vectors
#' with \mjeqn{n > 30000}{n > 30000}.
#'
#' @param lowerboundaries numeric vector with the lower boundaries (see
#'                        vector b in the details) between 0 and 1.
#'                        Defaults to \code{NULL}.
#' @param upperboundaries numeric vector with the upper boundaries (see
#'                        vector B in the details) between 0 and 1.
#'                        Defaults to \code{NULL}.
#' @param method algorithm to use, see details. Defaults to \code{"auto"}.
#' @return A float giving the probability
#'         \mjeqn{\pi_\text{cross}}{pi_cross} for the ordered
#'         uniform random variables to stay within the boundaries.
#' @references
#' \insertAllCited{}
#' @importFrom Rdpack reprompt
#' @import mathjaxr
#' @examples
#' b <- c(0.1, 0.1, 0.3)
#' B <- c(0.25, 0.5, 0.7)
#' crossingprob(b, B)
#' @export
crossingprob <-
  function(lowerboundaries = NULL,
           upperboundaries = NULL,
           method = c("auto",
                      "ecdf1-mns2016",
                      "ecdf1-new",
                      "ecdf2-ks2001",
                      "ecdf2-mn2017")) {
    method <- match.arg(method)
    l_null <- is.null(lowerboundaries)
    u_null <- is.null(upperboundaries)

    checkargs(lowerboundaries, upperboundaries, method)

    if (method == "auto") {
      method <-  if (!l_null && !u_null)
        "ecdf2-mn2017"
      else
        "ecdf1-new"
    }

    if (method == "ecdf1-mns2016") {
      if (max(length(upperboundaries), length(lowerboundaries)) > 30000) {
        warning("'ecdf1-mns2016' is not recommended for large vectors.")
      }
      if (l_null) {
        return(1 - ecdf1_mns2016_B(upperboundaries))
      } else {
        return(1 - ecdf1_mns2016_b(lowerboundaries))
      }
    } else if (method == "ecdf1-new") {
      if (l_null) {
        return(1 - ecdf1_new_B(upperboundaries))
      } else {
        return(1 - ecdf1_new_b(lowerboundaries))
      }
    } else if (method == "ecdf2-mn2017") {
      if (l_null) {
        lowerboundaries <- rep_len(0, length.out = length(upperboundaries))
      } else if (u_null) {
        upperboundaries <- rep_len(1, length.out = length(lowerboundaries))
      }
      return(1 - ecdf2(lowerboundaries, upperboundaries, TRUE))
    } else if (method == "ecdf2-ks2001") {
      if (l_null) {
        lowerboundaries <-
          rep_len(0, length.out = length(upperboundaries))
      } else if (u_null) {
        upperboundaries <-
          rep_len(1, length.out = length(lowerboundaries))
      }
      return(1 - ecdf2(lowerboundaries, upperboundaries, FALSE))
    }
  }
checkargs <-
  function(lowerboundaries,
           upperboundaries,
           method) {
    l_null <- is.null(lowerboundaries)
    u_null <- is.null(upperboundaries)

    if (l_null && u_null) {
      stop("Either the lower or upper boundary need to be defined!")
    }


    if (!l_null) {
      if (!all(lowerboundaries == cummax(lowerboundaries))) {
        stop("The lower boundaries should be monotonically increasing!")
      }
      if (min(lowerboundaries) < 0 || max(lowerboundaries) > 1) {
        stop("The lower boundaries need to be within [0,1]!")
      }
    }

    if (!u_null) {
      if (!all(upperboundaries == cummax(upperboundaries))) {
        stop("The upper boundaries should be monotonically increasing!")
      }
      if (min(upperboundaries) < 0 || max(upperboundaries) > 1) {
        stop("The upper boundaries need to be within [0,1]!")
      }

    }

    if (!l_null && !u_null) {
      if (length(lowerboundaries) != length(upperboundaries)) {
        stop("The upper and lower boundary need to be of the same length!")
      }
      if (any(lowerboundaries > upperboundaries)) {
        stop("The lower boundary can not cross the upper boundary!")
      }
      if (!method %in% c("auto", "ecdf2-mn2017", "ecdf2-ks2001")) {
        stop(paste0(method, " does not support two sided application!"))
      }
    }
  }
