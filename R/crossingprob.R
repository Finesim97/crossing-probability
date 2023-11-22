#' Crossing Probability
#'
#' @description
#' Calculates the boundary crossing probability in either a one- or
#' two-sided case.
#' \loadmathjax
#'
#' @details
#' For \mjeqn{n}{n} i.i.d. uniformly distributed random variables
#' \mjeqn{X_1, ..., X_n}{X1, ..., Xn} \mjeqn{X_i \sim U(0,1)}{Xi ~ U(0,1)}
#' the empirical cumulative distribution function is:
#'
#' \mjdeqn{\hat{F}_n(t) = \frac{1}{n}\sum_i{\mathbb{1}(
#' X_i \leq t)}.}{F^(t)_n = 1/n sum_i(1(Xi <= t)).
#' }
#'
#' For two monotone-increasing, non-crossing functions \mjeqn{g,h : [0,1]
#' \rightarrow \mathbb{R}}{g,h : [0,1] -> R} function the
#' two-sided non-crossing probability is:
#'
#' \mjdeqn{P(\forall t \in [0, 1] : g(t) < \hat{F}_n(t) < h(t) )
#' .}{P(forall t in [0,1] : g(t) < F^(t)_n < h(t) ).}
#'
#' See \insertCite{NEWEST;textual}{crossingprobability} for
#' more details.
#'
#' We provide the following methods as implemented by Amit Moscovich:
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
#' the crossing probability is returned and not the not crossing probability.
#'
#' @param lowerboundaries Lower boundaries. Defaults to \code{NULL}.
#' @param upperboundaries Upper boundaries. Defaults to \code{NULL}.
#' @param method See Details. Defaults to \code{"auto"}.
#' @return A float giving the probability to stay
#'    within the boundaries.
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
