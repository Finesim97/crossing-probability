lower_b <- c(0.3, 0.3, 0.3)
upper_b <- c(0.9, 0.9, 0.9)
both <- 0.784
lower <- 0.657
upper <- 0.271


test_that("example works", {
  res <- crossingprob(lower_b, upper_b)
  expect_equal(res, both)
})

methods_both <- c("auto", "ecdf2-ks2001", "ecdf2-mn2017")

for (method in methods_both) {
  test_that(paste0(method, " both works"), {
    res <- crossingprob(lower_b, upper_b, method = method)
    expect_equal(res, both)
  })
}

methods_one <- c("ecdf1-mns2016", "ecdf1-new")
for (method in methods_one) {
  test_that(paste0(method, " two sided fails"), {
    expect_error(crossingprob(lower_b, upper_b, method = method),
                 ".+ two side.+")
  })
}


methods_all <- c(methods_both, methods_one)

for (method in methods_all) {
  test_that(paste0(method, " lower works"), {
    res <- crossingprob(lowerboundaries = lower_b, method = method)
    expect_equal(res, lower)
  })

  test_that(paste0(method, " upper works"), {
    res <- crossingprob(upperboundaries = upper_b, method = method)
    expect_equal(res, upper)
  })

  test_that(paste0(method, " lower boundary outside [0,1] fails"), {
    # TODO macht 0 am anfang das ergebnis echt anders?
    expect_equal(crossingprob(c(0.9, 1), method = method), 1)
    expect_error(crossingprob(c(0.9, 1.1), method = method),
                 "^The lower .+ \\[0,1\\]!")
    expect_equal(crossingprob(c(0, 0.5), method = method), 0.25)
    expect_error(crossingprob(c(-0.1, 0.5), method = method),
                 "^The lower .+ \\[0,1\\]!")
  })

  test_that(paste0(method, " upper boundary outside [0,1] fails"), {
    expect_equal(crossingprob(upperboundaries = c(0.9, 1), method = method),
                 0.01)
    expect_error(crossingprob(upperboundaries = c(0.9, 1.1), method = method),
                 "^The upper .+ \\[0,1\\]!")
    expect_equal(crossingprob(upperboundaries = c(0, 0.5), method = method), 1)
    expect_error(crossingprob(upperboundaries = c(-0.1, 0.5), method = method),
                 "^The upper .+ \\[0,1\\]!")
  })


  test_that(paste0(method, " lower not increasing fails"), {
    expect_equal(crossingprob(c(0.1, 0.1), method = method), 0.19)
    expect_error(crossingprob(c(0.1, 0.09), method = method),
                 "^The lower .+ increasing!")
  })
  test_that(paste0(method, " upper not increasing fails"), {
    expect_equal(crossingprob(upperboundaries = c(0.1, 0.1), method = method),
                 0.99)
    expect_error(crossingprob(upperboundaries = c(0.1, 0.09), method = method),
                 "^The upper .+ increasing!")
  })


}

test_that("no boundaries fails", {
  expect_error(crossingprob(), "^Either .+")
})


test_that("mismatched lengths fails", {
  expect_error(crossingprob(c(0), c(1, 1)), ".+ length.+")
})
test_that("cross fails", {
  expect_error(crossingprob(c(0.1, 0.2, 0.5, 0.6), c(0.1, 0.2, 0.3, 0.7)),
               ".+ cross+")
})


test_that("'ecdf1-mns2016' with large vectors results in a warning", {
  pvec <- sort(runif(30001))
  expect_warning(crossingprob(pvec, NULL, method = "ecdf1-mns2016"),
                 ".+ large+")
  expect_warning(crossingprob(NULL, pvec, method = "ecdf1-mns2016"),
                 ".+ large+")
})
