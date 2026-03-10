test_that("as.quoted works like plyr::as.quoted", {
  q1 <- as.quoted(~ a + b)
  expect_equal(names(q1), c("a", "b"))
  expect_true(is.name(q1[[1]]))
  expect_true(is.name(q1[[2]]))
  expect_identical(class(q1), "quoted")

  q2 <- as.quoted(c("a", "b"))
  expect_equal(names(q2), c("a", "b"))
  expect_true(is.name(q2[[1]]))
  expect_true(is.name(q2[[2]]))
  expect_identical(class(q2), "quoted")

  q3 <- as.quoted(c("a", "log(b)"))
  expect_equal(names(q3), c("a", "log(b)"))
  expect_true(is.name(q3[[1]]))
  expect_true(is.call(q3[[2]]))
})

test_that("eval.quoted evaluates expressions in supplied environment", {
  df <- data.frame(a = 1:2, b = 3:4)
  q <- as.quoted(~ a + b)
  res <- eval.quoted(q, envir = df)

  expect_equal(res$a, 1:2)
  expect_equal(res$b, 3:4)

  q2 <- as.quoted(c("a", "log(b)"))
  res2 <- eval.quoted(q2, envir = df)
  expect_equal(res2$a, 1:2)
  expect_equal(res2$`log(b)`, log(3:4))
})
