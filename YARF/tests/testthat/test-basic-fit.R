test_that("a small classification forest fits and predicts", {
  old_cores = get("YARF_NUM_CORES", envir = YARF:::YARF_globals)
  on.exit(set_YARF_num_cores(old_cores), add = TRUE)
  set_YARF_num_cores(1)

  X = iris[1:30, 1:4]
  y = factor(rep(c("a", "b"), each = 15))
  fit = YARF(X, y, num_trees = 2, seed = 42, verbose = FALSE)

  expect_s3_class(fit, "YARF")
  expect_length(predict(fit, X), nrow(X))
  expect_silent(first_order_interaction_investigator(fit, plot = FALSE, num_var_plot = Inf))
})
