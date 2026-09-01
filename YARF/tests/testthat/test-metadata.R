test_that("package metadata exposes modern discovery fields", {
  desc = utils::packageDescription("YARF")

  expect_identical(desc$Version, "1.2.0")
  expect_match(desc$URL, "kapelner.github.io/YARF", fixed = TRUE)
  expect_match(desc$BugReports, "github.com/kapelner/YARF/issues", fixed = TRUE)
})
