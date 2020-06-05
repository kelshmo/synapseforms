context("accept-reject-submission.R")

test_that("strings are generated from reject_submission", {
  m <- mockery::mock("character strings") # i think this should be a string to mock the reason?
  mockery::stub(reject_submission, "rest_put", m)
  test_reject <- reject_submission(syn = "",
                                   form_data_id = 42,
                                   reason = "your submission looks good")
  testthat::expect_vector(test_reject)
})

test_that("warning for strings more than 500 chars", {
  string <- glue::glue_collapse({seq(1:300)}, sep = ",")
  m <- mockery::mock(string)
  mockery::stub(reject_submission, "rest_put", m)
  testthat::expect_error(reject_submission(syn = "",
                                           form_data_id = 42,
                                           reason = string)
  )

})
