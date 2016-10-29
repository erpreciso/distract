
context("Expected output")

test_that("too big feed number throw right message", {
    expect_error(distract(34), "feed not existing")
})

test_that("non-numeric value is not attempted to parse", {
    expect_error(distract("a"), "enter a numeric value")
})

test_that("correct message is displayed when no parameters", {
    expect_match(distract(), "Have a look")
})
