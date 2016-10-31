
context("Expected output")

test_that("too big feed number throw right message", {
    expect_error(distract(9999), "feed not existing")
})

test_that("non-numeric value is not attempted to parse", {
    expect_error(distract("a"), "enter a numeric value")
})

test_that("correct message is displayed when no parameters", {
    expect_match(distract(), "Have a look")
})

test_that("top entries returns correct error message", {
    expect_error(top.entries(NULL, 10), "enter a url")
    expect_error(top.entries("xxx"), "url not valid")
})

test_that("links function", {
    expect_error(links())
})
