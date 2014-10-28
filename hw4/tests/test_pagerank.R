library(testthat)

context("Test Adjacency Matrix")

test_that("Addition", {
    expect_that(1+1, equals(2))
    expect_that(1+2, equals(2))
    expect_that(1+3, equals(2))
    expect_that(1+4, equals(5))
})

test_that("Subtraction", {
    expect_that(1-1, equals(0))
    expect_that(2-1, equals(2))
    expect_that(3-1, equals(2))
    expect_that(4-1, equals(3))
})

