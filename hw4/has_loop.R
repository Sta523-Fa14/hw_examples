source("predicates.R")

has_loop = function(g)
{

    return(FALSE)
}



library(testthat)

context("Test has_loop")

test_that("Valid Graphs", {
  g1 = list(A = list(edges   = c(1L),
                     weights = c(1)))

  expect_true(has_loop(g1))

  g2 = list(A = list(edges   = c(1L, 2L),
                     weights = c(1, 1)),
            B = list(edges   = c(1L, 2L),
                     weights = c(1, 1)))

  expect_true(has_loop(g2))

  g3 = list(A = list(edges   = c(2L),
                     weights = c(1)),
            B = list(edges   = c(1L),
                     weights = c(1)))

  expect_true(has_loop(g3))


  g4 = list(A = list(edges   = c(2L),
                     weights = c(1)),
            B = list(edges   = c(3L),
                     weights = c(1)),
            C = list(edges   = c(2L),
                     weights = c(1)))
  
  expect_true(has_loop(g4))


  g5 = list(A = list(edges   = integer(),
                     weights = numeric()),
            B = list(edges   = integer(),
                     weights = numeric()),
            C = list(edges   = integer(),
                     weights = numeric()))
  
  expect_false(has_loop(g5))


  g6 = list(A = list(edges   = integer(),
                     weights = numeric()),
            B = list(edges   = c(1L,3L),
                     weights = c(1,1)),
            C = list(edges   = integer(),
                     weights = numeric()))
  
  expect_false(has_loop(g6))



  g7 = list(A = list(edges   = c(2L),
                     weights = c(1)),
            B = list(edges   = c(3L),
                     weights = c(1)),
            C = list(edges   = c(4L),
                     weights = c(1)),
            D = list(edges   = c(5L),
                     weights = c(1)),
            E = list(edges   = c(6L),
                     weights = c(1)),
            F = list(edges   = integer(),
                     weights = numeric()))

  expect_false(has_loop(g7))

  g8 = list(A  = list(edges   = c(2L,3L),
                      weights = c(1,1)),
            B1 = list(edges   = c(4L,5L),
                      weights = c(1,1)),
            B2 = list(edges   = c(6L,7L),
                      weights = c(1,1)),
            C11= list(edges   = integer(),
                      weights = numeric()),
            C12= list(edges   = integer(),
                      weights = numeric()),
            C21= list(edges   = integer(),
                      weights = numeric()),
            C22= list(edges   = integer(),
                      weights = numeric()))
            
  expect_false(has_loop(g8))

  g8$C12 = list(edges = 6L, weights = 1)
  expect_false(has_loop(g8))

  g8$C12 = list(edges = 1L, weights = 1)
  expect_true(has_loop(g8))
})
