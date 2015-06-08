library(testthat)

a <- 1 + 1
print(a)
a
warning('this is the warning message')
b <- 1 + 2
print(b)
test_that(
    'test_that message which should pass', {
    expect_that(sin(pi / 4), equals(1 / sqrt(2)))
})
c <- 1 + 3
print(c)
##test_that('test_that message which should FAIL', {
##    expect_that(sin(pi / 4), equals(1))
##})
d <- 1 + 4
print(d)

