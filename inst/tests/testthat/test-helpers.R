library(RxStudio)
library(testthat)

context('generators')

test_that('regimens', {
    expect_error(regimen())
    expect_error(regimen(TINF = 2))
    expect_error(regimen('2000'))
    expect_equal(regimen(2000), list(DOSE = 2000, set = 'REGIMEN'))
})
