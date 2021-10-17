library(RxStudio)
library(testthat)

context('generators')

test_that('regimens', {
    expect_error(regimen())
    expect_error(regimen(TINF = 2))
    expect_error(regimen('2000'))
    expect_equal(regimen(2000), list(DOSE = 2000, set = 'REGIMEN'))
})

test_that('dose', {
    expect_error(dose())
    expect_error(dose(TINF = 2))
    expect_error(dose('2000'))
    expect_error(dose(Sys.time()))
    expect_error(dose(Sys.time(), '2000'))
    expect_error(dose(as.POSIXct('2020-09-05'), 2000))
    expect_equal(
        dose(as.POSIXct('2020-09-05'), 2000, 2),
        list(DATETIME = as.POSIXct('2020-09-05'), DOSE = 2000, TINF = 2, set = 'HISTDOSE'))
})
