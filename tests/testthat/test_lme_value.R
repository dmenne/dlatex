context("Strings from lm, lme")
library(nlme)

test_that("Valid values are returned from lme.value and friends", {
  fm1Oats.sum <- summary(lme(yield~ordered(nitro)*Variety, data=Oats,
                             random = ~1|Block/Variety))
  expect_equal(lme.value(fm1Oats.sum,"VarietyMarvellous", signif = 3), 5.29)
  expect_equal(lme.stderr(fm1Oats.sum,"VarietyMarvellous", signif = 4), 7.079)
  expect_equal(lme.p(fm1Oats.sum,"VarietyMarvellous"),0.47)
  expect_equal(lme.ps(fm1Oats.sum,"VarietyMarvellous"),"$p=0.47$")
})  


test_that("Valid values are returned from lm.value and friends", {
  ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
  trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
  sex <- as.factor(rep(c("f","m"),10))
  group <- gl(2,10,20, labels=c("Ctl","Trt"))
  weight <- c(ctl, trt)
  x <- summary(lm(weight ~ group*sex))
  expect_equal(lm.value(x,"sexm", signif = 1),0.3)
  expect_equal(lm.stderr(x,"sexm"),0.41)
  expect_equal(lm.p(x,"sexm", signif = 4),0.4382)
  expect_equal(lm.ps(x,"sexm", signif = 4),"$p=0.4382$")
})

test_that("latexSNDouble returns valid string", {
  expect_equal(latexSNdouble(1.342E-12),"1.342\\\\!\\\\times\\\\!10^{-12}")
})
  