context("DMlatex output test")

test_that("summary.formula.reverse returns latex code",{
  set.seed(173) # so can replicate results
  sex =  factor(sample(c("m","f"), 500, rep=T))
  age =  rnorm(500, 50, 5)
  treatment =  factor(sample(c("Drug","Placebo"), 500, rep=T))
  x = summary(treatment~age+sex,method="reverse")
  lt = capture.output(DMlatex(
    x, caption="This is the caption",label="tab.summary.formula.reverse",file=""))
  expect_equal(length(lt),16)
  expect_match(lt[14],
    "\\\\caption\\{This is the caption\\\\label\\{tab.summary.formula.reverse\\}\\}")
})

test_that("DMlatex returns latex code",{
  set.seed(173) # so can replicate results
  sex =  factor(sample(c("m","f"), 500, rep=T))
  age =  rnorm(500, 50, 5)
  x= summary(age~sex)
  lt = capture.output(DMlatex(x,caption="hallo",label="tab",file=""))
  expect_equal(length(lt),19)
  expect_match(lt[17],
               "\\\\caption\\{hallo~~~~~N=500\\\\label\\{tab\\}\\}")
})


test_that("DMlatexSummary with noQuantiles==TRUE returns latex code without quantiles",{
  lt = capture.output(DMlatexSummaryby(warpbreaks,noQuantiles=TRUE))
  expect_equal(length(lt),84)
  expect_equal(lt[78],"breaks&$13$&$17$&$19$&$28$&$9$&$5$&$1.7$\\tabularnewline")
})



