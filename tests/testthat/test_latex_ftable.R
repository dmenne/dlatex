test_that("latex on an ftable object give correct latex output", {
  lt = capture.output(latex(ftable(Survived ~ ., data = Titanic),
            mincapwidth=100, label="tab:tit", caption="Titanic survivors"))
  expect_equal(length(lt),27)
  expect_match(lt[15],"&154& 14")
})
