context("Test Utility Functions")

data(mtcars)
variable_types <- lapply(mtcars,guess_variable_type,0.25)
expected_variable_types <- list(mpg = "continuous", cyl = "categorical", disp = "continuous", 
                                hp = "continuous", drat = "continuous", wt = "continuous", 
                                qsec = "continuous", vs = "binomial", am = "binomial", gear = "categorical", 
                                carb = "categorical")
expect_equal(variable_types,expected_variable_types)
