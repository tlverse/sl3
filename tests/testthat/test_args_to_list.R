context("args_to_list")

my_function <- function(arg1="default_1", arg2="default_2", ...){
  return(args_to_list())
}

arglist <- my_function(arg2="specified_2", dot_arg="test")

expect_equal(arglist, list(arg1 = "default_1", arg2 = "specified_2", dot_arg = "test"))

arglist <- my_function(arg3=list(a=2, b=list(c=1, d=3)))
expect_equal(arglist, 
             list(arg1 = "default_1", arg2 = "default_2", arg3 = list(
               a = 2, b = list(c = 1, d = 3))))

my_function2 <- function(arg1="default_1", arg2="default_2"){
  return(args_to_list())
}

arglist2 <- my_function2(arg2="specified_2")
expect_equal(arglist2, list(arg1 = "default_1", arg2 = "specified_2"))

wrapper_function <- function(){
  a=1
  my_function(arg1=a)
}

a=2
arglist <- wrapper_function()
expect_equal(arglist$arg1,1)

my_function_defaults=function(arg1=1, arg2=arg1){
  return(args_to_list())
}

arglist <- my_function_defaults()
expect_equal(arglist, list(arg1 = 1, arg2 = 1))
