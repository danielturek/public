library(RCurl)
library(rstan)
Ch17_code_stan <- getURL("https://raw.githubusercontent.com/stan-dev/example-models/master/ARM/Ch.17/17.4_multilevel_logistic_regression.R")

## Replace any "\r"s at the end of lines (happens on Windows) with "\n"s
Ch17_code_stan <- gsub("\r","\n",Ch17_code_stan)
writeLines(Ch17_code_stan, con = "Ch17_code_stan_from_github.R") ## tons of whitespace
## Split the lines for parsing and evaluation
Ch17_code_stan_lines <- strsplit(Ch17_code_stan, "\n")[[1]]
## Find the line number to stop at, after dataList.1 has been created
stopline <- grep("dataList.1", Ch17_code_stan_lines)[2]-1
## parse and eval the lines we need (and assign to a variable to avoid long screen output)
writeLines(Ch17_code_stan[1:stopline], con = "Ch17_code_stan_from_github_lines_to_run.R")
avoidOutput <- eval(parse(text = Ch17_code_stan_lines[1:stopline]))

save.image(file = "data_setup_from_stan_github.RData")
