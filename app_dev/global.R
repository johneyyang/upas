# source files
  source("upas_functions")
# load test data
  upas <- load_multifile("data", "*")
# clean test data
  upas <- upas_process(upas)