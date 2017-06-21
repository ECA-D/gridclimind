load("exemplar_data.rda")

if(require("RUnit", quietly=TRUE)) {
  ## Run all the tests
  library(gridclimind)
  library(ncdf4)
  wd <- getwd()
  testsuite <- defineTestSuite("gridclimind", dirs=wd, testFileRegexp = "^test_.+.R$", testFuncRegexp = "^test.+")
  gridclimind.test.result <- runTestSuite(testsuite)
  printTextProtocol(gridclimind.test.result)
  stopifnot(gridclimind.test.result$gridclimind$nFail == 0 && gridclimind.test.result$gridclimind$nErr == 0)    
}
