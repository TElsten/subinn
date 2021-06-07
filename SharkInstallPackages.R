install.packages(
  c("class", "mlbench", "glmnet", "Matrix", "e1071", "ipred", "ESKNN", "kernlab", "rknn", "randomForest", "ElemStatLearn"), 
  Sys.getenv("R_LIBS_USER"), 
  repos="http://cran.case.edu"
)