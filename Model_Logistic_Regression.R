# title: Model_Logistic_Regression
# autuor: Ivy Yang & Joyce Xu
# version: 4.0
# output: .csv file & .txt file



# set work path
setwd("~/Documents/Capstone - Raw Data Backup/Logistic Regression")
# set model output file
file.output.model = paste("Logit_Reg_4_Interval_Model_test.txt", sep = "")
# set error rate output file
file.output.error = paste("Logit_Reg_4_Interval_Error_test.csv", sep = "")
# give model title
model.title = paste("Logit_Reg_4_Interval", sep = "")
# read data
data = read.csv("group_data_4_interval.csv", header = T)
# load package
require(nnet)

# define function to do regressions
reg = function(y, data.train) {
  # choose baseline
  data.train[, y] <- relevel(data.train[, y], ref = "low")
  
  # initial model: use no variable
  empty.text = paste(y, "~ 1", sep = " ")
  empty.model <- multinom(as.formula(empty.text), data.train)
  # full model: use all variable
  full.text = paste(y, "~ .", sep = " ")
  full.model <- multinom(as.formula(full.text), data.train)
  
  # both model
  reg.model <- step(empty.model, direction = "both", scope = formula(full.model))
  reg.model
}

# define function to predict
pre = function(y, data.test, train.reg) {
  test.p = predict(train.reg, data.test)
  test.a = data.test[, y]
  error = mean(test.p != test.a, na.rm = T)
  error
}

# get the valid data set for current model
set.full = function(y, data) {
  col = which(colnames(data) == y)
  data.full = data[, c(col, 38:48)]
  data.full = na.omit(data.full)
  data.full[, y] = as.factor(data.full[, y])
  data.full
}

# create cross validation training and testing sets
cross.validation.sets = function(y, data.full, k) {
  data.sets = list()
  for (i in 1:k) {
    set.seed(i)
    # split 10 fold data into training and testing sets
    random.test = sample(1:nrow(data.full), nrow(data.full)/k, replace = F)
    random.train = c(1:nrow(data.full))[-random.test]
    data.train = data.full[random.train, ]
    data.test = data.full[random.test, ]
    data.sets[[i]] = data.train
    data.sets[[i+k]] = data.test
  }
  data.sets
}

# predict all outputs one by one
k = 10
Output.predict.y = colnames(data)[49:50]
Output.predict.model = list()
Output.predict.error = list()
for (i in 1:length(Output.predict.y)){
  y = Output.predict.y[i]
  data.full = set.full(y, data)
  data.sets = cross.validation.sets(y, data.full, k)
  # cross validation
  errors = c()
  for(j in 1:k) {
    data.train = data.sets[[j]]
    data.test = data.sets[[j+k]]
    reg.train = reg(y, data.train)
    errors = c(errors, pre(y, data.test, reg.train))
  }
  error = mean(errors)
  # full model
  data.train = data.full
  reg.model = reg(y, data.train)
  # save results
  Output.predict.model[[i]] = reg.model
  Output.predict.error[[i]] = error
}
names(Output.predict.model) = Output.predict.y
names(Output.predict.error) = Output.predict.y

# output model
cat("Logistic Regression of 4 Levels with even intervals", file = file.output.model)
cat("\n\n", file = file.output.model, append = TRUE)
for (i in 1:length(Output.predict.model)) {
  cat(names(Output.predict.error)[i], file = file.output.model, append = TRUE)
  cat("\n", file = file.output.model, append = TRUE)
  capture.output(Output.predict.model[[i]], file = file.output.model, append = TRUE)
  cat("Misclassification Rate: ", file = file.output.model, append = TRUE)
  cat(Output.predict.error[[i]], file = file.output.model, append = TRUE)
  cat("\n\n----------------------------------------------------------------------------------\n\n", 
      file = file.output.model, append = TRUE)
}


# output error rate
Output.df = t(as.data.frame(Output.predict.error))
colnames(Output.df) = model.title
write.csv(Output.df, file.output.error, row.names = T)