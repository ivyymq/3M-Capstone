# title: Model_Logistic_Regression
# autuor: Ivy Yang & Joyce Xu
# version: 3.0
# output: .csv file & .txt file



# set work path
path = "~/Documents/Capstone - Raw Data Backup"

# set input file
# file.input = "group_data_4_entry.csv"
# group.num = "4_Entry"
# file.subtitle = "Logistic Regression of 4 Levels with even entries"

# set input file
file.input = "group_data_4_entry_imputed.csv"
group.num = "4_Entry_Imputed"
file.subtitle = "Logistic Regression of 4 Levels with even entries after imputing all missing values with mean value"

# set input file
# file.input = "group_data_4_interval.csv"
# group.num = "4_Interval"
# file.subtitle = "Logistic Regression of 4 Levels with even intervals"

# set input file
# file.input = "group_data_4_interval.csv"
# group.num = "4_Interval"
# file.subtitle = "Logistic Regression of 4 Levels with even intervals"

# set work path
setwd(path)
# set model output file
file.output.model = paste("Logit_Reg_", group.num, "_Model.txt", sep = "")
# set error rate output file
file.output.error = paste("Logit_Reg_", group.num, "_Error.csv", sep = "")
# give model title
model.title = paste("Logit_Reg_", group.num, sep = "")
# read data
data = read.csv(file.input, header = T)
data$Domain_1_Score_group = NULL
# load package
require(nnet)

# define function to do regressions
reg = function(y, data.train) {
  # choose baseline
  data.train[, y] <- relevel(data.train[, y], ref = "Low")
  
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

# predict HAC scores
k = 10
HAC.predict.y = colnames(data)[49:53]
HAC.predict.model = list()
HAC.predict.error = list()
for (i in 1:length(HAC.predict.y)){
  y = HAC.predict.y[i]
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
  HAC.predict.model[[i]] = reg.model
  HAC.predict.error[[i]] = error
}
names(HAC.predict.model) = HAC.predict.y
names(HAC.predict.error) = HAC.predict.y

# predict HAI scores
HAI.predict.y = colnames(data)[54:65]
HAI.predict.model = list()
HAI.predict.error = list()
for (i in 1:length(HAI.predict.y)){
  y = HAI.predict.y[i]
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
  HAI.predict.model[[i]] = reg.model
  HAI.predict.error[[i]] = error
}
names(HAI.predict.model) = HAI.predict.y
names(HAI.predict.error) = HAI.predict.y

# predict RCD scores
RCD.predict.y = colnames(data)[66:84]
RCD.predict.model = list()
RCD.predict.error = list()
for (i in 1:length(RCD.predict.y)){
  y = RCD.predict.y[i]
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
  RCD.predict.model[[i]] = reg.model
  RCD.predict.error[[i]] = error
}
names(RCD.predict.model) = RCD.predict.y
names(RCD.predict.error) = RCD.predict.y

# output model
cat(file.subtitle, file = file.output.model)
cat("\n\n", file = file.output.model)
for (i in 1:length(HAC.predict.model)) {
  cat(names(HAC.predict.error)[i], file = file.output.model, append = TRUE)
  cat("\n", file = file.output.model, append = TRUE)
  capture.output(HAC.predict.model[[i]], file = file.output.model, append = TRUE)
  cat("Misclassification Rate: ", file = file.output.model, append = TRUE)
  cat(HAC.predict.error[[i]], file = file.output.model, append = TRUE)
  cat("\n\n----------------------------------------------------------------------------------\n\n", 
      file = file.output.model, append = TRUE)
}
for (i in 1:length(HAI.predict.model)) {
  cat(names(HAI.predict.error)[i], file = file.output.model, append = TRUE)
  cat("\n", file = file.output.model, append = TRUE)
  capture.output(HAI.predict.model[[i]], file = file.output.model, append = TRUE)
  cat("Misclassification Rate: ", file = file.output.model, append = TRUE)
  cat(HAI.predict.error[[i]], file = file.output.model, append = TRUE)
  cat("\n\n----------------------------------------------------------------------------------\n\n", 
      file = file.output.model, append = TRUE)
}
for (i in 1:length(RCD.predict.model)) {
  cat(names(RCD.predict.error)[i], file = file.output.model, append = TRUE)
  cat("\n", file = file.output.model, append = TRUE)
  capture.output(RCD.predict.model[[i]], file = file.output.model, append = TRUE)
  cat("Misclassification Rate: ", file = file.output.model, append = TRUE)
  cat(RCD.predict.error[[i]], file = file.output.model, append = TRUE)
  cat("\n\n----------------------------------------------------------------------------------\n\n", 
      file = file.output.model, append = TRUE)
}

# output error rate
HAC.df = t(as.data.frame(HAC.predict.error))
colnames(HAC.df) = model.title
HAI.df = t(as.data.frame(HAI.predict.error))
colnames(HAI.df) = model.title
HAC.df = t(as.data.frame(HAC.predict.error))
colnames(HAC.df) = model.title
RCD.df = t(as.data.frame(RCD.predict.error))
colnames(RCD.df) = model.title
DF = rbind(HAC.df, HAI.df, RCD.df)
write.csv(DF, file.output.error, row.names = T)