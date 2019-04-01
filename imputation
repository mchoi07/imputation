# reading data

bc_data = read.table("breast-cancer-wisconsin.data.txt", stringsAsFactors = FALSE, header = FALSE, sep = ',')
head(bc_data) 

for (i in 2:11) {
  print(paste0("V",i))
  print(table(bc_data[,i]))
}

# V7 is the only factor with missing data, where the missing data
# is represented with question marks.

# Show the observations with missing data.

bc_data[which(bc_data$V7 == "?"),]

nrow(bc_data[which(bc_data$V7 == "?"),])/nrow(bc_data)
# 0.02288984

# it's only 2.29% which is less than 5%. Therefore we could use data imputation

# Find and store the missing rows

missing_rows <- which(bc_data$V7 == "?", arr.ind = TRUE)
missing_rows

# Defining Mode Function 

getting_mode = function(value){
  unique_value = unique(value)
  unique_value[which.max(tabulate(match(value,unique_value)))]
}


# Getting Mode Value
mode_value = as.numeric(getting_mode(bc_data[-missing_rows,"V7"]))

# Imputing Missing Data to mode

imputation_mode = bc_data
imputation_mode[missing_rows,]$V7 = mode_value
imputation_mode$V7 = as.integer(imputation_mode$V7)
imputation_mode$V7[missing_rows]

# Getting Mean Value
mean_value = mean(as.numeric(bc_data[-missing_rows,"V7"]))
mean_value
# [1] 3.544656

# Imputing Missing Data to mean

imputation_mean = bc_data
imputation_mean[missing_rows,]$V7 = mean_value
imputation_mean$V7 = as.integer(imputation_mean$V7)
imputation_mean$V7[missing_rows]


# Imputing Missing Data using Regression Model
regression_data = bc_data[-missing_rows,2:10]
regression_data$V7 = as.integer(regression_feature$V7)

rm_model <- lm(V7~V2+V3+V4+V5+V6+V8+V9+V10, data = regression_data)
summary(rm_model)

final_rm_model <- lm(V7~V2+V4+V5+V8, data = regression_data)
summary(final_rm_model)

# Getting Predictive Value from Regerssion Model

predictive_value = predict(final_rm_model, newdata = bc_data[missing_rows,])
predictive_value

# Imputing Missing Data to Predictive Value

imputation_rm = bc_data
imputation_rm[missing_rows,]$V7 = predictive_value
imputation_rm$V7[missing_rows]

# Getting Perturb Value
perturb_value = rnorm(nrow(bc_data[missing_rows,]), predictive_value, sd(predictive_value))

# Imputing Missing Data to Predictive Value

imputation_perturb = bc_data
imputation_perturb[missing_rows,]$V7 = perturb_value
imputation_perturb$V7[missing_rows]
