#####################################################################

# Make Table 1 with 95% CI for Continuous and Categorical Variables 
# Author: Wendy Wang
# Date: January 12th, 2024

#####################################################################

######### LOAD LIBRARY #########
library(binom)

######### INPUT YOUR INFORMATION HERE #########
mydata <- YOUR_DATASET_NAME
mydata$by_variable <- multianalysis$case # The columns of your table 1 are defined by levels of this variable
n0 <- table(mydata$by_variable)[1]
n1 <- table(mydata$by_variable)[2]
binary_variables <- c("sex", "mwash_ge1day", "income_c20", "hbp_med34","lipdrug", "fh_db") # List all your dichotomous variables here exactly the way they appear in the dataset
continuous_variables <- c("a2","METs_HPFS", "gr_alcdia","stress_score","waist_mean","bmi_calc","gluc_fast","homa_ir","insulin_fast","hba1c","CRP_N","chol_mgdl","trig_mgdl",
                          "hdl_mgdl","ldl_mgdl","il_6_pg_ml","icam_ng_ml","vcam_ng_ml","adiponectin_ug_ml") # List all your continuous variables here exactly the way they appear in the dataset 

######### RUN THE FUNCTION #########
# Define the main function that calculates 95% CI for both continuous and categorical variables
calculate_ci <- function(mean = NA, sd = NA, n = NA, count = NA, total = NA, data_type = "continuous") {
  # Initialize an empty list to store CI results
  ci <- list()
  
  if (data_type == "continuous") {
    # Calculate standard error
    se <- sd / sqrt(n)
    # Calculate CI
    error_margin <- 1.96 * se
    ci$lower <- mean - error_margin
    ci$upper <- mean + error_margin
  } else if (data_type == "categorical") {
    # Calculate CI for proportion using Wilson score interval
    ci_bounds <- binom.confint(count, total, conf.level = 0.95, methods = "wilson")
    ci$lower <- ci_bounds$lower
    ci$upper <- ci_bounds$upper
  } else {
    stop("Invalid data type specified. Use 'continuous' or 'categorical'.")
  }
  
  return(ci)
}

######### GET RESULTS #########

### Continuous variables ###
############################

# Calculate means and standard deviations of variables in Table 1 by disease status
means<- aggregate(mydata[continuous_variables], list (by_variable=mydata$by_variable), mean, na.rm=T)
means<-t(means)
means # Output means
sd<- aggregate(mydata[continuous_variables], list (by_variable=mydata$by_variable), sd, na.rm=T)
sd<-t(sd)

# Output 95% CI for continuous variables 
CI_continuous <- data.frame(matrix(NA,length(continuous_variables),9,dimnames=list(NULL,c("variable", "mean0", "sd0","LL_n0", "UL_n0", "mean1", "sd1","LL_n1", "UL_n1"))))

for (i in 1:length(continuous_variables)){
  print(continuous_variables[i])
  CI_continuous$variable[i] <- continuous_variables[i]
  CI_continuous$mean0[i] <- means[i+1,1]
  CI_continuous$sd0[i] <- sd[i+1,1]
  cici <- calculate_ci(mean = means[i+1,1], sd = sd[i+1,1], n = n0, data_type = "continuous")
  CI_continuous$LL_n0[i] <- cici[1]
  CI_continuous$UL_n0[i] <- cici[2]
  CI_continuous$mean1[i] <- means[i+1,2]
  CI_continuous$sd1[i] <- sd[i+1,2]
  cici <- calculate_ci(mean = means[i+1,2], sd = sd[i+1,2], n = n1, data_type = "continuous")
  CI_continuous$LL_n1[i] <- cici[1]
  CI_continuous$UL_n1[i] <- cici[2]
}
View(CI_continuous)

### Categorical variables ###
#############################

#### Binary Variables
binary_variables <- c("sex", "mwash_ge1day", "income_c20", "hbp_med34","lipdrug", "fh_db") # List all your binary variables here exactly the way they appear in the dataset

CI_binary <- data.frame(matrix(NA,length(binary_variables),9,dimnames=list(NULL,c("variable","n0","n0_prop","LL_n0", "UL_n0","n1","n1_prop","LL_n1", "UL_n1"))))

for (i in 1:length(binary_variables)){
  cat_count <- table(mydata[[binary_variables[i]]], mydata$by_variable)
  cat_prop <- prop.table(cat_count, 2)
  CI_binary$variable[i] <- binary_variables[i]
  CI_binary$n0[i] <- as.numeric(cat_count[2,1])
  CI_binary$n0_prop[i] <- as.numeric(cat_prop[2,1])
  cici <- calculate_ci(count =  CI_binary$n0[i], total = n0, data_type = "categorical")
  CI_binary$LL_n0[i] <- cici[1]
  CI_binary$UL_n0[i] <- cici[2]
  CI_binary$n1[i] <- as.numeric(cat_count[2,2])
  CI_binary$n1_prop[i] <- as.numeric(cat_prop[2,2])
  cici <- calculate_ci(count = as.numeric(cat_count[2,2]), total = n1, data_type = "categorical")
  CI_binary$LL_n1[i] <- cici[1]
  CI_binary$UL_n1[i] <- cici[2]
  print(binary_variables[i])
}
View(CI_binary)

#### Categorical Variables
# I did not put this into loops because I only had two of these, but you can either run this manually or put in a loop based on your own needs
cat_count <- table(multi$smoke_cat, multi$perio)
cat_count
prop.table(cat_count,2)
calculate_ci(count = as.numeric(cat_count[1,1]), total = n0, data_type = "categorical")
calculate_ci(count = as.numeric(cat_count[1,2]), total = n1, data_type = "categorical")
calculate_ci(count = as.numeric(cat_count[2,1]), total = n0, data_type = "categorical")
calculate_ci(count = as.numeric(cat_count[2,2]), total = n1, data_type = "categorical")
calculate_ci(count = as.numeric(cat_count[3,1]), total = n0, data_type = "categorical")
calculate_ci(count = as.numeric(cat_count[3,2]), total = n1, data_type = "categorical")

