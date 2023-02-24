#Q-1 - 
#a) 
#Reading data from csv file
data = read.csv("/Users/sirishasatish/Desktop/Stats 6313/mini projects code R_2022/miniproject 5/bodytemp-heartrate.csv")

#Creating 2 classes for male and female using subset()
male = subset(data, data$gender == 1)
female = subset(data, data$gender == 2)

#Summary statistics for body temperature
#MALE
summary(male$body_temperature)
#FEMALE
summary(female$body_temperature)

#Plotting box plots for body temperatures
boxplot(male$body_temperature, female$body_temperature, main = "Boxplots of Body Temperatures", names = c('Male', 'Women'), ylab = " Body Temperatures", col = "yellow")

#Plotting Q_Q Plots for body temperatures
par(mfrow=c(1,2))
qqnorm(male$body_temperature, main = 'Q-Q Plot for Males') 
qqline(male$body_temperature)
qqnorm(female$body_temperature, main = 'Q-Q Plot for Females')
qqline(female$body_temperature)

#Confidence Interval using t-test function for body temperature values
t.test(male$body_temperature, female$body_temperature, alternative =
         'two.sided', var.equal = F)

#b)
#Summary statistics for heart rates
#MALE
summary(male$heart_rate)
#FEMALE
summary(female$heart_rate)

#Plotting box plots for heart rates
boxplot(male$heart_rate, female$heart_rate, main = "Boxplots of Heart Rates", names = c('Male', 'Women'), ylab = "Heart Rates", col = "yellow")


#Plotting Q_Q Plots for heart rates
par(mfrow=c(1,2))
qqnorm(male$heart_rate, main = 'Q-Q Plot for Males') 
qqline(male$heart_rate)
qqnorm(female$heart_rate, main = 'Q-Q Plot for Females')
qqline(female$heart_rate)

#Confidence Interval using t-test function for body temperature values
t.test(male$heart_rate, female$heart_rate, alternative =
         'two.sided', var.equal = F)


#c)
#Plotting the scatter plots for the body temperature & heart rate values for males & females 
par(mfrow=c(1,2))
plot(male$heart_rate, male$body_temperature, pch=1, main='Scatter Plot for Male')
abline(lm(male$body_temperature~male$heart_rate))
plot(female$heart_rate, female$body_temperature, pch=1, main='Scatter Plot for Female')
abline(lm(female$body_temperature~female$heart_rate))

# Finding the correlation values between body temperatures and heart rates 
cor(male$body_temperature, male$heart_rate)
cor(female$body_temperature, female$heart_rate)


#2)
# Creating check_z_ci function - checks whether true mean exists within confidence interval 
# Simulating a sample and constructing an interval
check_z_ci <- function(n, lamda){
  u<-rexp(n,lamda)
  lbound <- mean(U) - qnorm(0.975) * sd(U) / sqrt(n) 
  ubound <- mean(U) + qnorm(0.975) * sd(U) / sqrt(n)
  tm = 1/lambda
  if(ubound > tm & lbound < tm){
    return (1)
  }
  else{
    return(0)
  }
}

# Creating function zproportion - compute coverage probabilities using checkztmci 
zproportion <- function(n, lambda){
  values <- replicate(5000, check_z_ci(n, lambda)) 
  ones <- values[which (values == 1)]
  return (length(ones)/5000)
}
zproportion(5,0.01)
  

