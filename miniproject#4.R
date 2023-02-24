#Q1 - Reading the data into gpa_data
gpa_data <- read.csv("/Users/sirishasatish/Desktop/Stats 6313/mini projects code R_2022/miniproject 4/gpa.csv")

gpa_val<-as.numeric(gpa_data$gpa)
act_val<-as.numeric(gpa_data$act)

#Plotting the scatterplot
plot(gpa_val,act_val,main="Scatterplot of GPA and ACT",xlab='GPA',ylab='ACT') 

abline(lm(act_val~gpa_val))

#Correlation between GPA and ACT
cor(gpa_val,act_val)

#Creating a statistical function for correlation

library(boot) 
covariance<-function(gpa_data,indices){ 
  f_gpa<-gpa_data$gpa[indices]
  f_act<-gpa_data$act[indices]
  result <- cor(f_gpa, f_act)
  return(result)
}

#using bootstrap estimation 
cov_boot<-boot(gpa_data, covariance, R=999, sim ="ordinary", stype = 'i')
cov_boot

#Calculating point estimate
names(cov_boot)
mean(cov_boot$t)

#calculating the CI
boot.ci(cov_boot)

#Verifying the computed CI
sort(cov_boot$t)[c(25,975)]



#Q-2)
#Reading the data into volt_data and store into 2 seperate variables
volt_data<-read.csv("/Users/sirishasatish/Desktop/Stats 6313/mini projects code R_2022/miniproject 4/VOLTAGE.csv")
remote<-volt_data$voltage[which(volt_data$location==0)]
local<-volt_data$voltage[which(volt_data$location==1)]

#plotting box plot
boxplot(volt_data, remote, main="Boxplot of local and remote locations.", names = c("Local Location","Remote Location"),range = 1.5)

#Summary statistics of voltage values at local and remote locations.
summary(local)
summary(remote)

#Plotting QQ plots for the voltage values at local and remote locations.
par(mfrow=c(1,2))
qqnorm(local,main="Local")
qqline(local)
qqnorm(remote, main="Remote")
qqline(remote)

#Calculating mean, variance, standard error and CI of the voltafge dataset

var(remote)
var(local)
std_err<-sqrt(var(local)/30+var(remote)/30)
std_err
d_mean<-mean(remote)-mean(local)
d_mean+c(-1,1)*qnorm(0.975)*std_err
d_mean


#Q-3)
#Reading the data into vapor_data
vapor_data<-read.csv("/Users/sirishasatish/Desktop/Stats 6313/mini projects code R_2022/miniproject 4/VAPOR.csv")
theo_vap<-vapor_data$theoretical
exp_vap<-vapor_data$experimental

#Plotting QQ Plots
par(mfrow=c(1,2))
qqnorm(theo_vap,main="Theoretical")
qqline(theo_vap)
qqnorm(exp_vap, main="Experimental")
qqline(exp_vap)

#Plotting the boxplot and determining the summary statistics
boxplot(theo_vap,exp_vap, names = c("Theoretical Values","Experimental Values"), main = "Box plot of Theoretical and Experimental Values")

#Summary statistics
summary(theo_vap)

summary(exp_vap)

#Calculation of mean, sd, CI & t(n-1) value of the difference between theoretical and experimental values
vap_diff<-theo_vap-exp_vap
vap_diff

mean(vap_diff)

sd(vap_diff)