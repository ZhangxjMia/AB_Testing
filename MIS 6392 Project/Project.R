library(pacman)
pacman::p_load(readxl, ggplot2, dplyr, caret, pwr, MatchIt, lattice)
options(scipen = 999)
options(digits = 3)
options(warn=0)
set.seed(123)

#-----------------------Load data -----------------------------
mydata <- as.data.frame(read_excel("Project Survey.xlsx"))
View(mydata)


#-------------------Data Preparation --------------------------
str(mydata)

#Average GPA for each major
mydata %>%
  group_by(major) %>%
  summarise_at(vars(GPA), funs(mean(., na.rm = TRUE)))

#Convert gender, resource, job/intern into binary variables. Label encoding. 
mydata$gender <- ifelse(mydata$gender == "Female", 1, 0)
table(mydata$gender)

mydata$resource <- ifelse(mydata$resource == "Yes", 1, 0)
table(mydata$resource)

mydata$job_intern <- ifelse(mydata$job_intern == "Yes", 1, 0)
table(mydata$job_intern)

#Convert major, degree, enroll_sem, age
table(mydata$major)
mydata$major %>%
  factor(levels = c("BA", "CS", "ECN", "FIN", "ITM", "SCM"))

table(mydata$degree)
mydata$degree %>%
  factor(levels = c("Bachelor", "Master", "PhD"), labels = c('B', 'M', 'P'))

table(mydata$enroll_sem)
mydata$enroll_sem %>%
  factor(levels = c("Fall 2017", "Fall 2018", "Fall 2019", "Spring 2018", "Spring 2019", "Spring 2020"),
         labels = c(1, 2, 3, 4, 5, 6))

table(mydata$age)
mydata$age %>%
  factor(levels = c("19 or less", "20 ~ 25", "25 ~ 30", "30 +"),
         labels = c(0, 1, 2, 3))

table(mydata$hours)
mydata$hours %>%
  factor(levels = c("0", "< 1hr", "1~2 hrs", "2~3 hrs", "4 hrs +"),
         labels = c(0, 1, 2, 3, 4))


#---------------Set Treatment Group & Control Group ------------------

#Treatment Group: individuals who use online resources, resource = 1
treat <- mydata %>% subset(resource == 1)
mean(treat$GPA) #3.65
sd(treat$GPA) #0.266

#Control Group: individuals who don't use online resources, resource = 0
ctrol <- mydata %>% subset(resource == 0)
mean(ctrol$GPA) #3.47
sd(ctrol$GPA) #0.229

#----------------------------Power Analysis----------------------------------------
cal_d = (mean(treat$GPA) - mean(ctrol$GPA))/sqrt(((sd(treat$GPA)^2) + (sd(ctrol$GPA)^2))/2)
pwr.t.test(n = NULL, d = cal_d, sig.level = 0.05, power = 0.9, 
           type = "two.sample", alternative = "two.sided")
#The power is the likelihood of finding statistical significance.


#--------------------------Propensity Score Matching----------------------------
mymatch = matchit(resource ~ major + degree + enroll_sem + gender + age + GPA + hours + 
                    job_intern, data = mydata, method = "nearest", ratio = 1)
summary(mymatch)


#-------------------------Analysis-----------------------------
#Obtain the mached data
matched_data <- match.data(mymatch)
View(matched_data)

histogram(~ GPA | factor(resource), data = matched_data, col = "steelblue", 
          main = "GPA Distribution")

matched_data %>% ggplot(aes(x = resource, y = GPA, group = factor(resource))) +
                  geom_boxplot(aes(colour = factor(resource)))

#Welch's t-test
t.test(GPA ~ resource, data = matched, var.equal = FALSE, conf.level = 0.95)
