library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(caret)
library(VIM)

data <- read.csv("train-file.csv")
str(data)
head(data)
summary(data)

data <- data %>%
  mutate(Total_Income = ApplicantIncome + CoapplicantIncome)

str(data)


table(data$Loan_Amount_Term)

plot(data)
plot(data[, c("Loan_Status", "ApplicantIncome", "CoapplicantIncome", 
              "Total_Income", "LoanAmount","Loan_Amount_Term")])
table(data$Loan_Amount_Term)
table(data$Credit_History)

data$Loan_Amount_Term <- factor(data$Loan_Amount_Term)
data$Credit_History <- factor(data$Credit_History, labels = c("No", "Yes"))

data_sub <- data %>%
  select(-Loan_ID)

data_sub <- kNN(data_sub)

data_sub <- data_sub %>%
  select(-(Gender_imp:Loan_Status_imp))


data_sub %>%
  group_by(Loan_Status) %>%
  summarize(table(Credit_History))

data_sub %>%
  filter(Loan_Status == "Y" & Credit_History == "No") %>%
  length()

poor_credit


summary(data)

ggplot(data, aes(x=Total_Income, fill = Loan_Status)) + 
  geom_histogram()

ggplot(data, aes(x = ApplicantIncome, fill = Loan_Status)) +
  geom_histogram()

ggplot(data, aes(x = CoapplicantIncome, fill = Loan_Status)) +
  geom_histogram()

sum(data$CoapplicantIncome == 0)

table(data$Gender, data$Loan_Status)

ggplot(data, aes(x=Education, y = Total_Income, fill = Loan_Status)) +
  geom_boxplot()

ggplot(data, aes(x=Total_Income, y = LoanAmount, col = Loan_Status)) +
  geom_jitter(alpha = 0.7) +
  facet_grid(. ~ Credit_History) +
  labs(title = "Loan Amount by Total Income", 
       x = "Total Income", y="Loan Amount (Thousands)")

ggplot(data, aes(x=ApplicantIncome, y = LoanAmount, col = Loan_Status)) +
  geom_jitter(alpha = 0.7) +
  facet_grid(. ~ Credit_History) +
  labs(title = "Loan Amount by Applicant Income and Credit History",
       x = "Applicant Income", y = "Loan Amount (Thousands)")

ggplot(data, aes(x=CoapplicantIncome, y = LoanAmount, col = Loan_Status)) +
  geom_jitter(alpha = 0.7) +
  facet_grid(. ~ Credit_History)

ggplot(data, aes(x=Total_Income, y = LoanAmount, col = Loan_Status)) +
  geom_jitter(alpha = 0.7) +
  facet_grid(. ~ Gender) +
  labs(title = "Loan Amount by Total Income", 
       x = "Total Income", y="Loan Amount (Thousands)")

ggplot(data, aes(x=Total_Income, y = LoanAmount, col = Loan_Status)) +
  geom_jitter(alpha = 0.7) +
  facet_grid(. ~ Dependents) +
  labs(title = "Loan Amount by Total Income", 
       x = "Total Income", y="Loan Amount (Thousands)")

ggplot(data, aes(x=LoanAmount, y = Total_Income, col = Credit_History)) +
  geom_point()


inTrain <- createDataPartition(y = data_sub$Loan_Status, p = 0.8, list=FALSE)
train <- data_sub[inTrain, ]
test <- data_sub[-inTrain, ]



model_rf <- train(Loan_Status ~ ., train, 
                  method = "ranger",
                  weights = train$Total_Income,
                  trControl = trainControl(method = "cv", 
                                           number = 10, repeats = 10))
model_rf

predictions_rf <- predict(model_rf, test)
confusionMatrix(predictions_rf, test$Loan_Status)
