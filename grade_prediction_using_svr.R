library(e1071)
library(readxl)

library(hydroGOF)
# Enter your file path here
data<- read_excel("D:/R-files/Project/Student_Grade_Predictor/student_dataset.xlsx")
str(data)
head(data)


test_data <- read_excel("D:/R-files/Project/Student_Grade_Predictor/test_dataset.xlsx")
str(test_data)


                                ## Model for prediction of DSP Marks ##

dsp_attendance <- data["dsp_attendance"]
dsp_marks <- data["dsp_marks"]
test_dsp_data <- test_data["dsp_attendance"]

model<- svm(dsp_marks ~ dsp_attendance, data=data[1:34,],cost=64,epsilon=1)


tune_dsp_marks <- tune(svm, dsp_marks ~ dsp_attendance,  data = data[1:34,],
                       ranges = list(epsilon = seq(0,1,0.1), cost = 2^(2:9))
)

print(tune_dsp_marks)


# Draw the tuning graph
plot(tune_dsp_marks)

tune_dsp_marks <- tune(svm, dsp_marks ~ dsp_attendance,  data = data[1:34,],
                       ranges = list(epsilon = seq(0.6,1,0.01), cost = 2^(2:9))
) 

print(tune_dsp_marks)
plot(tune_dsp_marks)

model <- tune_dsp_marks$best.model


predict1<-predict(model,newdata=data[35:49,])

predict1

error<- dsp_marks[35:49,] - predict1


error_rmse <- sqrt(mean(error^2))
error_rmse

predict2<-predict(model,newdata=test_dsp_data[1:6,])

predict2

                                ## Model for prediction of CSS Marks ##


css_attendance <- data["css_attendance"]
css_attendance

css_marks <- data["css_marks"]
test_css_data <- test_data["css_attendance"]

model<- svm(css_marks ~ css_attendance, data=data[1:34,],cost=64,epsilon=1)


tune_css_marks <- tune(svm, css_marks ~ css_attendance,  data = data[1:34,],
                       ranges = list(epsilon = seq(0,1,0.1), cost = 2^(2:9))
)

print(tune_css_marks)


# Draw the tuning graph
plot(tune_css_marks)

tune_css_marks <- tune(svm, css_marks ~ css_attendance,  data = data[1:34,],
                       ranges = list(epsilon = seq(0.6,0.8,0.01), cost = 2^(2:9))
) 

print(tune_css_marks)
plot(tune_css_marks)

model <- tune_css_marks$best.model


predict1<-predict(model,newdata=data[35:49,])

predict1

error<- css_marks[35:49,] - predict1


error_rmse <- sqrt(mean(error^2))
error_rmse

predict2<-predict(model,newdata=test_css_data[1:6,])

predict2



                              ## Model for prediction of AI marks ##



ai_attendance <- data["ai_attendance"]
ai_attendance

ai_marks <- data["ai_marks"]
test_ai_data <- test_data["ai_attendance"]
test_ai_data

model<- svm(ai_marks ~ ai_attendance, data=data[1:34,],cost=64,epsilon=1)


tune_ai_marks <- tune(svm, ai_marks ~ ai_attendance,  data = data[1:34,],
                       ranges = list(epsilon = seq(0,1,0.1), cost = 2^(2:9))
)

print(tune_ai_marks)


# Draw the tuning graph
plot(tune_ai_marks)

tune_ai_marks <- tune(svm, ai_marks ~ ai_attendance,  data = data[1:34,],
                       ranges = list(epsilon = seq(0.4,1,0.01), cost = 2^(2:9))
) 

print(tune_ai_marks)
plot(tune_ai_marks)

model <- tune_ai_marks$best.model


predict1<-predict(model,newdata=data[35:49,])

predict1

error<- ai_marks[35:49,] - predict1


error_rmse <- sqrt(mean(error^2))
error_rmse

predict2<-predict(model,newdata=test_ai_data[1:6,])

predict2


                              ## Model for prediction of IP marks



ip_attendance <- data["ip_attendance"]
ip_attendance

ip_marks <- data["ip_marks"]
test_ip_data <- test_data["ip_attendance"]
test_ip_data

model<- svm(ip_marks ~ ip_attendance, data=data[1:34,],cost=64,epsilon=1)


tune_ip_marks <- tune(svm, ip_marks ~ ip_attendance,  data = data[1:34,],
                      ranges = list(epsilon = seq(0,1,0.1), cost = 2^(2:9))
)

print(tune_ip_marks)


# Draw the tuning graph
plot(tune_ip_marks)

tune_ip_marks <- tune(svm, ip_marks ~ ip_attendance,  data = data[1:34,],
                      ranges = list(epsilon = seq(0.8,1,0.01), cost = 2^(2:9))
) 

print(tune_ip_marks)
plot(tune_ip_marks)

model <- tune_ip_marks$best.model


predict1<-predict(model,newdata=data[35:49,])

predict1

error<- ip_marks[35:49,] - predict1


error_rmse <- sqrt(mean(error^2))
error_rmse

predict2<-predict(model,newdata=test_ip_data[1:6,])

predict2
