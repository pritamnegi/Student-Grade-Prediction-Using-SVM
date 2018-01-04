library(e1071)
library(readxl)

# Enter your file path here
data<- read_excel("D:/R-files/Project/Student_Grade_Predictor/student_dataset.xlsx")
str(data)


                                ## Model for prediction of DSP Marks ##


model<- svm(data$dsp_marks ~ data$dsp_attendance, data)

predicted_dsp_marks <- predict(model, data)

plot(data$dsp_attendance, predicted_dsp_marks, col = "red", pch=4)

error_dsp_marks <- data$dsp_marks - predicted_dsp_marks

error_dsp_marks



# perform a grid search
tune_dsp_marks <- tune(svm, data$dsp_marks ~ data$dsp_attendance,  data = data,
                   ranges = list(epsilon = seq(0,1,0.1), cost = 2^(2:9))
)
print(tune_dsp_marks)
# Draw the tuning graph
plot(tune_dsp_marks)


tune_dsp_marks <- tune(svm, data$dsp_marks ~ data$dsp_attendance,  data = data,
                   ranges = list(epsilon = seq(0.4,0.8,0.01), cost = 2^(2:9))
) 

print(tune_dsp_marks)
plot(tune_dsp_marks)


tuned_dsp_marks_model <- tune_dsp_marks$best.model
tuned_dsp_marks <- predict(tuned_dsp_marks_model, data) 

tuned_dsp_marks

error <- data$dsp_marks - tuned_dsp_marks  




                                ## Model for prediction of CSS Marks ##


model<- svm(data$css_marks ~ data$css_attendance, data)

predicted_css_marks <- predict(model, data)

plot(data$css_attendance, predicted_css_marks, col = "red", pch=4)

error_css_marks <- data$css_marks - predicted_css_marks

error_css_marks



# perform a grid search
tune_css_marks <- tune(svm, data$css_marks ~ data$css_attendance,  data = data,
                       ranges = list(epsilon = seq(0,1,0.1), cost = 2^(2:9))
)
print(tune_css_marks)

# Draw the tuning graph
plot(tune_css_marks)


tune_css_marks <- tune(svm, data$css_marks ~ data$css_attendance,  data = data,
                       ranges = list(epsilon = seq(0,0.4,0.01), cost = 2^(2:9))
) 

print(tune_css_marks)
plot(tune_css_marks)


tuned_css_marks_model <- tune_css_marks$best.model
tuned_css_marks <- predict(tuned_css_marks_model, data) 

tuned_css_marks

error <- data$css_marks - tuned_css_marks  




                              ## Model for prediction of AI marks ##



model<- svm(data$ai_marks ~ data$ai_attendance, data)

predicted_ai_marks <- predict(model, data)

plot(data$ai_attendance, predicted_ai_marks, col = "red", pch=4)

error_ai_marks <- data$ai_marks - predicted_ai_marks

error_ai_marks



# perform a grid search
tune_ai_marks <- tune(svm, data$ai_marks ~ data$ai_attendance,  data = data,
                       ranges = list(epsilon = seq(0,1,0.1), cost = 2^(2:9))
)
print(tune_ai_marks)

# Draw the tuning graph
plot(tune_ai_marks)


tune_ai_marks <- tune(svm, data$ai_marks ~ data$ai_attendance,  data = data,
                       ranges = list(epsilon = seq(0.4,0.6,0.01), cost = 2^(2:9))
) 

print(tune_ai_marks)
plot(tune_ai_marks)


tuned_ai_marks_model <- tune_ai_marks$best.model
tuned_ai_marks <- predict(tuned_ai_marks_model, data) 

tuned_ai_marks

error <- data$ai_marks - tuned_ai_marks  



                              ## Model for prediction of IP marks



model<- svm(data$ip_marks ~ data$ip_attendance, data)

predicted_ip_marks <- predict(model, data)

plot(data$ip_attendance, predicted_ip_marks, col = "red", pch=4)

error_ip_marks <- data$ip_marks - predicted_ip_marks

error_ip_marks



# perform a grid search
tune_ip_marks <- tune(svm, data$ip_attendance, predicted_ip_marks,  data = data,
                      ranges = list(epsilon = seq(0,1,0.1), cost = 2^(2:9))
)
print(tune_ip_marks)

# Draw the tuning graph
plot(tune_ip_marks)


tune_ip_marks <- tune(svm, data$ip_marks ~ data$ip_attendance,  data = data,
                      ranges = list(epsilon = seq(0,0.2,0.01), cost = 2^(2:9))
) 

print(tune_ip_marks)
plot(tune_ip_marks)


tuned_ip_marks_model <- tune_ip_marks$best.model
tuned_ip_marks <- predict(tuned_ip_marks_model, data) 

tuned_ip_marks

error <- data$ip_marks - tuned_ip_marks  



