data = read.csv("VanderbiltSched.csv")

# Q1
plot(data$T...7, data$Actual)

# Q2
linreg = lm(Actual~T...7, data=data)
abline(linreg)

# Q3
q3 = data.frame(data$DOW, data$Actual-data$T...1)
aggregate(q3[, 2], list(q3$data.DOW), mean)

# Q4

# Q5

# Q6

# Q7
q7 = data[, c(1, 2, 18, 19)]
q7_train = q7[0:225, ]
q7_test = q7[226:241, ]

q7_lm = lm(Actual~T...1, data=q7_train)

# Q8

# Q9
q9_data = data.frame(T...1 = q7_test$T...1)
q9_actual = data.frame(Actual = q7_test$Actual)
q9_pred = data.frame(predict(q7_lm, q9_data))

q9_pred
q9_actual
sq_diff = (q9_pred - q9_actual)^2
colMeans(sq_diff)

# The Mean Square Error is 19, meaning the average prediction is within 19 surgeries of the actual number


# Q10
# The predictions should be communicated as ball-park estimates.
# The MSE (19) provides the average amount a prediction is off of the actual number
# Staff scheduling should account for this uncertainty to ensure there are adequate personnel available to meet the demand.
