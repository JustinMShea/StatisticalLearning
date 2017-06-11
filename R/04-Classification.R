library(ISLR)

# Logistic Regression example on `Default` credit card data
str(Default)
?Default

balance_default <- glm(default ~ balance, data = Default, family="binomial")
summary(balance_default)

student_default <- glm(default ~ student, data = Default, family="binomial")
summary(student_default)

# Logistic regression on `Smarket` stock market data
str(Smarket)
summary(Smarket)
?Smarket

pairs(Smarket, col=Smarket$Direction)


glm.fit <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume,
               data = Smarket, family = binomial)

summary(glm.fit)

glm.probs <- predict(glm.fit, type = "response") 
glm.probs[1:5]

glm.pred <- ifelse(glm.probs > 0.5, "Up", "Down")

table(glm.pred, Smarket$Direction)
mean(glm.pred==Smarket$Direction)

# Make training and test set
  train <- Smarket$Year < 2005
  
glm.fit <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume,
               data=Smarket, family=binomial, subset=train)

     glm.probs <- predict(glm.fit, newdata=Smarket[!train, ], type = "response") 
     glm.pred  <- ifelse(glm.probs > 0.5, "Up", "Down")
Direction.2005 <- Smarket$Direction[!train]

table(glm.pred, Direction.2005)
mean(glm.pred==Direction.2005)

#Fit smaller model
  glm.fit <- glm(Direction~Lag1+Lag2, data=Smarket, 
                 family=binomial, subset=train)
glm.probs <- predict(glm.fit, newdata=Smarket[!train, ], type = "response") 
 glm.pred <- ifelse(glm.probs > 0.5, "Up", "Down")

table(glm.pred, Direction.2005)
mean(glm.pred==Direction.2005)

106/(76+106)

predict(glm.fit,newdata=data.frame(Lag1=c(1.2,1.5),Lag2=c(1.1,-0.8)),type="response")

## Linear Discriminant Analysis
library(MASS)
lda.fit <- lda(Direction~Lag1+Lag2, data=Smarket, subset = Year<2005)
lda.fit
plot(lda.fit)

Smarket.2005 <- subset(Smarket, Year==2005)
lda.pred <- predict(lda.fit, Smarket.2005)
lda.pred[1:5,]
class(lda.pred)
data.frame(lda.pred)[1:5,]
table(lda.pred$class, Smarket.2005$Direction)
mean(lda.pred$class==Smarket.2005$Direction)


# Quadratic Discriminant Analysis
qda.fit <- qda(Direction ~ Lag1 + Lag2, data = Smarket, subset = train)
qda.fit

qda.class <- predict(qda.fit, Smarket.2005)$class

table(qda.class, Direction.2005)
mean(qda.class==Direction.2005)

## K-Nearest Neighbors
library(class)
?knn
attach(Smarket)
Xlag <- cbind(Lag1,Lag2)
train <- Year<2005
knn.pred <- knn(Xlag[train,], Xlag[!train,], Direction[train], k=1)
table(knn.pred, Direction[!train])
mean(knn.pred==Direction[!train])

# LDA Iris
data(iris)
str(iris)
