#===== 1. Przygotowanie danych =====
library('ggplot2')

# Wczytanie danych z pliku csv

data<-read.csv(file="skoda.csv")

head(data)
summary(data)
#pairs(data)

# problem z enginesize (co to znaczy == 0?)
data$engineSize <- as.factor(data$engineSize)
table(data$engineSize)
index <- which(data$engineSize == 0)
data[index,]

# wydadało się że to jest samochód elektryczny, ale fuel type nie wskazuje na to
# usuwamy te obserwacje z enginesize = 0
data2 <- data[-index,]
row.names(data2) <- NULL # reset indeksów

summary(data2)


# kolejny problem: coś kosztuje 91874?
# kilka obserwacji posortowanych po cenie
head(data2[order(data2$price,decreasing = TRUE),])

# kilka obserwacji modelu Karoq posortowanych po cenie
index <- which(data2$model==" Karoq")
temp <- data2[index,]
temp <- temp[order(temp$price, decreasing = TRUE),]
temp[1:10,]

#usuwamy tamto (prawdopodobnie błąd - za bardzo odstaje od reszty)
index <- which(data2$price==91874)

data3 <- data2[-index,]
row.names(data3) <- NULL

summary(data3)


# year jako wiek samochodu od roku 2020 (tworzymy nową zmienną)
table(data3$year)
data3$age <- 2020 - data3$year
head(data3)


# zmienne jakościowe

#qplot(data3$model, ylab = 'Count', xlab = 'Model')
table(data3$model)

table(data3$transmission)
#barplot(table(data3$transmission))

table(data3$fuelType)
#barplot(table(data3$fuelType))

table(data3$engineSize)
#barplot(table(data3$engineSize))

data3[which(data3$engineSize==2.5),]


# dane gotowe
data_clean <- data3[,-2]
head(data_clean)
summary(data_clean)
names(data_clean)



#===== 2. Wykresy korelacji, zaleznosci =====
qplot(data_clean$price, xlab = 'Price', ylab = 'Count', main='Frequency Histogram: Price')

library('corrplot')
cor_mat <- cor(data_clean[,-c(1,3,5,8)])

corrplot(cor_mat, method = 'color')
corrplot(cor_mat, method = 'number')


# wykresy zależności i braku zależności
ggplot(data = data_clean, aes(x = age, y = price)) +
  geom_point() +
  xlab('Age') +
  ylab('Price') +
  ggtitle('Age vs. Price')

ggplot(data = data_clean, aes(x = mileage, y = price)) +
  geom_point() +
  xlab('Mileage') +
  ylab('Price') +
  ggtitle('Mileage vs. Price')

ggplot(data = data_clean, aes(x = mpg, y = price)) +
  geom_point() +
  xlab('MPG') +
  ylab('Price') +
  ggtitle('MPG vs. Price')

## odstajace wyzej?
data3[which(data3$mpg > 83), ][order(data3[which(data3$mpg > 83), ]$mpg, decreasing = TRUE), ]

ggplot(data = data_clean, aes(x = tax, y = price)) +
  geom_point() +
  xlab('Tax') +
  ylab('Price') +
  ggtitle('Tax vs. Price')

ggplot(data = data_clean, aes(x = model, y = price)) +
  geom_point() +
  xlab('Model') +
  ylab('Price') +
  ggtitle('Model vs. Price')

ggplot(data = data_clean, aes(x = transmission, y = price)) +
  geom_point() +
  xlab('Transmission') +
  ylab('Price') +
  ggtitle('Transmission vs. Price')

ggplot(data = data_clean, aes(x = fuelType, y = price)) +
  geom_point() +
  xlab('Fuel type') +
  ylab('Price') +
  ggtitle('Fuel type vs. Price')

ggplot(data = data_clean, aes(x = engineSize, y = price)) +
  geom_point() +
  xlab('Engine size') +
  ylab('Price') +
  ggtitle('Engine size vs. Price')



#===== 3. Przygotowanie danych do budowania modeli =====

# dummies (poradzenie sobie ze zmniennymi factor)
# wyczyszczenie danych tekstowych z białych znaków (był problem z model)
library(stringr)
data_clean$model <- str_trim(data_clean$model)
data_clean$transmission <- str_trim(data_clean$transmission)
data_clean$fuelType <- str_trim(data_clean$fuelType)

names(data_clean)
unique(data_clean$model)


# teraz robimy dummies ręcznie, zamiana jakościowych na binarne
unique(data_clean$model)
data_clean$model_octavia <- ifelse(data_clean$model == "Octavia", 1, 0)
data_clean$model_citigo <- ifelse(data_clean$model == "Citigo", 1, 0)
data_clean$model_yeti_out <- ifelse(data_clean$model == "Yeti Outdoor", 1, 0)
data_clean$model_superb <- ifelse(data_clean$model == "Superb", 1, 0)
data_clean$model_kodiaq <- ifelse(data_clean$model == "Kodiaq", 1, 0)
data_clean$model_rapid <- ifelse(data_clean$model == "Rapid", 1, 0)
data_clean$model_karoq <- ifelse(data_clean$model == "Karoq", 1, 0)
data_clean$model_fabia <- ifelse(data_clean$model == "Fabia", 1, 0)
data_clean$model_yeti <- ifelse(data_clean$model == "Yeti", 1, 0)
data_clean$model_scala <- ifelse(data_clean$model == "Scala", 1, 0)
data_clean$model_roomster <- ifelse(data_clean$model == "Roomster", 1, 0)
data_clean$model_kamiq <- ifelse(data_clean$model == "Kamiq", 1, 0)

test <- data_clean[,c(10:21)]
sum(test) #wychodzi liczba obserwacji data_clean czyli ok

unique(data_clean$transmission)
data_clean$transmission_manual <- ifelse(data_clean$transmission == "Manual", 1, 0)
data_clean$transmission_automatic <- ifelse(data_clean$transmission == "Automatic", 1, 0)
data_clean$transmission_semiauto <- ifelse(data_clean$transmission == "Semi-Auto", 1, 0)
data_clean$transmission_other <- ifelse(data_clean$transmission == "Other", 1, 0)

test <- data_clean[,c(22:25)]
sum(test) #wychodzi liczba obserwacji data_clean czyli ok


unique(data_clean$fuelType)
data_clean$fuelType_petrol <- ifelse(data_clean$fuelType == "Petrol", 1, 0)
data_clean$fuelType_diesel <- ifelse(data_clean$fuelType == "Diesel", 1, 0)
data_clean$fuelType_hybrid <- ifelse(data_clean$fuelType == "Hybrid", 1, 0)
data_clean$fuelType_other <- ifelse(data_clean$fuelType == "Other", 1, 0)

test <- data_clean[,c(26:29)]
sum(test) #wychodzi liczba obserwacji data_clean czyli ok

#unique(data_clean$engineSize)
data_clean$engineSize_2.0 <- ifelse(data_clean$engineSize == 2.0, 1, 0)
data_clean$engineSize_1.2 <- ifelse(data_clean$engineSize == 1.2, 1, 0)
data_clean$engineSize_1.6 <- ifelse(data_clean$engineSize == 1.6, 1, 0)
data_clean$engineSize_1.0 <- ifelse(data_clean$engineSize == 1.0, 1, 0)
data_clean$engineSize_1.5 <- ifelse(data_clean$engineSize == 1.5, 1, 0)
data_clean$engineSize_1.4 <- ifelse(data_clean$engineSize == 1.4, 1, 0)
data_clean$engineSize_1.9 <- ifelse(data_clean$engineSize == 1.9, 1, 0)
data_clean$engineSize_1.8 <- ifelse(data_clean$engineSize == 1.8, 1, 0)
data_clean$engineSize_2.5 <- ifelse(data_clean$engineSize == 2.5, 1, 0)
test <- data_clean[,c(30:38)]
sum(test) #wychodzi liczba obserwacji data_clean czyli ok

#usuwamy niepotrzebne kolumny
#names(data_clean)
data_clean2 <- data_clean[,-c(1,3,5,8)]
names(data_clean2)


set.seed(123) #Ustawiamy seed dla powtarzalności wyników

permuted_indices <- sample(nrow(data_clean2)) #Losowe permutowanie indeksów
data_clean2 <- data_clean2[permuted_indices,]
length(data_clean2$price)*0.75
  
# Tworzymy zbiór uczący i testowy:
train_indices <- 1:4696
test_indices <- 4697:6261

train <- data_clean2[train_indices,]
test <- data_clean2[-train_indices,]

head(train)




#==== MODELE =====

#===== 4. Modele liniowe =====
names(train)

model1 <- glm(price ~ 1, data = train)
model2 <- glm(price ~ age, data = train)
model3 <- glm(price ~ age + mileage, data = train)
model4 <- glm(price ~., data = train)
model5 <- step(model4, direction = "backward",)
model6 <- step(model1, direction = "forward", scope = formula(model4))

model <- list(model1, model2, model3, model4, model5, model6)


# Wykonujemy 10-CV by wybrać najlepszy model
library('boot')
errors <- c()

for(i in 1:length(model)){
  errors[i] <- cv.glm(train, model[[i]], K=10)$delta[1]
  cat("Model", i, "MSE:", errors[i], "\n")
}

errors
# Model 1 MSE: 39680045 
# Model 2 MSE: 24805003 
# Model 3 MSE: 24652933 
# Model 4 MSE: 3116636 
# Model 5 MSE: 3130760 
# Model 6 MSE: 3113791 

length(names(model4$coefficients)) #34
length(names(model5$coefficients)) #27
length(names(model6$coefficients)) #25

# model pełny (model4) oraz metoda forward (model6) dają podobne wyniki
# wybieramy model6 ze względu na to, że używa mniej zmiennych

# Formuła wybranego modelu
formula(model6)
# price ~ age + engineSize_1.0 + model_kodiaq + engineSize_2.0 + 
#   transmission_manual + model_karoq + model_superb + mileage + 
#   model_kamiq + fuelType_hybrid + mpg + fuelType_diesel + model_octavia + 
#   model_scala + model_yeti_out + engineSize_1.9 + engineSize_1.6 + 
#   model_citigo + tax + model_yeti + engineSize_2.5 + engineSize_1.2 + 
#   engineSize_1.8 + transmission_automatic


# Oceniamy wybrany model, testując na zbiorze testowym.
# Użyjemy dwóch metryk do oceny jakości tego i następnych modelów.
# Standardowo błąd średniokwadratowy MSE
# Dodatkowo błąd absolutny MAE, użyteczy z biznesowego punktu widzenia (pokazuje realną różnicę błędu ceny)
prediction <- predict.glm(model6, test)

mse_lm <- mean((prediction-test$price)**2)
mse_lm
# 2778571


mae_lm <- mean(abs(prediction-test$price))
mae_lm
# 1250.026


#===== 5. Bootstrap =====

# Następnie wykonamy boostrap na wybranym przed chwilą modelu
boot.fn <- function(data, index){
  coef(glm(price ~ age + engineSize_1.0 + model_kodiaq + engineSize_2.0 + 
             transmission_manual + model_karoq + model_superb + mileage + 
             model_kamiq + fuelType_hybrid + mpg + fuelType_diesel + model_octavia + 
             model_scala + model_yeti_out + engineSize_1.9 + engineSize_1.6 + 
             model_citigo + tax + model_yeti + engineSize_2.5 + engineSize_1.2 + 
             engineSize_1.8 + transmission_automatic, data = data, subset = index))
}

len_train <- length(train$price) ##wielkość traina

# estymacja współczynników - pełny zbiór danych
a <- boot.fn(train, 1:len_train)
a
# (Intercept)                    age         engineSize_1.0 
# 2.591990e+04          -9.703157e+02          -7.714856e+02 
# model_kodiaq         engineSize_2.0    transmission_manual 
# 7.737319e+03           2.429251e+03          -1.394680e+03 
# model_karoq           model_superb                mileage 
# 5.636370e+03           4.129788e+03          -6.195652e-02 
# model_kamiq        fuelType_hybrid                    mpg 
# 4.195435e+03           3.591243e+04          -1.758650e+02 
# fuelType_diesel          model_octavia            model_scala 
# 1.129562e+03           1.996188e+03           1.933217e+03 
# model_yeti_out         engineSize_1.9         engineSize_1.6 
# 9.766555e+02           5.943008e+03           9.669544e+02 
# model_citigo                    tax             model_yeti 
# -5.867477e+02          -2.859166e+00           5.760908e+02 
# engineSize_2.5         engineSize_1.2         engineSize_1.8 
# -4.932997e+03           2.405498e+02          -1.791138e+03 
# transmission_automatic 
# 1.395829e+02 

# estymacja współczynników - wersja boostrapowa
b <- boot.fn(train, sample(len_train,len_train, replace=T))
b
# (Intercept)                    age         engineSize_1.0 
# 2.481910e+04          -9.743619e+02          -7.499735e+02 
# model_kodiaq         engineSize_2.0    transmission_manual 
# 8.124672e+03           2.460546e+03          -1.357863e+03 
# model_karoq           model_superb                mileage 
# 5.913695e+03           4.298234e+03          -5.965752e-02 
# model_kamiq        fuelType_hybrid                    mpg 
# 4.472777e+03           3.406863e+04          -1.585908e+02 
# fuelType_diesel          model_octavia            model_scala 
# 7.839904e+02           2.150523e+03           2.107771e+03 
# model_yeti_out         engineSize_1.9         engineSize_1.6 
# 1.096748e+03           6.690144e+03           9.568198e+02 
# model_citigo                    tax             model_yeti 
# -6.695151e+02          -2.489090e+00           4.847954e+02 
# engineSize_2.5         engineSize_1.2         engineSize_1.8 
# -4.449212e+03           2.504592e+02          -1.725806e+03 
# transmission_automatic 
# -7.333366e+00 

summary(abs(a-b))
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
# 0.0023   21.5121  120.0920  263.9284  277.3421 1843.7931 


# estymacja odchyleń metodą bootstap
boot.est <- boot(train, boot.fn, 1000)
boot.est
# Bootstrap Statistics :
#   original       bias     std. error
# t1*   2.591990e+04  -1.04816883 4.393920e+02
# t2*  -9.703157e+02   0.65236602 3.441066e+01
# t3*  -7.714856e+02  -1.55625695 1.041532e+02
# t4*   7.737319e+03   4.77415306 2.159716e+02
# t5*   2.429251e+03   9.16066383 1.729550e+02
# t6*  -1.394680e+03   0.13977566 6.360614e+01
# t7*   5.636370e+03   1.02235667 1.540395e+02
# t8*   4.129788e+03   0.29740863 1.335234e+02
# t9*  -6.195652e-02  -0.00046129 2.885974e-03
# t10*  4.195435e+03   8.70187956 1.732890e+02
# t11*  3.591243e+04 -14.14699733 1.197194e+03
# t12* -1.758650e+02   0.24802401 7.240796e+00
# t13*  1.129562e+03  -8.12605429 2.016165e+02
# t14*  1.996188e+03   0.62028812 8.776989e+01
# t15*  1.933217e+03  -0.20098181 1.463908e+02
# t16*  9.766555e+02   9.04838805 1.154651e+02
# t17*  5.943008e+03 178.27387160 2.080075e+03
# t18*  9.669544e+02   8.35292873 1.769742e+02
# t19* -5.867477e+02  -1.53184595 9.251313e+01
# t20* -2.859166e+00  -0.05324246 6.249385e-01
# t21*  5.760908e+02  12.82185425 1.601966e+02
# t22* -4.932997e+03  24.58655237 5.587411e+02
# t23*  2.405498e+02  -4.15826216 1.180814e+02
# t24* -1.791138e+03  33.42283576 1.170025e+03
# t25*  1.395829e+02  -0.58140095 1.080706e+02

summary(model6)$coeff

# Porównanie
comparison <- data.frame(Classic_Est = summary(model6)$coeff[, 1], Bootstrap_Est = boot.est$t0)
comparison
#                         Classic_Est Bootstrap_Est
# (Intercept)             2.591990e+04  2.591990e+04
# age                    -9.703157e+02 -9.703157e+02
# engineSize_1.0         -7.714856e+02 -7.714856e+02
# model_kodiaq            7.737319e+03  7.737319e+03
# engineSize_2.0          2.429251e+03  2.429251e+03
# transmission_manual    -1.394680e+03 -1.394680e+03
# model_karoq             5.636370e+03  5.636370e+03
# model_superb            4.129788e+03  4.129788e+03
# mileage                -6.195652e-02 -6.195652e-02
# model_kamiq             4.195435e+03  4.195435e+03
# fuelType_hybrid         3.591243e+04  3.591243e+04
# mpg                    -1.758650e+02 -1.758650e+02
# fuelType_diesel         1.129562e+03  1.129562e+03
# model_octavia           1.996188e+03  1.996188e+03
# model_scala             1.933217e+03  1.933217e+03
# model_yeti_out          9.766555e+02  9.766555e+02
# engineSize_1.9          5.943008e+03  5.943008e+03
# engineSize_1.6          9.669544e+02  9.669544e+02
# model_citigo           -5.867477e+02 -5.867477e+02
# tax                    -2.859166e+00 -2.859166e+00
# model_yeti              5.760908e+02  5.760908e+02
# engineSize_2.5         -4.932997e+03 -4.932997e+03
# engineSize_1.2          2.405498e+02  2.405498e+02
# engineSize_1.8         -1.791138e+03 -1.791138e+03
# transmission_automatic  1.395829e+02  1.395829e+02

# dwa powyżej dają podobne wyniki, czyli jest dobrze



#===== 6. Wybór podzbioru zmiennych ===== 

# funkcja regsubsets() - wybór najlepszego podzbioru określonej liczby predyktorów
library(leaps)
regfit.full <- regsubsets(price ~ ., train)

# summary() wyświetla najlepszy podzbiór zmiennych dla każdego rozmiaru
summary(regfit.full)

# które zmienne są w modelu z 9 zmiennymi?
names(coef(regfit.full, 9))
# [1] "(Intercept)"            "mileage"                "mpg"                   
# [4] "age"                    "model_superb"           "model_kodiaq"          
# [7] "model_karoq"            "transmission_automatic" "engineSize_2.0"        
# [10] "engineSize_1.2" 


# Zwiększamy dopuszczalną liczbę zmiennych niezależnych, u nas będzie 29
regfit.full <- regsubsets(price ~ ., data = train, nvmax = 29)

reg.summary <- summary(regfit.full)
reg.summary


# Analizujemy R^2 dla kolejnych 29 modeli:
reg.summary$rsq
# [1] 0.3754281 0.5987890 0.7074729 0.8204338 0.8412831 0.8574119 0.8688265
# [8] 0.8847402 0.8949412 0.9054095 0.9101286 0.9137762 0.9179310 0.9197207
# [15] 0.9203541 0.9209312 0.9215314 0.9219095 0.9222750 0.9224526 0.9225729
# [22] 0.9226293 0.9226786 0.9227260 0.9227528 0.9227692 0.9227730 0.9227740
# [29] 0.9227742
# Najlepszy dla modelu z 29 zmiennymi


# Narysowanie wykresów dla wszystkich miar ułatwi wybór najlepszego modelu
par(mfrow = c(2, 2))
plot(reg.summary$rss, xlab = "Number of Variables",
     ylab = "RSS", type = "l")
plot(reg.summary$adjr2, xlab = "Number of Variables",
     ylab = "Adjusted RSq", type = "l")

# Znajdujemy i zaznaczamy max dla Adjusted RSq:
which.max(reg.summary$adjr2)
points(25, reg.summary$adjr2[25], col = "red", cex = 2, pch = 20)

# Wykonujemy analogiczne rysunki dla C_p oraz BIC:
plot(reg.summary$cp, xlab = "Number of Variables", ylab = "Cp", type = "l")
which.min(reg.summary$cp)

points(24, reg.summary$cp[24], col = "red", cex = 2, pch = 20)

which.min(reg.summary$bic)

plot(reg.summary$bic, xlab = "Number of Variables",
     ylab = "BIC", type = "l")

points(20, reg.summary$bic[20], col = "red", cex = 2, pch = 20)


# Które zmienne zostały wybrane do kolejnych modeli, wegłud kryterium?
par(mfrow=c(1,1))
plot(regfit.full, scale = "r2")
plot(regfit.full, scale = "adjr2")
plot(regfit.full, scale = "Cp")
plot(regfit.full, scale = "bic")


# Podejście 1: model z najniższym BIC
# Najniższy BIC otrzymujemy dla modelu z 20 zmiennymi
# Jakie zmienne do modelu z najniższym BIC
plot(regfit.full , scale = "bic")
coef(regfit.full, 20)
names(coef(regfit.full,20))
# [1] "(Intercept)"            "mileage"                "tax"                   
# [4] "mpg"                    "age"                    "model_octavia"         
# [7] "model_citigo"           "model_yeti_out"         "model_superb"          
# [10] "model_kodiaq"           "model_karoq"            "model_yeti"            
# [13] "model_scala"            "transmission_manual"    "transmission_automatic"
# [16] "engineSize_2.0"         "engineSize_1.2"         "engineSize_1.0"        
# [19] "engineSize_1.4"         "engineSize_1.9"         "transmission_other"   


## Funkcja predict dla regsubsets()
predict.regsubsets <- function(object, newdata, id, ...){
  form <- as.formula(object$call[[2]])
  mat <- model.matrix(form, newdata)
  coefi <- coef(object, id = id)
  xvars <- names(coefi)
  mat[ , xvars] %*% coefi
}


# Predykcja plus błędy dla modelu z najniższym BIC
pred <- predict.regsubsets(regfit.full, test, 20)

mse_bic20 <- mean((pred-test$price)^2)
mse_bic20
# 4318514

mae_bic20 <- mean(abs(pred-test$price))
mae_bic20
# 1492.923

## Błedy dużo wyższe niż z wcześniejszych modeli liniowych


# Podejście drugie: optymalny model z użyciem 10-CV
k <- 10
n <- nrow(train)
set.seed(1)
folds <- sample(rep(1:k, length = n))

# Macierz wyników
cv.errors <- matrix(NA, k, 28, dimnames = list(NULL, paste(1:28)))

# 10-CV:
options(warn=-1)

for(j in 1:k){
  best.fit <- regsubsets(price ~ ., data = train[folds != j, ], nvmax = 29)
  for(i in 1:28){
    pred <- predict(best.fit, train[folds == j, ], id = i)
    cv.errors[j, i] <- mean((train$price[folds == j] - pred)^2)
  }
}

options(warn=0)


# Wyniki z 10-CV:
mean.cv.errors <- apply(cv.errors, 2, mean)
mean.cv.errors
# 1         2         3         4         5         6         7         8 
# 24811725  24776341  17560704  15123647  13119563  11458272  10343050   8497369 
# 9        10        11        12        13        14        15        16 
# 8066488   7050872   6158648   5507773   5431679   5304049   5039419   5044390 
# 17        18        19        20        21        22        23        24 
# 4901549   4813457   4811943   4793455   4932360   4823130   4744006   4704173 
# 25        26        27        28 
# 4508508   7532656   3442815 234173260 

plot(mean.cv.errors[-28], type = "b")

## Komentarz: najlepszy model z 27 zmiennymi, czyli podobnie jak wcześniej liniowe

# Testujemy model z 27 zmiennymi na zbiorze testowym
reg.best <- regsubsets(price ~ ., data = train, nvmax = 27)
coef(reg.best, 27)

pred <- predict.regsubsets(reg.best, test, 27)

mse_regcv <- mean((pred-test$price)^2)
mse_regcv
# 2783338

mae_regcv <- mean(abs(pred-test$price))
mae_regcv
# 1252.265



#===== 7. Regresja grzbietowa =====

# Przygotowanie danych
xtrain <- model.matrix(price ~ ., train)[, -1]
ytrain <- train$price

xtest <- model.matrix(price ~ ., test)[, -1]
ytest <- test$price

# Korzystamy z biblioteki glmnet
# Funkcja glmnet()
# parametr alpha = 0 oznacza regresje grzbietową

# Dopasowujemy model regresji grzbietowej
library(glmnet)
grid <- 10^seq(10, -2, length = 100)
ridge.mod <- glmnet(xtrain, ytrain, alpha = 0, lambda = grid)

# Znajdujemy najlepszy model, 10-CV
set.seed(1)
cv.out <- cv.glmnet(xtrain, ytrain, alpha = 0)
plot(cv.out)

# Z 10-CV otrzymujemy najlepszy parametr lambda
bestlam <- cv.out$lambda.min
bestlam
# 385.9079

# Błędy dla najlepszego parametru lambda na zbiorze testowym
ridge.pred <- predict(ridge.mod, s = bestlam, newx = xtest)
mse_ridge <- mean((ridge.pred - ytest)^2)
mse_ridge
# 2862187

mae_ridge <- mean(abs(ridge.pred-ytest))
mae_ridge
# 1254.379


# Jak wygląda model dla najlepszej lambdy?
predict(cv.out, best_lambda, type = "coefficients")
# 34 x 1 sparse Matrix of class "dgCMatrix"
# lambda.1se
# (Intercept)             2.530707e+04
# mileage                -6.169473e-02
# tax                     9.584324e-01
# mpg                    -1.244458e+02
# age                    -8.958658e+02
# model_octavia          -3.807703e+02
# model_citigo           -2.859689e+03
# model_yeti_out         -1.096843e+03
# model_superb            1.844211e+03
# model_kodiaq            5.572723e+03
# model_rapid            -2.162633e+03
# model_karoq             3.390981e+03
# model_fabia            -2.152074e+03
# model_yeti             -1.496790e+03
# model_scala             1.048264e+02
# model_roomster         -2.765485e+03
# model_kamiq             2.204882e+03
# transmission_manual    -9.320968e+02
# transmission_automatic  8.738377e+02
# transmission_semiauto   5.620612e+02
# transmission_other     -4.767765e+02
# fuelType_petrol        -5.359786e+02
# fuelType_diesel         1.817625e+02
# fuelType_hybrid         2.622731e+04
# fuelType_other         -3.743550e+02
# engineSize_2.0          2.111198e+03
# engineSize_1.2         -4.641906e+02
# engineSize_1.6          2.436885e+02
# engineSize_1.0         -1.530596e+03
# engineSize_1.5         -2.511436e+02
# engineSize_1.4         -3.403561e+02
# engineSize_1.9          4.796855e+03
# engineSize_1.8         -2.222562e+03
# engineSize_2.5         -5.318123e+03

## Efekt regresji grzbietowej:
## każda zmienna użyta, żadna nie została wyzerowana



#===== 8. Regresja LASSO =====

# Znowu funkcja glmnet()
# ale tym razem parametr alpha=1
lasso.mod <- glmnet(xtrain, ytrain, alpha = 1, lambda = grid)

# Sprawdźmy, jak w naszym zadaniu sprawdzi się regresja lasso
cv.out <- cv.glmnet(xtrain, ytrain, alpha = 1)
plot(cv.out)

# Najlepszy parametr lambda?
bestlam <- cv.out$lambda.min
bestlam
# 6.90254


# Liczymy błędy na zbiorze testowym:
lasso.pred <- predict(lasso.mod, s = bestlam, newx = xtest)

mse_lasso <- mean((lasso.pred - ytest)^2)
mse_lasso
# 2778904

mae_lasso <- mean(abs(lasso.pred-ytest))
mae_lasso
# 1250.133

mse_lm > mse_lasso

## Wyniki lekko lepsze niż za pomocą regresji grzbietowej
## Ale model liniowy z regresji krokowej forward jest nadal najlepszy


# Współczynniki optymalnego modelu
predict(cv.out, type = "coefficients", s = bestlam)[1:34,]
# 34 x 1 sparse Matrix of class "dgCMatrix"
# s1
# (Intercept)             2.880097e+04
# mileage                -6.141662e-02
# tax                    -2.396972e+00
# mpg                    -1.716260e+02
# age                    -9.735460e+02
# model_octavia           0.000000e+00           
# model_citigo           -2.483558e+03
# model_yeti_out         -9.088200e+02
# model_superb            2.131257e+03
# model_kodiaq            5.756073e+03
# model_rapid            -1.911044e+03
# model_karoq             3.673849e+03
# model_fabia            -1.896471e+03
# model_yeti             -1.286132e+03
# model_scala             0.000000e+00           
# model_roomster         -2.445699e+03
# model_kamiq             2.226952e+03
# transmission_manual    -1.400855e+03
# transmission_automatic  1.360550e+02
# transmission_semiauto   0.000000e+00           
# transmission_other     -6.740967e+02
# fuelType_petrol        -1.038842e+03
# fuelType_diesel         0.000000e+00          
# fuelType_hybrid         3.404720e+04
# fuelType_other         -5.342426e+02
# engineSize_2.0          2.348112e+03
# engineSize_1.2          1.290482e+00
# engineSize_1.6          8.469431e+02
# engineSize_1.0         -9.951239e+02
# engineSize_1.5         -1.489848e+02
# engineSize_1.4         -2.301612e+01
# engineSize_1.9          5.613077e+03
# engineSize_1.8         -1.639772e+03
# engineSize_2.5         -4.498354e+03

## 4 współczynniki wyzerowane, regresja LASSO zadziałała



#===== 9. Regresja głównych składowych PCR =====

# Korzystamy z biblioteki pls i funkcji prc()
library(pls)

# 10-krotna CV dla każdej ilości składowych głównych
set.seed(1)
pcr.fit <- pcr(price ~., data = train, validation = "CV")

# pcr() podaje pierwiastki z MSE
summary(pcr.fit)
# Data: 	X dimension: 4696 33 
# Y dimension: 4696 1
# Fit method: svdpc
# Number of components considered: 33
# 
# VALIDATION: RMSEP
# Cross-validated using 10 random segments.
# (Intercept)  1 comps  2 comps  3 comps  4 comps  5 comps  6 comps
# CV            6300     5487     5242     5002     4305     3219     3151
# adjCV         6300     5487     5242     5001     4304     3218     3150
# 7 comps  8 comps  9 comps  10 comps  11 comps  12 comps  13 comps
# CV        3075     2951     2938      2813      2739      2733      2659
# adjCV     3073     2948     2937      2811      2738      2731      2657
# 14 comps  15 comps  16 comps  17 comps  18 comps  19 comps  20 comps
# CV         2522      2517      2486      2484      2358      2305      2150
# adjCV      2520      2515      2485      2482      2357      2304      2149
# 21 comps  22 comps  23 comps  24 comps  25 comps  26 comps  27 comps
# CV         2091      2051      2017      2012      1904      1771      1769
# adjCV      2090      2050      2004      2012      1909      1771      1768
# 28 comps   29 comps   30 comps   31 comps   32 comps   33 comps
# CV         1769  1.239e+10  2.557e+10  3.727e+10  1.225e+11  2.421e+11
# adjCV      1768  1.175e+10  2.425e+10  3.535e+10  1.162e+11  2.297e+11
# 
# TRAINING: % variance explained
# 1 comps  2 comps  3 comps  4 comps  5 comps  6 comps  7 comps  8 comps
# X       100.00   100.00   100.00   100.00   100.00   100.00   100.00   100.00
# price    24.22    30.81    37.47    53.81    74.36    75.44    76.65    78.51
# 9 comps  10 comps  11 comps  12 comps  13 comps  14 comps  15 comps
# X        100.0    100.00    100.00    100.00    100.00    100.00    100.00
# price     78.7     80.51     81.47     81.57     82.56     84.26     84.35
# 16 comps  17 comps  18 comps  19 comps  20 comps  21 comps  22 comps
# X        100.00    100.00    100.00    100.00    100.00    100.00     100.0
# price     84.74     84.79     86.31     86.93     88.61     89.19      89.6
# 23 comps  24 comps  25 comps  26 comps  27 comps  28 comps  29 comps
# X        100.00    100.00    100.00    100.00    100.00    100.00    100.00
# price     89.99     90.14     91.02     92.21     92.25     92.25     92.28
# 30 comps  31 comps  32 comps  33 comps
# X        100.00    100.00    100.00    100.00
# price     92.28     92.28     92.28     92.28

validationplot(pcr.fit, val.type = "MSEP")

## coś się psuje od 29 składowej (zmienne zależne?)


# Jeszcze raz, ale ograniczmy max liczbę składowych do 28
set.seed(1)
pcr.fit <- pcr(price ~., data = train, validation = "CV", ncomp=28)

summary(pcr.fit)
# Data: 	X dimension: 4696 33 
# Y dimension: 4696 1
# Fit method: svdpc
# Number of components considered: 28
# 
# VALIDATION: RMSEP
# Cross-validated using 10 random segments.
# (Intercept)  1 comps  2 comps  3 comps  4 comps  5 comps  6 comps
# CV            6300     5487     5242     5002     4305     3219     3151
# adjCV         6300     5487     5242     5001     4304     3218     3150
# 7 comps  8 comps  9 comps  10 comps  11 comps  12 comps  13 comps
# CV        3075     2951     2938      2813      2739      2733      2659
# adjCV     3073     2948     2937      2811      2738      2731      2657
# 14 comps  15 comps  16 comps  17 comps  18 comps  19 comps  20 comps
# CV         2522      2517      2486      2484      2358      2305      2150
# adjCV      2520      2515      2485      2482      2357      2304      2149
# 21 comps  22 comps  23 comps  24 comps  25 comps  26 comps  27 comps
# CV         2091      2051      2017      2012      1904      1771      1769
# adjCV      2090      2050      2004      2012      1909      1771      1768
# 28 comps
# CV         1769
# adjCV      1768
# 
# TRAINING: % variance explained
# 1 comps  2 comps  3 comps  4 comps  5 comps  6 comps  7 comps  8 comps
# X       100.00   100.00   100.00   100.00   100.00   100.00   100.00   100.00
# price    24.22    30.81    37.47    53.81    74.36    75.44    76.65    78.51
# 9 comps  10 comps  11 comps  12 comps  13 comps  14 comps  15 comps
# X        100.0    100.00    100.00    100.00    100.00    100.00    100.00
# price     78.7     80.51     81.47     81.57     82.56     84.26     84.35
# 16 comps  17 comps  18 comps  19 comps  20 comps  21 comps  22 comps
# X        100.00    100.00    100.00    100.00    100.00    100.00     100.0
# price     84.74     84.79     86.31     86.93     88.61     89.19      89.6
# 23 comps  24 comps  25 comps  26 comps  27 comps  28 comps
# X        100.00    100.00    100.00    100.00    100.00    100.00
# price     89.99     90.14     91.02     92.21     92.25     92.25

validationplot(pcr.fit, val.type = "MSEP")

## minimum dla składowej nr 28


# Błędy na zbiorze testowym
pcr.pred <- predict(pcr.fit, test[-1], ncomp = 28)

mse_pcr <- mean((pcr.pred - test$price)^2)
mse_pcr
# 2776254

mae_pcr <- mean(abs(pcr.pred - test$price))
mae_pcr
# 1251.048



#===== 10. Regresja cząstkowych najmniejszych kwadratów PLS =====

set.seed(1)
pls.fit <- plsr(price ~ ., data = train, validation = "CV")
summary(pls.fit)
# Data: 	X dimension: 4696 33 
# Y dimension: 4696 1
# Fit method: kernelpls
# Number of components considered: 33
# 
# VALIDATION: RMSEP
# Cross-validated using 10 random segments.
# (Intercept)  1 comps  2 comps  3 comps  4 comps  5 comps  6 comps
# CV            6300     5487     5214     4978     3268     2814     2435
# adjCV         6300     5487     5213     4976     3267     2812     2434
# 7 comps  8 comps  9 comps  10 comps  11 comps  12 comps  13 comps
# CV        2264     2133     2054      2013      1998      1972      1936
# adjCV     2263     2132     2053      2012      1997      1971      1935
# 14 comps  15 comps  16 comps  17 comps  18 comps  19 comps  20 comps
# CV         1893      1852      1816      1803      1798      1790      1780
# adjCV      1892      1851      1815      1802      1797      1789      1779
# 21 comps  22 comps  23 comps  24 comps  25 comps  26 comps  27 comps
# CV         1770      1769      1769      1769      1769      1769      1770
# adjCV      1769      1768      1769      1768      1768      1768      1768
# 28 comps  29 comps  30 comps  31 comps  32 comps  33 comps
# CV         1770      1770      1770      1770      1770      1770
# adjCV      1768      1768      1768      1768      1768      1768
# 
# TRAINING: % variance explained
# 1 comps  2 comps  3 comps  4 comps  5 comps  6 comps  7 comps  8 comps
# X       100.00   100.00   100.00   100.00   100.00   100.00   100.00   100.00
# price    24.22    31.57    38.11    73.53    80.48    85.32    87.34    88.76
# 9 comps  10 comps  11 comps  12 comps  13 comps  14 comps  15 comps
# X       100.00    100.00    100.00    100.00    100.00    100.00    100.00
# price    89.58     89.99     90.13     90.38     90.73     91.13     91.53
# 16 comps  17 comps  18 comps  19 comps  20 comps  21 comps  22 comps
# X        100.00    100.00    100.00     100.0    100.00    100.00    100.00
# price     91.86     91.97     92.02      92.1     92.17     92.25     92.26
# 23 comps  24 comps  25 comps  26 comps  27 comps  28 comps  29 comps
# X        100.00    100.00    100.00    100.00    100.00    100.00    100.00
# price     92.26     92.27     92.27     92.28     92.28     92.28     92.28
# 30 comps  31 comps  32 comps  33 comps
# X        102.39    104.78    107.17    109.56
# price     92.28     92.28     92.28     92.28

validationplot(pls.fit, val.type = "MSEP")

## Minimum wydaje się dla ncomp=22

# Błedy na zbiorze testowym
pls.pred <- predict(pls.fit, test, ncomp = 22)
mse_pls <- mean((pls.pred - test$price)^2)
mse_pls
# 2769480

mae_pls <- mean(abs(pls.pred-test$price))
mae_pls
# 1249.765



#===== 11. Drzewa regresyjne =====

library(tree)
# Proste drzewo regresyjne z domyślnymi parametrami
tree<- tree(price ~ ., train)
summary(tree)
# Regression tree:
#   tree(formula = price ~ ., data = train)
# Variables actually used in tree construction:
#   [1] "mpg"                 "age"                 "engineSize_2.0"     
# [4] "model_octavia"       "engineSize_1.0"      "transmission_manual"
# Number of terminal nodes:  10 
# Residual mean deviance:  8738000 = 4.095e+10 / 4686 
# Distribution of residuals:
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -14790   -1997    -267       0    1740   15370 

# użyto 6 zmiennych, mamy 10 liści

plot(tree)
text(tree, pretty = 0)

# Sprawdźmy, czy przycinanie drzewa poprawi jego jakość predykcyjną
cv.tree <- cv.tree(tree)
print(cv.tree)
plot(cv.tree$size, cv.tree$dev, type = "b")
## najmniejszy blad jest dla liczby lsici  10 zatem 
## W tym przypadku całe rozważane drzewo jest wybierane przez walidację krzyżową

# Predykcje i błedy na zbiorze testowym
yhat <- predict(tree, newdata = test)
mse_tree <- mean((yhat - test$price)^2)
mse_tree
# 8903689

mae_tree <- mean(abs(test$price-yhat))
mae_tree
# 2317.985



#===== 12. Bagging =====
library(randomForest)
# Domyślnie funkcja robi 500 drzew
# Bagging czyli m = p = 33
# mtry = liczba zmiennych objaśniających

set.seed(1)
bagging_model <- randomForest(price ~ ., data = train, mtry = ncol(train) - 1, importance=TRUE)

bagging_model
# Call:
#   randomForest(formula = price ~ ., data = train, mtry = ncol(train) -      1, importance = TRUE) 
# Type of random forest: regression
# Number of trees: 500
# No. of variables tried at each split: 33
# 
# Mean of squared residuals: 1914275
# % Var explained: 95.17


# Sprawdzamy model na danych testowych:
yhat.bag <- predict(bagging_model, newdata = test)

plot(yhat.bag, test$price)
# dodaje linię y = 1 * x + 0 
abline(0, 1)   

mse_bagging <- mean((yhat.bag - test$price)^2)
mse_bagging
# 1614615

mae_bagging <- mean(abs(yhat.bag - test$price))
mae_bagging
# 904.3723

## dotychczas najlepsze wyniki

# Próba dopracowania modelu
# Różna liczba drzew, parametr ntree
# Testujemy to pętlą dla różnych wartości ntree

# PĘTLA DO TESTOWANIA

# ntree_values <- seq(100, 1000, by=100)
# 
# for (ntree_value in ntree_values) {
#   set.seed(1)
#   bag_model <- randomForest(price ~ ., data = train, mtry = ncol(train) - 1,
#                             importance=TRUE, ntree=ntree_value)
# 
#   yhat <- predict(bag_model, newdata = test)
#   mse <- mean((test$price - yhat)^2)
# 
#   cat("MSE dla ntree =", ntree_value, ":", mse, "\n")
# }

# MSE dla ntree = 100 : 1630163 
# MSE dla ntree = 200 : 1620146 
# MSE dla ntree = 300 : 1618591 
# MSE dla ntree = 400 : 1615862 
# MSE dla ntree = 500 : 1614615 
# MSE dla ntree = 600 : 1613421 
# MSE dla ntree = 700 : 1612719 
# MSE dla ntree = 800 : 1612396 
# MSE dla ntree = 900 : 1612644 
# MSE dla ntree = 1000 : 1611732 

## Różnice niewielkie, zatem zostawiamy domyślne ntree = 500 



#===== 13. Las losowy =====
# Podobnie jak w baggingu
# parametry m = p/3 dla drzew regresyjnych
set.seed(1)
rndfor <- randomForest(price ~ ., data = train, mtry = (ncol(train) - 1)/3, importance=TRUE)
rndfor
# Call:
#   randomForest(formula = price ~ ., data = train, mtry = (ncol(train) -      1)/3, importance = TRUE) 
# Type of random forest: regression
# Number of trees: 500
# No. of variables tried at each split: 11
# 
# Mean of squared residuals: 1798637
# % Var explained: 95.47


# Sprawdzamy model na danych testowych:
yhat.rndfor <- predict(rndfor, newdata = test)

plot(yhat.rndfor, test$price)
# dodaje linię y = 1 * x + 0 
abline(0, 1)   

mse_rndfor <- mean((yhat.rndfor - test$price)^2)
mse_rndfor
# 1537323

mae_rndfor <- mean(abs(yhat.rndfor - test$price))
mae_rndfor
# 897.1398

# Najlepszy dotychczas wynik



#===== 14. Boosting dla drzew regresyjnych =====
library(gbm)
set.seed(1)
# Funkcja gbm z ustalonymi parametrami
boost.train <- gbm(price ~ ., data = train,
                   distribution = "gaussian", n.trees = 5000,
                   interaction.depth = 4)

summary(boost.train)


# Zależność przegowa od wybranej zmiennej
plot(boost.train, i = "mpg")
plot(boost.train, i = "tax")

# Sprawdzamy nasz model na danych testowych:
yhat.boost <- predict(boost.train, newdata = test, n.trees = 5000)

mse_boost <- mean((yhat.boost - test$price)^2)
mse_boost
# 1710982

mae_boost <- mean(abs(yhat.boost - test$price))
mae_boost
# 934.5354


# Dopracowanie modelu
# Najlepsze hiperparametry?

# PĘTLE DO TESTOWANIA

## Liczba drzew
# number <- seq(100, 2000, by=100)
# 
# for(i in number){
#   set.seed(1)
#   boost.train <- gbm(price ~ ., data = train,
#                      distribution = "gaussian", n.trees = i,
#                      interaction.depth = 4, shrinkage = 0.1)
# 
#   yhat.boost <- predict(boost.train, newdata = test, n.trees = i)
#   error <- mean((yhat.boost - test$price)^2)
#   cat("Dla liczby drzew: ", i, "MSE: ", error, "\n")
# 
# }

# Dla liczby drzew:  100 MSE:  2188834
# Dla liczby drzew:  200 MSE:  1882831
# Dla liczby drzew:  300 MSE:  1761325
# Dla liczby drzew:  400 MSE:  1692979
# Dla liczby drzew:  500 MSE:  1668676
# Dla liczby drzew:  600 MSE:  1642775
# Dla liczby drzew:  700 MSE:  1628491
# Dla liczby drzew:  800 MSE:  1629259
# Dla liczby drzew:  900 MSE:  1631338
# Dla liczby drzew:  1000 MSE:  1621179
# Dla liczby drzew:  1100 MSE:  1605883
# Dla liczby drzew:  1200 MSE:  1608664
# Dla liczby drzew:  1300 MSE:  1617140
# Dla liczby drzew:  1400 MSE:  1603372
# Dla liczby drzew:  1500 MSE:  1602686
# Dla liczby drzew:  1600 MSE:  1605738
# Dla liczby drzew:  1700 MSE:  1596696
# Dla liczby drzew:  1800 MSE:  1603241
# Dla liczby drzew:  1900 MSE:  1622490
# Dla liczby drzew:  2000 MSE:  1631923
 
# liczba drzew 1700 najlepsza
 

## Depth
# number <- seq(1, 10, by=1)
# 
# for(i in number){
#   set.seed(1)
#   boost.train <- gbm(price ~ ., data = train,
#                      distribution = "gaussian", n.trees = 1700,
#                      interaction.depth = i, shrinkage = 0.1)
# 
#   yhat.boost <- predict(boost.train, newdata = test, n.trees = 1700)
#   error <- mean((yhat.boost - test$price)^2)
#   cat("Dla deep: ", i, "MSE: ", error, "\n")
# 
# }

# Dla deep:  1 MSE:  2204196
# Dla deep:  2 MSE:  1730592
# Dla deep:  3 MSE:  1586667
# Dla deep:  4 MSE:  1596696
# Dla deep:  5 MSE:  1623144
# Dla deep:  6 MSE:  1637300
# Dla deep:  7 MSE:  1660239
# Dla deep:  8 MSE:  1647574
# Dla deep:  9 MSE:  1696378
# Dla deep:  10 MSE:  1722349

# Najlepszy deep: 3


## Learning rate
# number <- seq(0.01,0.2,by=0.02)
# 
# for(i in number){
#   set.seed(1)
#   boost.train <- gbm(price ~ ., data = train,
#                      distribution = "gaussian", n.trees = 1700,
#                      interaction.depth = 3, shrinkage = i)
# 
#   yhat.boost <- predict(boost.train, newdata = test, n.trees = 1700)
#   error <- mean((yhat.boost - test$price)^2)
#   cat("Dla shrinkage: ", i, "MSE: ", error, "\n")
# 
# }

# Dla shrinkage:  0.01 MSE:  2036482 
# Dla shrinkage:  0.03 MSE:  1728441 
# Dla shrinkage:  0.05 MSE:  1672139 
# Dla shrinkage:  0.07 MSE:  1639962 
# Dla shrinkage:  0.09 MSE:  1596521 
# Dla shrinkage:  0.11 MSE:  1649032 
# Dla shrinkage:  0.13 MSE:  1668107 
# Dla shrinkage:  0.15 MSE:  1653424 
# Dla shrinkage:  0.17 MSE:  1702396 
# Dla shrinkage:  0.19 MSE:  1639951 
 
# Oryginalny 0.1 jest dobry


# Budujemy model ponownie, ale dla najlepszych hiperparametrów
set.seed(1)
boost.train <- gbm(price ~ ., data = train,
                   distribution = "gaussian", n.trees = 1700,
                   interaction.depth = 3, shrinkage=0.1)

summary(boost.train)

# Sprawdzamy nasz model na danych testowych:
yhat.boost <- predict(boost.train, newdata = test, n.trees = 1700)

mse_boost <- mean((yhat.boost - test$price)^2)
mse_boost
# 1586667

mae_boost <- mean(abs(yhat.boost - test$price))
mae_boost
# 924.8131



#===== 15. Bayesowskie addytywne drzewa regresyjne - BART =====

# Przygotowanie danych do modelu BART
xtrain <- train[-1]
ytrain <- train$price
xtest <- test[-1]


# Uruchamiamy z domyślnymi parametrami:
library(BART)

set.seed(1)
bartfit <- gbart(xtrain, ytrain, x.test = xtest)


# Sprawdzamy poprawność na danych testowych:
yhat.bart <- bartfit$yhat.test.mean

mse_bart <- mean((test$price - yhat.bart)^2)
mse_bart
# 1656678

mae_bart <- mean(abs(test$price - yhat.bart))
mae_bart
# 933.4376


# Ciekawostka
# Dostajemy informację jak  często zmienne były używane w rozważanych drzewach
ord <- order(bartfit$varcount.mean, decreasing = T)
bartfit$varcount.mean[ord]
# > bartfit$varcount.mean[ord]
# mpg                    age                mileage 
# 35.903                 18.556                 14.158 
# model_superb            model_karoq    transmission_manual 
# 13.992                 13.410                 13.121 
# model_kodiaq         engineSize_1.5         engineSize_2.0 
# 12.474                 12.387                 12.152 
# fuelType_hybrid           model_citigo            model_rapid 
# 9.514                  9.494                  9.316 
# model_octavia         engineSize_1.6        fuelType_petrol 
# 9.238                  8.806                  8.319 
# model_fabia         engineSize_1.4                    tax 
# 8.223                  8.196                  8.148 
# fuelType_diesel transmission_automatic             model_yeti 
# 7.477                  7.203                  6.781 
# engineSize_1.2            model_scala         fuelType_other 
# 6.632                  6.597                  6.314 
# engineSize_1.0         model_yeti_out  transmission_semiauto 
# 6.126                  5.689                  5.664 
# model_kamiq         engineSize_1.9         model_roomster 
# 5.618                  5.478                  5.462 
# transmission_other         engineSize_1.8         engineSize_2.5 
# 0.000                  0.000                  0.000 



#===== 16. XGBOOST =====
library(xgboost)
        
# Pakiet xgboost używa matrix data
train_x <- data.matrix(train[, -1])  
train_y <- train[, 1]

test_x <- data.matrix(test[, -1])
test_y <- test[, 1]

# Kolejne przekształcenia
# xgb.DMatrix() z pakietu xgboost służy do przekształcania danych
xgb_train <- xgb.DMatrix(data = train_x, label = train_y)
xgb_test <- xgb.DMatrix(data = test_x, label = test_y)


# Model z pewnymi ustalonymi parametrami
watchlist <- list(train=xgb_train, test=xgb_test)
model <- xgb.train(data = xgb_train, max.depth = 4, watchlist = watchlist,
                   nrounds = 200)


# Szukamy najlepszego  parametru max.depth
# oraz w krórej iteracji model jest najlepiej nauczony
# for(i in c(2:15)){
#   watchlist <- list(train=xgb_train, test=xgb_test)
#   model <- xgb.train(data = xgb_train, max.depth = i, watchlist = watchlist,
#                      nrounds = 1000, verbose = 0)
# 
#   a <- min(model$evaluation_log$test_rmse)
#   b <- which(model$evaluation_log$test_rmse == a)
#   c <- model$evaluation_log$iter[b]
#   d <- model$evaluation_log$test_rmse[c]
# 
#   cat("Depth", i, ": no", c, ", rmse:", d, "\n")
# 
# }

# Depth 2 : no 598 , rmse: 1314.449
# Depth 3 : no 234 , rmse: 1278.959
# Depth 4 : no 175 , rmse: 1291.288
# Depth 5 : no 100 , rmse: 1298.286
# Depth 6 : no 41 , rmse: 1293.313
# Depth 7 : no 25 , rmse: 1274.588
# Depth 8 : no 17 , rmse: 1298.154
# Depth 9 : no 23 , rmse: 1305.482
# Depth 10 : no 15 , rmse: 1291.91
# Depth 11 : no 19 , rmse: 1314.768
# Depth 12 : no 14 , rmse: 1318.726
# Depth 13 : no 14 , rmse: 1318.814
# Depth 14 : no 13 , rmse: 1338.432
# Depth 15 : no 13 , rmse: 1343.031

# The best values
# Depth 7 : no 25 , rmse: 1274.588


# Budujemy model xgboost dla otrzymanych paramtetrów
final <- xgboost(data = xgb_train, max.depth = 7, nrounds = 25, verbose = 0)
final
# ##### xgb.Booster
# raw: 150.5 Kb 
# call:
#   xgb.train(params = params, data = dtrain, nrounds = nrounds, 
#             watchlist = watchlist, verbose = verbose, print_every_n = print_every_n, 
#             early_stopping_rounds = early_stopping_rounds, maximize = maximize, 
#             save_period = save_period, save_name = save_name, xgb_model = xgb_model, 
#             callbacks = callbacks, max.depth = 7)
# params (as set within xgb.train):
#   max_depth = "7", validate_parameters = "1"
# xgb.attributes:
#   niter
# callbacks:
#   cb.evaluation.log()
# # of features: 33 
# niter: 25
# nfeatures : 33 
# evaluation_log:
#   iter train_rmse
# <num>      <num>
#   1 11045.1554
# 2  7868.6241
# ---                 
#   24   956.4166
# 25   949.7967


# Robimy predykcję na zbiorze testowym:
pred_y <- predict(final, xgb_test)

# Liczymy błąd predykcji na zbiorze testowym:
mse_xgboost <- mean((test_y - pred_y)^2)
mse_xgboost
# 1624576

mae_xgboost <- mean(abs(test_y - pred_y))
mae_xgboost
# 926.6741



#=======Podsumowanie=====

## Pełny model liniowy dla porównania jako model wyjściowy
prediction <- predict.glm(model4, test)

mse_full <- mean((prediction-test$price)**2)
mse_full
# 2780182

mae_full <- mean(abs(prediction-test$price))
mae_full
# 1251.551


# Budujemy data frame dla otrzymanych błędów
models_names <- c('glm full model','glm forward', 'best subset: bic', 'best subset: cv',
                  'ridge', 'lasso', 'prc', 'pls', 'tree', 'bagging',
                  'rand forest', 'boosting', 'bart', 'xg boost')

models_mse <- c(mse_full, mse_lm, mse_bic20, mse_regcv, mse_ridge, mse_lasso, mse_pcr,
                mse_pls, mse_tree, mse_bagging, mse_rndfor, mse_boost, mse_bart,
                mse_xgboost)

models_mae <- c(mae_full, mae_lm, mae_bic20, mae_regcv, mae_ridge, mae_lasso, mae_pcr,
                mae_pls, mae_tree, mae_bagging, mae_rndfor, mae_boost, mae_bart,
                mae_xgboost)

models_df <- data.frame(
  Model = models_names,
  MSE = models_mse,
  MAE = models_mae
)


# Sortowanie po MSE
models_df_sorted <- models_df[order(models_df$MSE), ]

# Wyświetlenie posortowanych wyników
print(models_df_sorted)


## WYNIKI
# > print(models_df_sorted)
# Model     MSE       MAE
# 11      rand forest 1537323  897.1398
# 12         boosting 1586667  924.8131
# 10          bagging 1614615  904.3723
# 14         xg boost 1624576  926.6741
# 13             bart 1656678  933.4376
# 8               pls 2769480 1249.7649
# 7               prc 2776254 1251.0484
# 2       glm forward 2778571 1250.0261
# 6             lasso 2778904 1250.1328
# 1    glm full model 2780182 1251.5509
# 4   best subset: cv 2783338 1252.2649
# 5             ridge 2862187 1254.3787
# 3  best subset: bic 4318514 1492.9229
# 9              tree 8903689 2317.9848


# Komentarz:
# Wyjściowy model pełny nie daje dobrych rezultatów
# Otrzymaliśmy 5 modeli, które widocznie najlepiej sobie radzą
# Wybór najlepszego z tej piątki można oprzeć na złożoności obliczeniowej, gdyż rezultaty między nimi są niewielkie

