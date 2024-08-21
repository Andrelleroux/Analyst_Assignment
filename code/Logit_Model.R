
No_Boat_Model <- function(){

Data_Logit <- read_csv("data/Titanic_csv.csv") %>%
    filter(!is.na(age)) %>%
    mutate(age = round(age)) %>%
    #mutate(got_boat = ifelse(!is.na(boat), 1, 0)) %>%
    select(pclass, survived, sex, age, sibsp, parch, fare,
           #got_boat
           ) %>% na.omit()

Data_Logit$survived <- as.factor(Data_Logit$survived)

set.seed(7104)

split <- initial_split(Data_Logit, prop = 0.80)
Train_data <- training(split)
Test_data <- testing(split)

Logit <- train(

    survived ~ . ,

    data = Train_data,

    method = "glm",
    family = "binomial",
    trControl = trainControl(method = "cv", number = 10)
)

Logit

PredLogit <- predict(Logit, newdata = Test_data)
Conf_Logit <- confusionMatrix(PredLogit, Test_data$survived)

print(Conf_Logit)

Rose_data <- data.frame(
    pclass = 1,
    sex = "female",
    age = 17,
    sibsp = 0,
    parch = 1,
    fare = 500
)

Pred_Rose <- predict(Logit, newdata = Rose_data, type = "prob")
Pred_Rose

Jack_data <- data.frame(
    pclass = 3,
    sex = "male",
    age = 20,
    sibsp = 0,
    parch = 0,
    fare = 0
)

Pred_Jack <- predict(Logit, newdata = Jack_data, type = "prob")
Pred_Jack

final_Logit <- Logit$finalModel

Coeff <- final_Logit$coefficients

importance <- varImp(Logit, scale = F)

VIP_B_Plot <- plot(importance, main = "Variable Importance - Logistic Regression")
VIP_B_Plot
}
