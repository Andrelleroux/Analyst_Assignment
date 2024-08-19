Graph4_1 <- function(){

library(tidyverse)

Data_init <- read_csv("data/Titanic_csv.csv") %>%
    group_by(pclass, survived) %>%
    summarise(Freq = n())

Data_init$survived <- as.factor(Data_init$survived)


PClass <- Data_init %>% filter(!is.na(pclass)) %>%
    ggplot(aes(x = pclass, y = Freq, fill = survived)) +
    geom_bar(position = "fill", stat = "identity") +
    labs(title = "Survival by Class on the Titanic",
         x = "Class",
         y = "Percentage of Passengers") +
    scale_fill_manual(name = "",
                        labels = c("Passed Away", "Survived"),
                        values = c("darkorchid3", "deepskyblue2")) + theme_minimal()

PClass

}