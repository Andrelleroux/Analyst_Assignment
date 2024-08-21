Graph4_5 <- function(){

    Data_Age_Hist <- read_csv("data/Titanic_csv.csv") %>%
        group_by(age, pclass) %>% mutate(age = round(age)) %>%
        filter(!is.na(age), sex == "male", pclass == 3) %>%
        summarise(Freq = n())

    Plot4_5 <- Data_Age_Hist %>% ggplot(aes(x = age, y = Freq)) +
        geom_bar(fill = "darkolivegreen", position = "identity", stat = "identity", alpha = 0.8) +
        geom_smooth(colour = "darkorange2",linewidth = 1, se = F) +
        labs(title = "Distribution of the Age of Third Class Men Aboard The Titanic",
             x = "Age",
             y = "Number of Passengers") +
        theme_minimal()

    Plot4_5

}