Graph4_4 <- function(){

    Data_Age_Hist <- read_csv("data/Titanic_csv.csv") %>%
        group_by(age, pclass) %>% mutate(age = round(age)) %>%
        filter(!is.na(age)) %>%
        summarise(Freq = n())

    Data_Age_Hist$pclass <- as.factor(Data_Age_Hist$pclass)

    Plot4_3 <- Data_Age_Hist %>% ggplot(aes(x = age, y = Freq)) +
        geom_bar(aes(fill = pclass), position = "identity", stat = "identity", alpha = 0.6) +
        geom_smooth(aes(colour = pclass), linewidth = 0.75, alpha = 0.2, se = F) +
        labs(title = "Distribution of Age Across Classes Aboard The Titanic",
             x = "Age",
             y = "Number of Passengers") +
        scale_fill_manual(name = "",
                          labels = c("First Class", "Second Class", "Third Class"),
                          values = c("darkorchid1", "yellow1", "darkslategray2")) +
        scale_colour_manual(name = "",
                          labels = c("First Class", "Second Class", "Third Class"),
                          values = c("purple3", "darkgoldenrod2", "deepskyblue")) +
        theme_minimal()

    Plot4_3



}