Graph4_3 <- function(){

    Data_Age_Hist <- read_csv("data/Titanic_csv.csv") %>%
        group_by(age, pclass) %>% mutate(age = round(age)) %>%
        filter(!is.na(age)) %>%
        summarise(Freq = n())

    Data_Age_Hist$pclass <- as.factor(Data_Age_Hist$pclass)

    Plot4_2 <- Data_Age_Hist %>% ggplot(aes(x = age, y = Freq, fill = pclass)) +
        geom_bar(position = "identity", stat = "identity", alpha = 0.6)+
        facet_wrap(~pclass, scales = "fixed") +
        labs(title = "Histogram of Age Across Classes Aboard The Titanic",
             x = "Age",
             y = "Number of Passengers") +
        scale_fill_manual(name = "",
                          labels = c("First Class", "Second Class", "Third Class"),
                          values = c("darkorchid1", "darkgoldenrod1", "darkslategray3")) +
        theme_minimal()

    Plot4_2



}