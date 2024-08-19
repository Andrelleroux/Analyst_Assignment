Get_Gender_Class_Split <- function(){

    Data_init2 <- read_csv("data/Titanic_csv.csv") %>%
        group_by(pclass, sex) %>%
        summarise(Freq = n())

    Data_init2$sex <- as.factor(Data_init2$sex)

    PSex <- Data_init2 %>% filter(!is.na(pclass)) %>%
        ggplot(aes(x = pclass, y = Freq, fill = sex)) +
        geom_bar(position = "stack", stat = "identity") +
        labs(title = "Gender Split in Different Classes on the Titanic",
             x = "Class",
             y = "Number of Passengers") +
        scale_fill_manual(name = "",
                          labels = c("Female", "Male"),
                          values = c("goldenrod2", "darkslategrey")) + theme_minimal()

    PSex

}