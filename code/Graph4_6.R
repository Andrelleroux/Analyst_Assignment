Graph4_6 <- function(){

    Data_First <- read_csv("data/Titanic_csv.csv") %>%
        filter(!is.na(age)) %>%
        mutate(desig_group = ifelse(age <= 15, "Children",
                              ifelse(sex == "female" & age > 15, "Adult Women",
                                     ifelse(sex == "male" & age > 15, "Adult Men", 0)))) %>%
        mutate(got_boat = ifelse(!is.na(boat), 1, 0)) %>%
        group_by(desig_group, got_boat) %>%
        summarise(Freq = n())

    Data_First$got_boat = as.factor(Data_First$got_boat)

    Plot4_6 <- Data_First %>%
        ggplot(aes(x = desig_group, y = Freq, fill = got_boat)) +
        geom_bar(position = "fill", stat = "identity") +
        labs(title = "Percentage of Passengers in Lifeboats by Population Demographics",
             x = "Demographics",
             y = "Percentage of Passengers") +
        scale_fill_manual(name = "",
                          labels = c("Didn't Get into Boats", "Got into Boats"),
                          values = c("firebrick3", "chartreuse4")) + theme_minimal()

    Plot4_6

}