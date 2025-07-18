library(hrbrthemes)
library(scales)
library(ggplot2)

#cleaning county data
FL_pop_counties[, -1] <- lapply(FL_pop_counties[, -1], function(x) {
  as.numeric(gsub(",", "", x)) })
FL_pop_counties <- FL_pop_counties %>%
  mutate('Asian/NHPI' = ASIAN + HAW.PACISL, 'AIAN/MR/Else' = INDIAN.ALAS + MULTI)

FL_pop_counties$ASIAN <- NULL
FL_pop_counties$INDIAN.ALAS <- NULL
FL_pop_counties$HAW.PACISL <- NULL
FL_pop_counties$MULTI <- NULL

fl_county_demographics <- FL_pop_counties %>% 
  rename(
    White = WHITE,
    Black = BLACK.NH.,
    Hispanic = HISPANIC.LATINO)

###############################################################################

#orange county demographic visualization
fl_oc_demographics_2016 <- fl_county_demographics %>%
  filter(COUNTIES == "Orange")

oc_county_data <- data.frame(
  Race = c("White", "Black", "Hispanic", "Asian/NHPI", "AIAN/MR/Else"),
  Population = c(547269, 269770, 410732, 
                 72180, 27464))

oc_county_barplot <-ggplot(data=oc_county_data, aes(x=Race, y=Population)) +
  geom_bar(stat="identity") + theme_ipsum() + scale_fill_ipsum() + ggtitle("Orange County Race Demographics") +
  theme(plot.title = element_text(hjust = 0.5))

final_oc_barplot <- oc_county_barplot + scale_y_continuous(name = "Population") +
  scale_x_discrete(name = "Race") +
  geom_text(aes(label = Population), vjust = -0.3) +
  geom_col(aes(fill = Race)) +
  theme(
    axis.text.x = element_text(angle = 30),
    axis.title.x = element_text(hjust = 0.5, size = 20),
    axis.title.y = element_text(hjust = 0.5, size = 20, vjust=5),
    plot.title = element_text(hjust = 0.5)
  )
final_oc_barplot

###############################################################################

#palm beach county demographic visualization

fl_pb_demographics_2016 <- fl_county_demographics %>%
  filter(COUNTIES == "Palm Beach")

pb_county_data <- data.frame(
  Race = c("White", "Black", "Hispanic", "Asian/NHPI", "AIAN/MR/Else"),
  Population = c(811671,265368,312952,40162,21316))

pb_county_barplot <-ggplot(data=pb_county_data, aes(x=Race, y=Population)) +
  geom_bar(stat="identity") + theme_ipsum() + scale_fill_ipsum() + ggtitle("Palm Beach County Race Demographics") +
  theme(plot.title = element_text(hjust = 0.5))

final_pb_barplot <- pb_county_barplot + scale_y_continuous(name = "Population") +
  scale_x_discrete(name = "Race") +
  geom_text(aes(label = Population), vjust = -0.3) +
  geom_col(aes(fill = Race)) +
  theme(
    axis.text.x = element_text(angle = 30),
    axis.title.x = element_text(hjust = 0.5, size = 20),
    axis.title.y = element_text(hjust = 0.5, size = 20,  vjust=5),
    plot.title = element_text(hjust = 0.5)
  )
final_pb_barplot

###############################################################################

#miami-dade county demographic visualization

fl_md_demographics_2016 <- fl_county_demographics %>%
  filter(COUNTIES == "Miami-Dade")

md_county_data <- data.frame(
  Race = c("White", "Black", "Hispanic", "Asian/NHPI", "AIAN/MR/Else"),
  Population = c(381324,436320,1810054,41200,20992))

md_county_barplot <-ggplot(data=md_county_data, aes(x=Race, y=Population)) +
  geom_bar(stat="identity") + theme_ipsum() + scale_fill_ipsum() + ggtitle("Miami-Dade County Race Demographics") +
  theme(plot.title = element_text(hjust = 0.5))

final_md_barplot <- md_county_barplot + scale_y_continuous(name = "Population") +
  scale_x_discrete(name = "Race") +
  geom_text(aes(label = Population), vjust = -0.3) +
  geom_col(aes(fill = Race)) +
  theme(
    axis.text.x = element_text(angle = 30),
    axis.title.x = element_text(hjust = 0.5, size = 20),
    axis.title.y = element_text(hjust = 0.5, size = 20,  vjust=5),
    plot.title = element_text(hjust = 0.5)
  )
final_md_barplot

###############################################################################

#broward county demographic visualization

fl_br_demographics_2016 <- fl_county_demographics %>%
  filter(COUNTIES == "Broward")

br_county_data <- data.frame(
  Race = c("White", "Black", "Hispanic", "Asian/NHPI", "AIAN/MR/Else"),
  Population = c(722559,534547,552008,70688,33978))

br_county_barplot <-ggplot(data=br_county_data, aes(x=Race, y=Population)) +
  geom_bar(stat="identity") + theme_ipsum() + scale_fill_ipsum() + ggtitle("Broward County Race Demographics") +
  theme(plot.title = element_text(hjust = 0.5))

final_br_barplot <- br_county_barplot + scale_y_continuous(name = "Population") +
  scale_x_discrete(name = "Race") +
  geom_text(aes(label = Population), vjust = -0.3) +
  geom_col(aes(fill = Race)) +
  theme(
    axis.text.x = element_text(angle = 30),
    axis.title.x = element_text(hjust = 0.5, size = 20),
    axis.title.y = element_text(hjust = 0.5, size = 20,  vjust=5),
    plot.title = element_text(hjust = 0.5)
  )
final_br_barplot

###############################################################################

#hillsborough county demographic visualization

fl_hi_demographics_2016 <- fl_county_demographics %>%
  filter(COUNTIES == "Hillsborough")

hi_county_data <- data.frame(
  Race = c("White", "Black", "Hispanic", "Asian/NHPI", "AIAN/MR/Else"),
  Population = c(696935,221581,391181,57783,30613))

hi_county_barplot <-ggplot(data=hi_county_data, aes(x=Race, y=Population)) +
  geom_bar(stat="identity") + theme_ipsum() + scale_fill_ipsum() + ggtitle("Hillsborough County Race Demographics") +
  theme(plot.title = element_text(hjust = 0.5))

final_hi_barplot <- hi_county_barplot + scale_y_continuous(name = "Population") +
  scale_x_discrete(name = "Race") +
  geom_text(aes(label = Population), vjust = -0.3) +
  geom_col(aes(fill = Race)) +
  theme(
    axis.text.x = element_text(angle = 30),
    axis.title.x = element_text(hjust = 0.5, size = 20),
    axis.title.y = element_text(hjust = 0.5, size = 20,  vjust=5),
    plot.title = element_text(hjust = 0.5)
  )
final_hi_barplot
