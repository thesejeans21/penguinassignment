library(palmerpenguins)
library(ggplot2)
library(janitor)
library(dplyr)

#Removed setwd for anonymity purposes

#Saving a safe copy of the raw data before we start cleaning it in case we make a mistake and need the original version

write.csv(penguins_raw, "data_raw/penguins_raw.csv")


# Defining the cleaning function

cleaning <- function(data_raw){
  data_raw %>%
    clean_names() %>%
    remove_empty(c("rows", "cols")) %>%
    select(-starts_with("delta")) %>%
    select(-comments)
}

#Loading penguins_raw data

penguins_raw <- read.csv("data_raw/penguins_raw.csv")

names(penguins_raw)

#Using cleaning function to clean penguins_raw

penguins_cleaner <- cleaning(penguins_raw)

names(penguins_cleaner)

#Saving the clean data to a seperate folder
write.csv(penguins_cleaner,"data_clean/penguins_cleaner.csv")

#Running statistical test
#Creating a linear regression model between culmen length and body mass

linmod <- lm(body_mass_g ~ culmen_length_mm, data=penguins_cleaner)

summary(linmod)

#Generating scatterplot with linear model

lm_figure <- ggplot(data=penguins_cleaner,
       aes(x = body_mass_g,
           y = culmen_length_mm,))+
  geom_point(color = "mediumblue", shape = 16, alpha = 0.75)+
  labs(title = "Linear Regression Model for Penguin Body Mass vs Culmen Length",
       x = "Penguin Body Mass (g)",
       y= "Penguin Culmen Length (mm)")+
  geom_smooth(method = lm, se=FALSE)+
  theme_light()

lm_figure

#Recording package versions 
sink(file = "package_versions/package-versions.txt")
sessionInfo()
sink()








