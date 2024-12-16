# Descriptive analysis of bivariate datasets using Correlation Coefficients
# Pearson Correlation Coefficients

library(dplyr)
library(ggplot2)

# Correlation coefficients of unrelated datasets

set.seed(123)
# This function creates a sequence of random numbers to be used in a code chunk
town.1 <- tibble(
  ID = 1:20,
  Age = sample(x = 15:50, size = 20, replace = TRUE),
  # The sample() function gives specifications on the variable being created
  # The 'x =' argument specifies the values to be used
  # The 'size =' argument specifies the number of observations in the variable
  # The 'replace = TRUE/FALSE' argument specifies that a value randomly selected can be repeated or not
  Weight = sample(x = 40:95, size = 20, replace = FALSE)
)
town.1

# Let me create a plot comparing values of our variables

ggplot(town.1, aes(Weight, Age)) +
  # The ggplot() function's first value should be the name of the dataframe being analysed
  # The aes() function specifies the variables being analysed in the dataframe, the first variable set to be displayed on the x axis and v/v
  geom_point() +
  # This function adds layers to the points of the scatterplot being created
  geom_text(aes(label = ID), nudge_x = 1.2, nudge_y = 1.2) +
  # This function adds labels to the dot plots in the plot
  # The first value should be the aes() function with the 'label =' argument passed with the variable containing the identifications for each observation
  # The second value is an argument 'nudge_x =' which specifies the position next to the plot on the x axis should be and the following argument specifies for position on the y axis
  theme_bw()
  # Specifies the theme for the plot to be constructed

# The scatterplot created shows how our datasets are not related as the plots are unevenly scattered without any form of linearisation

# Highlighting specific spots in the plot

ggplot(town.1, aes(Weight, Age)) +
  geom_point() +
  geom_text(aes(label = ID), nudge_x = 1.2, nudge_y = 1.2) +
  geom_vline(xintercept = 94, col = "red", size = 0.2) +
  # This function introduces a vertical and horizontal line across the plot used to highlight specific points
  # The x/y intercept argument specifies the specific points on the x and y axis it should intersect at; The 'col =' argument specifies the color of the line; and the 'size =' argument should specify the size of the line 
  geom_hline(yintercept = 31, col = "red", size = 0.2) +
  geom_point(aes(x, y), tibble(x = 51, y = 46), shape = 1, size = 14, col = "blue") +
  # This functions aes(x, y) function piped to it gives specifications on the axes of interest; The tibble() function specifies the specific observation being highlighted; The following arguments specify the structure of the highlight
  geom_point(aes(x, y), tibble(x = 78, y = 49), shape = 3, size = 12, col = "purple") +
  theme_bw()

# Let me get the exact statistical values for the correlation coefficient, the p value, the t value and the confidence intervals

cor.test(x = town.1$Weight, y = town.1$Age) %>% 
  print()
#The weak positive correlation coefficient and p value tells me that the relation between the age and weight variables is not significant if any
# The Weight and Age variables are statistically independent
# ============================================================================================================================================================================================================================================

# Analysis of related datasets
# Positively related datasets

tibble(
  Weight = seq(from = 40, to = 80, by = 2),
  Height.in = 70:90,
  r = cor(Weight, Height.in)
) %>% 
  ggplot() +
  geom_point(aes(Height.in, Weight)) +
  geom_text(aes(x = 73, y = 75, label = paste("rho =", r)), col = "violet") +
  # The first function has specified the position we want to put out text on, the 'label =' argument gives info about what it should be named and what data from our dataframe it is
  theme_classic()

# Negatively related datasets

tibble(
  Height.in = 75:95,
  Weight = 80:60,
  Correlation = cor(Height.in, Weight)
) %>% 
  ggplot() +
  geom_point(aes(Height.in, Weight)) +
  geom_text(aes(x = 77, y = 70, label = paste("Cor =", Correlation)), col = "maroon") +
  theme_classic()

# Let me now simulate data similar to real case studies

town.2 <- tibble(
  Age = c(25, 20, 40, 29, 15, 19, 28, 31, 38, 45, 33, 36, 25, 27, 30, 18, 16, 35, 37,
          49, 50, 22, 23, 55, 24, NA, 15, 17, 48, 51, 54, 44, 42, 41, NA, NA, 39, 53),
  Weight = c(60, 48, 85, 69, 40, 46, 90, 75, 77, 89, 70, 68, 61, 66, 41, 85, 43, 74, 80,
             NA, 99, 63, 90, 48, 67, 70, 44, 49, 87, 97, 60, 86, 79, 83, 55, 80, 69, NA),
   )

cor.test(town.2$Age, town.2$Weight)
# Let me create a linear regression plot representing this data

ggplot(town.2, aes(Age, Weight)) +
  geom_smooth(method = "lm") +
  geom_jitter() +
  geom_text(aes(x = 20, y = 100, label = "rho = 0.5333495"), col = "red") +
  theme_classic()

# ========================================================================================================================================================================================================================================

# Analysis of relationship between single Numerical /continuous and categorical values

town.2 <- tibble(
  Age = c(25, 20, 40, 29, 15, 19, 28, 31, 38, 45, 33, 36, 25, 27, 30, 18, 16, 35, 37,
          49, 50, 22, 23, 55, 24, NA, 15, 17, 48, 51, 54, 44, 42, 41, NA, NA, 39, 53),
  Age.group = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                2, 2, 1, 1, 2, 1, 2, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 1, 2),
  Sex = c(2, 2, 2, 1, 1, 1, 2, 1, 2, 1, 2, 1, 2, 2, 2, 1, 1, 1, 2, 2, 1, 2, 1, 2, 1, 2,
          2, 1, 1, 2, 1, 1, 2, 1, 1, 1, 1, 2),
  Weight = c(60, 48, 85, 69, 40, 46, 90, 75, 77, 89, 70, 68, 61, 66, 41, 85, 43, 74, 80,
             NA, 99, 63, 90, 48, 67, 70, 44, 49, 87, 97, 60, 86, 79, 83, 55, 80, 69, NA)
) %>% 
  mutate(
    Age.group = factor(Age.group, labels = c("40 and below", "60 and below")),
    Sex = factor(Sex, labels = c("Male", "Female"))
  ) %>% 
  print()

# Let me plot the values

town.2.summary <- town.2 %>% 
  filter(!is.na(Weight)) %>% 
  group_by(Age.group) %>% 
  summarise(
    Count = n(),
    Mean = mean(Weight),
    `Standard deviation` = sd(Weight),
    Minimum = min(Weight),
    Maximum = max(Weight)
  ) %>% 
  print()

town.2 %>% 
  filter(!is.na(Weight)) %>% 
  ggplot(aes(x = Age.group, y = Weight)) +
  geom_jitter(aes(col = Age.group), width = 0.20) +
  geom_segment(
    aes(x = c(0.75, 1.75), y = Mean, xend = c(1.25, 2.25), yend = Mean, col = "Age.group"), 
    size = 1.5, data = town.2.summary
  ) +
  scale_x_discrete("Age Group") +
  scale_y_continuous("Weight (Kgs)") +
  scale_color_manual(values = c("#BC581A", "#00519B", "black")) +
  theme_classic() +
  theme(legend.position = "none", axis.text.x = element_text(size = 12))

# Relationship between a single numerical/continuous variable and multiple categorical variables

town.2.summary.2 <- town.2 %>% 
  filter(!is.na(Weight)) %>% 
  group_by(Age.group, Sex) %>% 
  summarise(
    Count = n(),
    Mean = mean(Weight),
    `Standard deviation` = sd(Weight),
    Minimum = min(Weight),
    Maximum = max(Weight)
  ) %>% 
  print()

# Plot
