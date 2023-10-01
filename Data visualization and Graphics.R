library(palmerpenguins)
library(dplyr)
library(ggplot2)
library(purrr)
library(repurrrsive)
library(readr)
library(tidyverse)
library(explore)
library(ggridges)
library(pins)
library(lubridate)
penguins %>% 
  group_by(species) %>%
  summarize(mass_mean = mean(body_mass_g, na.rm = TRUE)) %>%
  ggplot(aes(x = species, y = mass_mean)) +
  # what happens if you don't include the stat argument?
  geom_bar(stat = "identity")
  ##################
penguins %>% 
  ggplot(aes(x = body_mass_g)) +
  geom_histogram()
##########
penguins %>% 
  ggplot(aes(x = bill_length_mm, y = bill_depth_mm)) +
  geom_point()
#############
penguins %>% 
  ggplot(aes(x = bill_length_mm, y = bill_depth_mm)) +
  geom_point(color = "blue", size = 2)
#############
penguins %>% 
  ggplot(aes(x = bill_length_mm, y = bill_depth_mm)) +
  geom_point(aes(color = species), size = 2)
###############
penguins %>% 
  ggplot(aes(x = bill_length_mm, y = bill_depth_mm)) +
  geom_point(aes(color = species), size = 2) +
  scale_color_manual(values = c("#aa6600","#666666","#224477"))
###############

penguins %>% 
  ggplot(aes(x = bill_length_mm, y = bill_depth_mm)) +
  geom_point(aes(color = species), size = 2) +
  scale_color_manual(values = c("#aa6600","#666666","#224477")) +
  scale_x_continuous(name = "Bill Length (mm)", 
                     breaks = seq(30, 60, by = 5), limits = c(30, 60))
######################
penguins %>% 
  group_by(species) %>%
  summarize(mass_mean = mean(body_mass_g, na.rm = TRUE)) %>% 
  ggplot(aes(x = species, y = mass_mean)) +
  geom_bar(stat = "identity")
#################
penguins %>% 
group_by(species) %>%
  summarize(mass_mean = mean(body_mass_g, na.rm = TRUE)) %>% 
  ggplot(aes(x = species, y = mass_mean)) +
  geom_bar(stat = "identity") +
  coord_flip()

##########

penguins %>% 
  count(sex) %>% 
  ggplot(aes(x = "", y = n, fill = sex)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  theme_void()
##########
penguins %>% 
  filter(!is.na(sex)) %>% 
  ggplot(aes(x = sex, y = body_mass_g)) +
  geom_point(position = "jitter")
############
penguins %>% 
  filter(!is.na(sex)) %>% 
  ggplot(aes(x = sex, y = body_mass_g)) +
  geom_point(position = "jitter") +
  facet_wrap(~species)
######
penguins %>% 
  ggplot(aes(x = bill_length_mm, y = bill_depth_mm)) +
  geom_point(aes(color = species), size = 2) +
  scale_color_manual(values = c("#aa6600","#666666","#224477"))
########
penguins %>% 
  ggplot(aes(x = bill_length_mm, y = bill_depth_mm)) +
  geom_point(aes(color = species), size = 2) +
  scale_color_manual(values = c("#aa6600","#666666","#224477")) +
  theme(legend.position = "top", legend.title = element_blank())
#########
penguins %>% 
  filter(!is.na(sex)) %>% 
  ggplot(aes(x = sex, y = body_mass_g)) +
  geom_point(position = "jitter") +
  facet_wrap(~species) +
  theme(panel.grid.major.x = element_blank())
#######
penguins %>% 
  count(species)
#####
penguins %>% 
  filter(species == "Gentoo") %>% 
  count(species)
######

penguins %>% 
  filter(species %in% c("Gentoo","Chinstrap")) %>% 
  count(species)
######
penguins %>% 
  filter(species == "Gentoo" | species == "Chinstrap") %>% 
  count(species)
###########
penguins %>% 
  filter(species != "Adelie") %>% 
  count(species)
##########
penguins %>% 
  filter(!is.na(sex)) %>% 
  count(sex)
#######
penguins %>% 
  group_by(species) %>% 
  summarize(mass_mean = mean(body_mass_g))
####
penguins %>% 
  filter(is.na(body_mass_g))
#####
penguins %>% 
  group_by(species) %>% 
  summarize(mass_mean = mean(body_mass_g, na.rm = TRUE))
##### select() is like filter() but for variables instead of observations###
#### arrange() sorts data
#### mutate() creates new variables (ifelse and case_when are often useful)
#### rename() does exactly what you think
#### left_join() (and other joins) combines data frames based on common keys
#### str_detect() detects whether or not a pattern is present in a string
#### str_replace() replaces a pattern in a string with something else
#### pivot_longer() "lengthens" data, increasing the number of rows & decreasing the number of columns
#### pivot_wider() does the opposite
########################################
penguins1 <- mutate(penguins, big_penguin = 
                      ifelse(body_mass_g >= 5000, "yes", "no"))
penguins2 <- filter(penguins1, !is.na(body_mass_g))

#############
big_penguins <- penguins %>% 
  mutate(penguins, big_penguin = 
           ifelse(body_mass_g >= 5000, "yes", "no")) %>% 
  filter(!is.na(body_mass_g))
###############
penguins_pivoted <- big_penguins %>% 
  pivot_longer(`2007`:`2009`, 
               names_to = "year", values_to = "body_mass")
#################
getwd()

got <- read_csv("battles.csv")
got %>% 
  glimpse()
########
got %>% 
  ggplot(aes(x = region)) + 
  geom_bar()
######
got %>% 
  ggplot(aes(x = region)) + 
  geom_bar() +
  coord_flip()
########
got %>% 
  count(region) %>%
  ggplot(aes(x = reorder(region, n), y = n)) + 
  geom_bar(stat = "identity") +
  coord_flip()
#########
got %>% 
  count(region) %>% 
  ggplot(aes(x = reorder(region, n), y = n)) + 
  geom_bar(stat = "identity") +
  coord_flip() +
  geom_text(aes(label = n), hjust = -0.5) +
  scale_y_continuous(limits = c(0, 18))
########
got %>%
  ggplot(aes(x = attacker_king, fill = battle_type)) + 
  geom_bar(position = "dodge")
######
got %>%
  filter(!is.na(attacker_king) & !is.na(battle_type)) %>%
  ggplot(aes(x = attacker_king, fill = battle_type)) + 
  geom_bar(position = "dodge") +
  theme(legend.position = c(0.1, 0.8))
#################

got %>%
  group_by(attacker_king, battle_type) %>%
  summarize(mean_size = mean(attacker_size, na.rm = TRUE)) %>%
  filter(!is.na(attacker_king) & !is.na(battle_type)) %>% 
  ggplot(aes(x = attacker_king, y = mean_size, fill = battle_type)) + 
  geom_bar(stat = "identity", position = "dodge") +
  theme(legend.position = c(0.1, 0.8))
################
got %>% 
  filter(!is.na(attacker_king) & !is.na(region)) %>% 
  ggplot(aes(x = attacker_king, fill = region)) + 
  geom_bar(position = "stack")
##########
got %>% 
  filter(!is.na(attacker_king) & !is.na(region)) %>% 
  ggplot(aes(x = attacker_king)) + 
  geom_bar() +
  facet_wrap(~region)
###########
got %>% 
  filter(!is.na(attacker_king) & !is.na(region)) %>% 
  ggplot(aes(x = region)) + 
  geom_bar() +
  coord_flip() +
  facet_wrap(~attacker_king)
########
got %>% 
  add_count(attacker_king) %>%
  ggplot(aes(x = attacker_king, y = n, fill = attacker_outcome)) +
  geom_bar(stat = "identity", position = "fill")
########
got %>% 
  filter(!is.na(attacker_king) & !is.na(attacker_outcome)) %>% 
  add_count(attacker_king) %>% 
  ggplot(aes(x = attacker_king, y = n, fill = attacker_outcome)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_y_continuous(name = "Percent Win", labels = scales::percent)
#########
got %>% 
  select(battle_number, attacker_king, 
         defender_king, attacker_outcome) %>% 
  pivot_longer(ends_with("king"), names_to = "type",
               values_to = "king", values_drop_na = TRUE)
#########
got %>% 
  select(battle_number, attacker_king, 
         defender_king, attacker_outcome) %>% 
  pivot_longer(ends_with("king"), names_to = "type",
               values_to = "king", values_drop_na = TRUE) %>% 
  mutate(outcome = case_when(type == "attacker_king" ~ attacker_outcome,
                             attacker_outcome == "win" ~ "loss",
                             attacker_outcome == "loss" ~ "win"))
###########
got %>% 
  select(battle_number, attacker_king, 
         defender_king, attacker_outcome) %>% 
  filter(!is.na(attacker_outcome)) %>% 
  pivot_longer(ends_with("king"), names_to = "type",
               values_to = "king", values_drop_na = TRUE) %>% 
  mutate(outcome = case_when(type == "attacker_king" ~ attacker_outcome,
                             attacker_outcome == "win" ~ "loss",
                             attacker_outcome == "loss" ~ "win")) %>% 
  add_count(king) %>% 
  ggplot(aes(x = king, y = n, fill = outcome)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_y_continuous(name = "Percent Win", labels = scales::percent)
##############
got %>% 
  select(battle_number, attacker_commander) %>% 
  separate(attacker_commander, c("a","b","c","d","e","f"),
           sep = ", ")
########
got %>% 
  select(battle_number, attacker_commander) %>% 
  separate(attacker_commander, c("a","b","c","d","e","f"),
           sep = ", ")%>% 
  pivot_longer(a:f, values_to = "commander",
               names_to = NULL, values_drop_na = TRUE)
#########
got %>% 
  select(battle_number, attacker_commander) %>% 
  separate(attacker_commander, c("a","b","c","d","e","f"), 
           sep = ", ") %>% 
  pivot_longer(a:f, values_to = "commander", 
               names_to = NULL, values_drop_na = TRUE) %>% 
  count(commander) %>%
  ggplot(aes(x = commander, y = n)) +
  geom_bar(stat = "identity")
###########
got %>% 
  select(battle_number, attacker_commander) %>% 
  separate(attacker_commander, c("a","b","c","d","e","f"), 
           sep = ", ") %>% 
  pivot_longer(a:f, values_to = "commander", 
               names_to = NULL, values_drop_na = TRUE) %>% 
  count(commander) %>%
  filter(n > 2) %>%
  ggplot(aes(x = reorder(commander, -n), y = n)) +
  geom_bar(stat = "identity")
###########

got %>%
  filter(!is.na(attacker_king)) %>% 
  count(attacker_king, year) %>% 
  ggplot(aes(x = attacker_king, y = n, fill = year)) + 
  geom_bar(stat = "identity", position = "dodge")
##########
got %>%
  filter(!is.na(attacker_king)) %>% 
  count(attacker_king, year) %>% 
  ggplot(aes(x = attacker_king, y = n, fill = as.character(year))) +
  geom_bar(stat = "identity", position = "dodge")
####
got %>%
  filter(!is.na(attacker_king)) %>% 
  count(attacker_king, year) %>% 
  ggplot(aes(x = attacker_king, y = n, fill = as.character(year))) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = n), position = position_dodge(0.9),
            vjust = -0.5)
######################
got %>%
  filter(!is.na(attacker_king)) %>% 
  count(attacker_king, year) %>% 
  ggplot(aes(x = attacker_king, y = n, fill = as.character(year))) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = n), position = position_dodge(0.9), 
            vjust = -0.5) +
  labs(fill = "Year",
       title = "Number of battles by attacking king and year") +
  theme(axis.title = element_blank(),
        axis.text = element_text(size = 12),
        plot.title = element_text(size = 12))
#################
got %>%
  filter(!is.na(attacker_king)) %>% 
  count(attacker_king, year) %>% 
  mutate(attacker_king = str_replace(attacker_king, "/", "/\n"),
         attacker_king = str_replace(attacker_king, " ", "\n")) %>%
  ggplot(aes(x = attacker_king, y = n, fill = as.character(year))) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = n), position = position_dodge(0.9), 
            vjust = -0.5) +
  labs(fill = "Year",
       title = "Number of battles by attacking king and year") + 
  theme(axis.title = element_blank(),
        axis.text = element_text(size = 14),
        plot.title = element_text(size = 16))
#############
url <- "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-25/chopped.tsv"
chopped <- read_tsv(url)
chopped %>% 
  glimpse()
########
chopped %>% 
  ggplot(aes(x = episode_rating)) +
  geom_histogram()
#########
chopped %>% 
  ggplot(aes(x = episode_rating)) +
  geom_histogram(bins = 5)
#######
chopped %>% 
  ggplot(aes(x = episode_rating)) +
  geom_histogram(bins = 100)
###########
chopped %>% 
  ggplot(aes(x = episode_rating)) +
  geom_histogram(bins = 35)
#######
chopped %>% 
  ggplot(aes(x = episode_rating)) +
  geom_histogram(binwidth = 0.1,  fill = "#ce7232", color = "black")
#####
chopped %>% 
  mutate(berry_dessert = ifelse(str_detect(dessert, "berries"), 
                                "Berry Dessert", "Berry-Free Dessert")) %>% 
  ggplot(aes(x = episode_rating, fill = berry_dessert)) +
  geom_histogram(binwidth = 0.1)
########
chopped %>% 
  ggplot(aes(x = episode_rating)) +
  geom_density()
#########
chopped %>% 
  ggplot(aes(x = episode_rating)) +
  geom_density(fill = "#ce7232", alpha = 0.75)
########
chopped %>% 
  ggplot(aes(x = episode_rating, y = ..density..)) +
  # the y argument scales down the histogram 
  # to match the density curve
  geom_histogram(binwidth = 0.1, fill = "#ce7232", 
                 color = "black", alpha = 0.75) +
  geom_density()
#########
chopped %>% 
  mutate(berry_dessert = ifelse(str_detect(dessert, "berries"), 
                                "Berry Dessert", "Berry-Free Dessert")) %>% 
  ggplot(aes(x = episode_rating, fill = berry_dessert)) +
  geom_density(alpha = 0.4)
#########
chopped %>% 
  mutate(berry_dessert = ifelse(str_detect(dessert, "berries"), 
                                "Berry Dessert", "Berry-Free Dessert")) %>% 
  ggplot(aes(x = episode_rating, color = berry_dessert)) +
  geom_density()
#######
chopped %>% 
  mutate(berry_dessert = ifelse(str_detect(dessert, "berries"), 
                                "Berry Dessert", "Berry-Free Dessert")) %>% 
  filter(!is.na(episode_rating)) %>% 
  ggplot(aes(x = episode_rating)) +
  geom_density(fill = "#ce7232", alpha = 0.5) +
  facet_wrap(~berry_dessert)
##########
chopped %>% 
  mutate(berry_dessert = ifelse(str_detect(dessert, "berries"), 
                                "Berry Dessert", "Berry-Free Dessert")) %>% 
  filter(!is.na(episode_rating)) %>% 
  ggplot(aes(y = berry_dessert, x = episode_rating)) +
  geom_boxplot()
#####
chopped %>%
  mutate(year = str_sub(air_date, -4)) %>% 
  ggplot(aes(y = year, x = episode_rating)) +
  geom_boxplot()
#######
chopped %>% 
  select(date) %>% 
  mutate(year = str_sub(air_date, -4))
#####
chopped %>%
  mutate(year = str_sub(air_date, -4)) %>% 
  ggplot(aes(y = year, x = episode_rating)) +
  geom_boxplot()
####
chopped %>%
  mutate(year = str_sub(air_date, -4)) %>% 
  ggplot(aes(y = year, x = episode_rating)) +
  geom_boxplot(fill = "gray", outlier.color = "#ce7232", 
               outlier.shape = "circle open", outlier.size = 2)
#### Highlighting a certain group
chopped %>% 
  mutate(year = str_sub(air_date, -4)) %>% 
  mutate(highlight_2013 = ifelse(year == 2013, 
                                 "highlight", "normal")) %>% 
  ggplot(aes(y = year, x = episode_rating, fill = highlight_2013)) +
  geom_boxplot() +
  scale_fill_manual(values = c("#bb0000","#dddddd")) +
  theme(legend.position = "none")
#######marking the mean

chopped %>%
  mutate(year = str_sub(air_date, -4)) %>% 
  ggplot(aes(y = year, x = episode_rating)) +
  geom_boxplot(fill = "gray") +
  stat_summary(fun = mean, geom = "point",
               shape = "square", size = 2, color = "red")
###### adding jittered points

chopped %>%
  mutate(year = str_sub(air_date, -4)) %>% 
  ggplot(aes(y = year, x = episode_rating)) +
  geom_boxplot() +
  geom_jitter(color = "black", size = 0.4, alpha = 0.5)
######suppressing outliers

chopped %>%
  mutate(year = str_sub(air_date, -4)) %>% 
  ggplot(aes(y = year, x = episode_rating)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(color="black", size = 0.4, alpha = 0.5)
##### Violin Plots
chopped %>% 
mutate(berry_dessert = ifelse(str_detect(dessert, "berries"), 
                              "Berry Dessert", "Berry-Free Dessert")) %>% 
  filter(!is.na(episode_rating)) %>% 
  ggplot(aes(y = berry_dessert, x = episode_rating)) + 
  geom_violin()
#####overlaying a boxplot

chopped %>% 
  mutate(berry_dessert = ifelse(str_detect(dessert, "berries"), 
                                "Berry Dessert", "Berry-Free Dessert")) %>% 
  filter(!is.na(episode_rating)) %>% 
  ggplot(aes(y = berry_dessert, x = episode_rating)) + 
  geom_violin() +
  geom_boxplot(width = 0.1, color = "grey", alpha = 0.5,
               outlier.colour = "red")
#####
chopped %>% 
  select(series_episode, episode_rating, judge1:judge3) %>% 
  pivot_longer(judge1:judge3, 
               values_to = "judge", names_to = NULL) %>% 
  add_count(judge)
######
chopped %>% 
  select(series_episode, episode_rating, judge1:judge3) %>% 
  pivot_longer(judge1:judge3, 
               values_to = "judge", names_to = NULL) %>% 
  add_count(judge) %>% 
  filter(n > 100) %>% 
  ggplot(aes(y = judge, x = episode_rating)) +
  geom_violin() +
  geom_boxplot(width = 0.1, color = "grey", alpha = 0.5,
               outlier.colour = "red")
########adding a custom axis

chopped %>% 
  select(series_episode, episode_rating, judge1:judge3) %>% 
  pivot_longer(judge1:judge3, 
               values_to = "judge", names_to = NULL) %>% 
  add_count(judge) %>% 
  filter(n > 100) %>% 
  mutate(custom_axis = paste0(judge, "\n", n, " episodes")) %>%
  ggplot(aes(y = custom_axis, x = episode_rating)) +
  geom_violin() +
  geom_boxplot(width = 0.1, color = "grey", alpha = 0.5,
               outlier.colour = "red")
##### ordering the custom axis

chopped %>% 
  select(series_episode, episode_rating, judge1:judge3) %>% 
  pivot_longer(judge1:judge3, 
               values_to = "judge", names_to = NULL) %>% 
  add_count(judge) %>% 
  filter(n > 100) %>% 
  mutate(custom_axis = paste0(judge, "\n", n, " episodes")) %>% 
  ggplot(aes(y = reorder(custom_axis, n), x = episode_rating)) +
  geom_violin() +
  geom_boxplot(width = 0.1, color = "grey", alpha = 0.5,
               outlier.colour = "red")
##### 
chopped %>% 
  select(episode_rating, judge1:judge3) %>% 
  pivot_longer(judge1:judge3, values_to = "judge", 
               names_to = NULL) %>% 
  add_count(judge) %>% 
  filter(n > 100) %>% 
  ggplot(aes(x = episode_rating, y = judge)) +
  geom_density_ridges()
####
chopped %>% 
  select(episode_rating, judge1:judge3) %>% 
  pivot_longer(judge1:judge3, values_to = "judge", 
               names_to = NULL) %>% 
  add_count(judge) %>% 
  filter(n > 100) %>% 
  ggplot(aes(x = episode_rating, y = judge)) +
  geom_density_ridges(stat = "binline", binwidth = 0.1)
#### including a median line

chopped %>% 
  select(episode_rating, judge1:judge3) %>% 
  pivot_longer(judge1:judge3, values_to = "judge", 
               names_to = NULL) %>% 
  add_count(judge) %>% 
  filter(n > 100 & !is.na(episode_rating)) %>% 
  ggplot(aes(x = episode_rating, y = judge, episode_rating)) +
  stat_density_ridges(quantile_lines = TRUE, quantiles = 2)

##### ordering by mean

chopped %>% 
  select(episode_rating, judge1:judge3) %>% 
  pivot_longer(judge1:judge3, values_to = "judge", 
               names_to = NULL) %>% 
  add_count(judge) %>% 
  filter(n > 100 & !is.na(episode_rating)) %>% 
  mutate(judge = fct_reorder(judge, episode_rating, .fun = mean)) %>%
  ggplot(aes(x = episode_rating, y = judge)) +
  geom_density_ridges()
####### adding jittered points

chopped %>% 
  select(episode_rating, judge1:judge3) %>% 
  pivot_longer(judge1:judge3, values_to = "judge", 
               names_to = NULL) %>% 
  add_count(judge) %>% 
  filter(n > 100) %>% 
  ggplot(aes(x = episode_rating, y = judge)) +
  geom_density_ridges(jittered_points = TRUE,
                      alpha = 0.5, point_size = 0.5)
####### adding a rug plot

chopped %>% 
  select(episode_rating, judge1:judge3) %>% 
  pivot_longer(judge1:judge3, 
               values_to = "judge", names_to = NULL) %>% 
  add_count(judge) %>% 
  filter(n > 100) %>% 
  ggplot(aes(x = episode_rating, y = judge)) +
  geom_density_ridges(jittered_points = TRUE,
                      position = position_points_jitter(width = 0.05, height = 0),
                      point_shape = '|', point_size = 3, point_alpha = 1,
                      alpha = 0.7)
#### adding a raincloud plot

chopped %>% 
  mutate(berry_dessert = ifelse(str_detect(dessert, "berries"), 
                                "Berry Dessert", "Berry-Free Dessert")) %>% 
  filter(!is.na(episode_rating)) %>% 
  ggplot(aes(x = episode_rating, y = berry_dessert)) +
  geom_density_ridges(jittered_points = TRUE, alpha = 0.5, 
                      point_size = 0.5, scale = 0.6,
                      position = "raincloud")
#####
mpg %>% 
  group_by(drv) %>% 
  summarize(mean_hwy_mpg = mean(hwy))
#####
mpg %>%
  filter(year == 2008) %>%
  group_by(manufacturer)
summarize(median_size = median(cty, na.rm = TRUE))
#### Line graph: Most commonly: when looking at values over time
data(economics)
ggplot(economics, aes(x = date, y = unemploy)) +
  geom_line() +
  labs(x = "Year", y = "Unemployment") +
  ggtitle("Unemployment over Time")
#######
data(economics)
# Step 3: Create the line graph
ggplot(economics, aes(x = date, y = unemploy)) +
  geom_line() +
  labs(x = "Year", y = "Unemployment") +
  ggtitle("Unemployment over Time")+
geom_area(fill = "#E0610E")
############
data(economics)
# Step 3: Create the line graph
ggplot(economics, aes(x = date, y = unemploy)) +
  geom_line() +
  labs(x = "Year", y = "Unemployment") +
geom_line(color = "#E0610E", size = 1.5, linetype = "dotted")
############
data(economics)
# Step 3: Create the line graph
ggplot(economics, aes(x = date, y = unemploy)) +
  geom_line() +
  labs(x = "Year", y = "Unemployment") +
  geom_line(color = "#E0610E", size = 1.5, linetype = "dotted")+
geom_point(alpha = 0.5, color = "black", size = 2)
######### Spaghatti graph
data(economics)
# Step 3: Create the line graph
ggplot(economics, aes(x = date, y = unemploy)) +
  geom_line() +
  labs(x = "Year", y = "Unemployment") +
  scale_x_continuous(breaks = seq(1, 17, 1))  + scale_x_continuous(breaks = seq(1, 17, 1)) +
  labs(color = "")
######## one way to reorder a legend
data(economics)
ggplot(economics, aes(x = date, y = unemploy)) +
geom_line() +
  scale_x_continuous(breaks = seq(1, 17, 1)) +
  labs(color = "")
######## Scatter plot: To study a relationship between two numeric variables
url <- "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-08/friends_info.csv"
friends_info <- read.csv(url)
friends_info %>% 
  ggplot(aes(x = us_views_millions, y = imdb_rating)) +
  geom_point()
#####
friends_info %>% 
  ggplot(aes(x = us_views_millions, y = imdb_rating)) +
  geom_point(alpha = 0.5, color = "red", size = 2)
#####
friends_info %>% 
  ggplot(aes(x = us_views_millions, y = imdb_rating)) +
  geom_jitter()
####
friends_info %>% 
  ggplot(aes(x = us_views_millions, y = imdb_rating, color = season)) +
  geom_jitter()
######
friends_info %>% 
  ggplot(aes(x = us_views_millions, y = imdb_rating, 
             color = as.character(season))) +
  geom_jitter()
###########
friends_info %>% 
  ggplot(aes(x = us_views_millions, y = imdb_rating, color = season)) +
  geom_jitter(size = 2) +
  scale_colour_gradient(low = "#fafafa", high = "#191970",
                        breaks = seq(1, 10, 1))
#################Scatter plot with best-fit line
friends_info %>% 
  ggplot(aes(x = us_views_millions, y = imdb_rating)) +
  geom_jitter(size = 2) +
  geom_smooth(method = "lm")
############
friends_info %>% 
  ggplot(aes(x = us_views_millions, y = imdb_rating)) +
  geom_jitter(size = 2) +
  geom_smooth(method = "lm", se = FALSE)
########## 
friends_info %>% 
  ggplot(aes(x = us_views_millions, y = imdb_rating)) +
  geom_jitter(size = 2) +
  geom_smooth(method = "lm", level = 0.99,
              color = "purple", fill = "#DCD0FF")
############## DATA Manipulation
url <- "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-08/friends_emotions.csv"
friends_emotions <- read.csv(url)
friends_joyful_sad <- friends_emotions %>% 
  group_by(season, episode, emotion) %>%
  summarize(count = n()) %>%
  add_count(wt = count) %>% 
  mutate(percent = count / n) %>% 
  filter(emotion %in% c("Joyful","Sad")) %>% 
  select(-c(count, n)) %>% 
  pivot_wider(names_from = emotion, values_from = percent) %>% 
  mutate(Sad = replace_na(Sad, 0))
friends_joyful_sad
########
friends_joyful_sad <- friends_emotions %>% 
  group_by(season, episode, emotion) %>% 
  summarize(count = n()) %>% 
  add_count(wt = count) %>% 
  mutate(percent = count / n) %>% 
  filter(emotion %in% c("Joyful","Sad")) %>%
  select(-c(count, n)) %>% 
  pivot_wider(names_from = emotion, values_from = percent) %>% 
  mutate(Sad = replace_na(Sad, 0))
friends_joyful_sad
##########
friends_info %>% 
  left_join(friends_joyful_sad, by = c("episode","season"))
######
friends_info %>% 
  left_join(friends_joyful_sad, by = c("episode","season")) %>% 
  filter(season <= 4) %>% 
  ggplot(aes(x = us_views_millions, y = imdb_rating, color = Joyful)) +
  geom_jitter()
##########
friends_info %>% 
  left_join(friends_joyful_sad, by = c("episode","season")) %>% 
  filter(season <= 4) %>% 
  ggplot(aes(x = us_views_millions, y = imdb_rating, color = Joyful)) +
  geom_jitter() +
  scale_colour_gradient(low = "#fafafa", high = "#191970", 
                        breaks = seq(0.1, 0.4, 0.1))
#######
url <- "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-08/friends.csv"
friends <- read.csv(url)
friends_top_actor <- friends %>% 
  group_by(season, episode, speaker) %>%
  summarize(count = n()) %>%
  add_count(wt = count) %>%
  mutate(percent = count / n) %>%
  filter(speaker %in% c("Chandler Bing","Joey Tribbiani",
                        "Monica Geller","Phoebe Buffay",
                        "Rachel Green","Ross Geller")) %>% 
  filter(percent == max(percent)) %>% 
  select(-c(count, n, percent))
friends_top_actor 
#######
friends_info %>% 
  left_join(friends_top_actor, by = c("episode","season")) %>%
  #####
friends_info %>% 
  left_join(friends_top_actor, by = c("episode","season")) %>% 
  ggplot(aes(x = us_views_millions, y = imdb_rating, color = speaker)) +
  geom_jitter(size = 2)
######## Data with Overploting
###When the sample size is large, overplotting can disguise trends
###Techniques:
###use smaller dots and/or transparency
###add color by group
###add jittering
###add a rug plot
getwd()
setwd("/Volumes/Behrooz/code/Rcodes")
txhousing<-read.csv("txhousing.csv")
txhousing %>% 
  filter(month == 1 & listings < 20000) %>% 
  ggplot(aes(x = median, y = listings)) +
  geom_point()
####################
txhousing %>% 
  filter(month == 1 & listings < 20000) %>% 
  ggplot(aes(x = median, y = listings)) +
  geom_point() +
  geom_rug(color = "purple", alpha = 0.1, size = 2)
######### add a marginal distribution
txhousing %>% 
  filter(month == 1 & listings < 20000) %>% 
  ggplot(aes(x = median, y = listings)) +
  geom_hex(bins = 30)
###########
storms %>% 
  ggplot(aes(x = wind, y = pressure)) +
  geom_hex(bins = 30)
############
####Bubble Chart
####Adding a third quant. variable to size

#####Not generally recommended!
##  Why?
 # - encoding the same type of variable (numeric) on two different scales: position and size
#- hard to compare the strengths of different associations
#- much easier to perceive differences when encoded by position rather than size
#- hard to see small differences in size
#- difficult to match scale of circle size to scale of difference
penguins %>% 
  filter(species != "Gentoo") %>% 
  ggplot(aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point()
#####
penguins %>% 
  filter(species != "Gentoo") %>% 
  ggplot(aes(x = flipper_length_mm, y = body_mass_g, 
             size = bill_length_mm)) +
  geom_point()
#######
penguins %>% 
  filter(species != "Gentoo") %>% 
  ggplot(aes(x = flipper_length_mm, y = body_mass_g, 
             size = bill_length_mm)) +
  geom_point(alpha = 0.5, color = "red") +
  scale_size(range = c(0.1, 7), breaks = c(35, 40, 45, 50, 55)) +
  theme(legend.position = "top")
#####
library(ggrepel)
penguins %>% 
  filter(species != "Gentoo") %>% 
  mutate(label = case_when(flipper_length_mm == 192 & 
                             body_mass_g == 2700 ~ "Chinstrap",
                           flipper_length_mm == 184 & 
                             body_mass_g == 4650 ~ "Adelie")) %>% 
  ggplot(aes(x = flipper_length_mm, y = body_mass_g, 
             size = bill_length_mm, color = species)) +
  geom_point(alpha = 0.5) +
  scale_size(range = c(0.1, 7), breaks = c(35, 40, 45, 50, 55), 
             name = "Bill Length (mm)") +
  geom_label_repel(aes(x = flipper_length_mm, y = body_mass_g, 
                       color = species, label = label), 
                   inherit.aes = FALSE) +
  scale_color_discrete(guide = "none") +
  theme(legend.position = "top")
##### Aggregating data
#####two methods, depending on needs
# Calculate mean price by city and year
txhousing <- txhousing %>% 
  group_by(city, year) %>% 
  mutate(mean_price = mean(median, na.rm = TRUE))

# Create the heatmap
txhousing %>% 
  ggplot(aes(x = year, y = city, fill = mean_price)) +
  geom_tile() +
  labs(x = "Year", y = "City", fill = "Mean Price") +
  scale_fill_viridis_c() +
  theme_minimal()
#### DOT PLOT
storms_agg <- storms %>% 
  filter(status == "hurricane" & year >= 2011) %>% 
  group_by(name, year) %>% 
  summarize(max_wind = max(wind))
####
storms_agg <- storms %>% 
  filter(status == "hurricane" & year >= 2011) %>% 
  group_by(name, year) %>% 
  summarize(max_wind = max(wind)) %>% 
  mutate(name_date = paste0(name, " (", year, ")"
  #######3
  storms_agg %>% 
    ggplot(aes(x = wind_mph, y = reorder(name_date, wind_mph))) +
    geom_point(color = "dark blue", size = 2) +
    labs(x = "Max Wind Speed (mph)", y = "",
         title = "Max wind speed of hurricanes, 2011 to 2015")  
  #########Pie Chart
  penguins %>% 
    count(species) %>% 
    ggplot(aes(x="", y = n, fill = species)) +
    geom_bar(stat = "identity", width = 1, color = "black") +
    coord_polar("y", start = 0)
  ######
  penguins %>% 
    count(species) %>% 
    ggplot(aes(x="", y = n, fill = species)) +
    geom_bar(stat = "identity", width = 1, color = "black") + 
    coord_polar("y", start = 0) +
    theme_void()
  #########Comparing dist. to a whole 
  #####distribution comparison with geom_density
  penguins %>% 
    ggplot(aes(x = body_mass_g, y = ..count..)) +
    geom_density_line(data = select(penguins, -species),
                      aes(fill = "all penguins"), color = "transparent")
  ########
  penguins %>% 
    ggplot(aes(x = body_mass_g, y = ..count..)) +
    geom_density_line(data = select(penguins, -species), 
                      aes(fill = "all penguins"), color = "transparent") +
    geom_density_line(aes(fill = "species"), color = "transparent") +
    facet_wrap(~species, nrow = 1)
  ######
  penguins %>% 
    ggplot(aes(x = body_mass_g, y = ..count..)) +
    geom_density_line(data = select(penguins, -species), 
                      aes(fill = "all penguins"), color = "transparent") +
    geom_density_line(aes(fill = "species"), color = "transparent") +
    facet_wrap(~species, nrow = 1) +
    scale_fill_manual(values = c("grey","#0C8346"), name = NULL,
                      guide = guide_legend(direction = "horizontal")) +
    labs(x = "Body Mass (g)") +
    theme(legend.position = "top")
  
  #########
  
