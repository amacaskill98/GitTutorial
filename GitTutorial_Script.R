#' _Github Demo_ 
#' Read in the data provided 
#' Plot a barchart to show the most popular lunch destination
#' Clean the data to make the plot better
#' Create your own version in a new repo for your own university/ site
#' 
#' Load libraries
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, # data manipulation
               viridis,
               leaflet,
               svglite)

#' Import data
df <- read_excel("data/favrests.xlsx") # proportion patients returning by dose

# Group and count venue choices 
ug <- df %>% 
  filter(University == "Glasgow")  %>%
  mutate_at(vars(Restaurant), factor) %>%
  group_by(Restaurant) %>%
  dplyr::summarise(n = length(Restaurant))

# make bar plot to show popular choices
p <- ggplot(data = ug, aes(x = Restaurant, y = n)) +
  geom_bar(stat="identity")
p

# Bit of a mess - lets clean up the data
#table(df$name) # get rid of non-unique names
table(df$University) # clean up misspellings

# examples of cleaning up messy data
df$Restaurant <- gsub("DM", "Dumpling monkey", df$Restaurant)
# or
df <- df %>%
  dplyr::mutate(Restaurant = recode(Restaurant,
                               'Sound Bitea' = 'Sound Bites',
                               'Sound Bite' = 'Sound Bites',
                               'IHI cafeteria' = 'IHI cafe'))

# save clean data (without overwriting the raw data!)
if(!dir.exists("outputs")){dir.create("outputs")}
write.csv(df, file = "outputs/favrests.xlsx", row.names = FALSE)

# replot
# make bar plot to show popular choices
ug <- df %>% 
  filter(University == "Glasgow")  %>%
  mutate_at(vars(Restaurant), factor) %>%
  group_by(Restaurant) %>%
  summarise(n = length(Restaurant)) %>%
  ggplot(aes(x = Restaurant, y = n)) 
  #geom_bar(stat="identity")
ug

# stacked barplot
p <- df %>% 
  group_by(Restaurant, University) %>%
  summarise(n = length(Restaurant)) %>%
  ggplot(aes(x = Restaurant, y = n, fill = University)) +
  # geom_bar(stat="identity") + 
  theme_minimal()
p

# save an svg file
if(!dir.exists("figs")){dir.create("figs")}
ggsave("figs/demo_stacked_chart.svg", p, device = "svg")

# See if the file can be edited in ppt! Follow instructions here:
# https://nalinan.medium.com/how-to-vectorize-plots-from-r-in-powerpoint-bad7c238e86a



