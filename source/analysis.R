library(tidyverse)
library(scales)
library(usmap)

# The functions might be useful for A4
source("../source/a4-helpers.R")

## Test queries ----
#----------------------------------------------------------------------------#
# Simple queries for basic testing
#----------------------------------------------------------------------------#
# Return a simple string
test_query1 <- function() {
  return ("Hello world")
}

# Return a vector of numbers
test_query2 <- function(num=6) {
  v <- seq(1:num)
  return(v)
}

## Section 2  ---- 
#----------------------------------------------------------------------------#
# Your functions and variables might go here ... <todo: update comment>
df <- read.csv("/Users/devdhawan/Documents/info201/data/jail.cvs")

get_aa_comp_wa_incarceration_pop <- function()
{
  new_df <- df %>% group_by(year) %>% filter(black_jail_pop != "NA") %>% summarise(black_jail_pop = sum(black_jail_pop))
  new_df2 <- df %>% group_by(year) %>% filter(white_jail_pop != "NA") %>% summarise(white_jail_pop = sum(white_jail_pop))
  new_df <- left_join(new_df, new_df2)
  new_df <- new_df %>% group_by(year) %>% summarise(prop = black_jail_pop/white_jail_pop)
  new_df <- new_df %>% filter(prop != "NaN")
  return(new_df)
}

plot_aa_comp_wa_incarceration_pop <- function()
{
  return(ggplot(get_aa_comp_wa_incarceration_pop(), aes(x = year, y = prop)) + geom_bar(stat = "identity", position = "dodge") + scale_y_continuous(labels = label_comma()) + labs(title = "Proportion of African American to White American Jail Population Distribution in the US from 1970 to 2018", x = "Year", y = "Total jail population"))
}

get_aa_comp_wa_incarceration_pop_state <- function()
{
  new_df <- df %>% group_by(state) %>% filter(black_jail_pop != "NA") %>% summarise(black_jail_pop = sum(black_jail_pop))
  new_df2 <- df %>% group_by(state) %>% filter(white_jail_pop != "NA") %>% summarise(white_jail_pop = sum(white_jail_pop))
  new_df <- left_join(new_df, new_df2)
  new_df <- new_df %>% filter(year == max(df$year))
  new_df <- new_df %>% group_by(state) %>% summarise(prop = black_jail_pop/white_jail_pop)
  return(new_df)
}

get_menVwomen_incarceration_pop <- function()
{
  new_df <- df %>% group_by(year) %>% filter(male_jail_pop != "NA") %>% summarise(male_jail_pop = sum(male_jail_pop))
  new_df2 <- df %>% group_by(year) %>% filter(female_jail_pop != "NA") %>% summarise(female_jail_pop = sum(female_jail_pop))
  new_df <- left_join(new_df, new_df2)
  new_df <- new_df %>% group_by(year) %>% summarise(prop = female_jail_pop/male_jail_pop)
  new_df <- new_df %>% filter(prop != "NaN")
  return(new_df)
}

# cvs_download <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")
# write.csv(cvs_download, "/Users/devdhawan/Documents/info201/data/jail.cvs", row.names=FALSE)

#----------------------------------------------------------------------------#

## Section 3  ---- 
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population
# Your functions might go here ... <todo:  update comment>
#----------------------------------------------------------------------------#
# This function ... <todo:  update comment>
get_year_jail_pop <- function() {
  return_df <- df %>% group_by(year) %>% filter(total_jail_pop != "NA") %>% summarise(total_jail_pop = sum(total_jail_pop))
return(return_df)  
}

# This function ... <todo:  update comment>
plot_jail_pop_for_us <- function()  {
  return(ggplot(get_year_jail_pop(), aes(x = year, y = total_jail_pop)) + geom_bar(stat = "identity", position = "dodge") + scale_y_continuous(labels = label_comma()) + labs(title = "Jail Population Distribution in the US from 1970 to 2018", x = "Year", y = "Total jail population", caption = "The distribution of total Jail Population in the US from 1970 to 2018"))
}

## Section 4  ---- 
#----------------------------------------------------------------------------#
# Growth of Prison Population by State 

get_jail_pop_by_states <- function(states) {
  new_df <- df %>% filter(state == states)
  return(new_df)
}

plot_jail_pop_by_states <- function(states) {
  len <- length(states)
  if (len < 3)
  {
    return("Not enough states")
  }
  if (len > 10)
  {
    return("Too many States")
  }
  return(ggplot(get_jail_pop_by_states(states), aes(x = year, y = total_jail_pop, colour = state)) + geom_point(size = 0.8, alpha = 0.09)+ geom_smooth(size = 2) + theme_minimal() + labs(title = "Jail Population Distribution in the US from 1970 to 2018", x = "Year", y = "Total jail population", caption = "Comparing jailed population of certain states"))
}

# See Canvas
#----------------------------------------------------------------------------#

## Section 5  ---- 
#----------------------------------------------------------------------------#
# <variable comparison that reveals potential patterns of inequality>
# Your functions might go here ... <todo:  update comment>

get_male_pop_by_race <- function(states)
{
  df_cur <- df %>% filter(year == max(df$year))
  new_df1 <- df_cur %>% group_by(state) %>% filter(black_jail_pop != "NA") %>% summarise(black_jail_pop = sum(black_jail_pop))
  new_df2 <- df_cur %>% group_by(state) %>% filter(white_jail_pop != "NA") %>% summarise(white_jail_pop = sum(white_jail_pop))
  new_df3 <- df_cur %>% group_by(state) %>% filter(latinx_jail_pop != "NA") %>% summarise(latinx_jail_pop = sum(latinx_jail_pop))
  new_df4 <- df_cur %>% group_by(state) %>% filter(native_jail_pop != "NA") %>% summarise(native_jail_pop = sum(native_jail_pop))
  new_df5 <- df_cur %>% group_by(state) %>% filter(other_race_prison_pop != "NA") %>% summarise(other_race_prison_pop = sum(other_race_prison_pop))
  new_df <- left_join(new_df1, new_df2)
  new_df <- left_join(new_df, new_df3)
  new_df <- left_join(new_df, new_df4)
  new_df <- left_join(new_df, new_df5)
  new_df <- new_df %>% filter(state == states)
  new_df <- new_df %>% select(black_jail_pop, white_jail_pop, latinx_jail_pop, native_jail_pop)
  return(new_df)
}

plot_male_pop_by_race <- function(states)
{
  x1 <- get_male_pop_by_race(states)
  x1 <- as.numeric(x1[1, ])
  title <- paste("Largest Jailed Population from", states, "During 2018", sep = " ")
  return(barplot(x1, xlab = "Race", ylab = "Jailed population", names.arg = c("Black", "Latinx", "Native", "White"),  main = title))
}

caption <- "Largest jailed population of each race based on state."
# See Canvas
#----------------------------------------------------------------------------#

## Section 6  ---- 
#----------------------------------------------------------------------------#
# <a map shows potential patterns of inequality that vary geographically>

get_pop_of_africanamerican <- function()
{
  new_df <- df %>% filter(year == max(df$year))
  new_df <- new_df %>% select(state, county_name, black_jail_pop)
  new_df <- na.omit(new_df)
  uniq_states <- unique(df %>% select(state))
  vec <- 1:nrow(uniq_states)
  for (x in 1:nrow(uniq_states))
  {
    vec[x] <- sum(new_df[which(new_df$state == uniq_states[x, 1]), 3])
  }
  return(vec)
}

get_uniq_states <- function()
{
  ust <- unique(df %>% select(state))
  ust <- as.vector(ust$state)
  return(ust)
}

create_dataframe_for_map <- function()
{
  ust <- get_uniq_states()
  vec <- get_pop_of_africanamerican()
  return(data.frame(black_jail_pop = vec, state = ust))
}

plot_pop_of_africanamerican <- function()
{
  new_df <- create_dataframe_for_map()
  return(plot_usmap(regions = "states", data = new_df, values = "black_jail_pop", color = "red") + 
           scale_fill_continuous(
             low = "white", high = "red", name = "African American Jailed Population (2018)", label = scales::comma
           ) + theme(legend.position = "right") + labs(caption = "African American Jailed population in each state."))
}

# See Canvas
#----------------------------------------------------------------------------#

## Load data frame ---- 


