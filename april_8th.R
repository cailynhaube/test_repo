library(tidyverse)
library(babynames)

valid_levels <- c("beginner", "intermediate", "advanced")

students <- read_csv("Data/student_info.csv", show_col_types = FALSE) |>
  rename(
    name               = `Name`,
    year_of_study      = `year of study`,
    major              = `major`,
    experience_level   = `level of programming experience (e.g. beginner, intermediate, advanced)`,
    prog_languages     = `programming languages (R, Python, Julia, etc.)`,
    research_interests = `research / academic interests`,
    group              = `Group number / name`,
    attended_jan30     = `attendance jan 30th`
  )

##############################
# what is the distribution of research interests in the whole class?
##############################

students_interests <- students |>
  select(name, research_interests) |>
  separate_longer_delim(research_interests, regex("[;/]"))


students_interests |> mutate(research_interests = str_to_lower(str_trim(research_interests))) |> 
  ggplot(aes(research_interests))+
  geom_bar()+
  theme(axis.text = element_text(angle= 45, vjust =0.5, hjust=1))

##############################
# what is the average level of programming experience in each group?
##############################

valid_levels <- c("beginner", "intermediate", "advanced")

students |>
  mutate(
    experience_level = str_to_lower(str_trim(experience_level)),
    experience_level = factor(experience_level, levels = valid_levels, ordered = TRUE),
  ) |>
  group_by(group) |>
  ggplot(aes(x=group,y=as.numeric(experience_level))) + 
  geom_boxplot()

##############################
# How many people in each group know R, python, etc.?
##############################

students_languages <- students |>
  select(name, group, prog_languages) |>
separate_longer_delim(prog_languages, regex("[, ]"))

students_languages <- students_languages |>
  mutate(
    prog_languages = str_to_lower(str_trim(prog_languages)))
    

students_languages |> 
  ggplot(aes(y = prog_languages)) + 
  geom_bar() + 
  # Use facet_grid to group the y-axis by 'group'
  facet_grid(group ~ ., scales = "free_x", space = "free_y")

##############################
# Do undergraduates have more or less programming experience than graduate students?
##############################

students_languages_2 <- students |>
  select(name, year_of_study, prog_languages)|>
  separate_longer_delim(prog_languages, regex("[, ]"))

students_languages_2$year_of_study <- ifelse(students_languages_2$year_of_study %in% c("senior", "Senior", "Junior"), "undergrad", "grad")

students_languages_2 <- students_languages_2 |>
  mutate(
    prog_languages = str_to_lower(str_trim(prog_languages)))

students_languages_2 |> 
  ggplot(aes(y = prog_languages)) + 
  geom_bar() + 
  # Use facet_grid to group the y-axis by 'group'
  facet_grid(year_of_study ~ ., scales = "free_x", space = "free_y")

