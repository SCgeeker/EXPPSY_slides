## Load packages to retrieve data
options(java.parameters = "-Xmx8000m")
library(tidyverse)
library(xlsx)
## Load packages for mixed-effect model
library(lme4)
library(simr)

## Access the directory of data sets
csv_path <- "D:/TCU/Lecture/Experimental_Psychology/9RPs/Data Files/"


## Get the xlsx of Shape Simulation
MeansShape <- csv_path %>%
    paste0(csv_path %>% list.files(pattern="^MeansShape")) %>%
    read.csv()

## Check the mean RT by participant
(MeansShape %>% group_by(similarity) %>%
    summarise(s1_match = mean(session1_match %>% as.character() %>% as.numeric(), na.rm = TRUE),
              s1_nomatch = mean(session1_nonmatch %>% as.character() %>% as.numeric(), na.rm = TRUE),
              s2_match = mean(session2_match %>% as.character() %>% as.numeric(), na.rm = TRUE),
              s2_nomatch = mean(session2_nonmatch %>% as.character() %>% as.numeric(), na.rm = TRUE)) %>%
    gather(key = "conditions", value = "Mean", 2:5) ) %>%
right_join( MeansShape %>% group_by(similarity) %>%
  summarise(s1_match = sd(session1_match %>% as.character() %>% as.numeric(), na.rm = TRUE)/sqrt(n()),
            s1_nomatch = sd(session1_nonmatch %>% as.character() %>% as.numeric(), na.rm = TRUE)/sqrt(n()),
            s2_match = sd(session2_match %>% as.character() %>% as.numeric(), na.rm = TRUE)/sqrt(n()),
            s2_nomatch = sd(session2_nonmatch %>% as.character() %>% as.numeric(), na.rm = TRUE)/sqrt(n())) %>%
  gather(key = "conditions", value = "SE", 2:5), by=c("similarity","conditions"), copy = TRUE ) %>%
  separate(conditions, c("session","matching"), sep = "_") %>%
  arrange(desc(similarity)) %>%
ggplot(aes(x=matching, y = Mean, color = matching))  + 
  geom_bar(stat = "identity", fill = "white") +
  geom_errorbar(aes(ymin=Mean-SE, ymax=Mean+SE), width=.2,
                position=position_dodge(.9)) +
  facet_grid(vars(similarity), vars(session)) +
  coord_cartesian(ylim = c(600, 1000)) 
  