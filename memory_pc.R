library(tidyverse)

memory_pc_df <- read.csv(file = "Memory_exps_PC.csv")
theme_set(theme_bw())

memory_pc <- memory_pc_df %>% ggplot(aes(x=nlevels, y=mean, group=paste0(Task,", ",round(delta,2)), color = paste0(Task,", ",round(delta,2)))) + 
    geom_errorbar(aes(ymin=lower, ymax=upper), width=.1, 
                  position=position_dodge(0.05)) +
    geom_line() +
    geom_point() +
    labs(title="Achieved powers at certain numbers of participants",
         x="Number of participants", y = "Power",
         color = "Task, Effect Size",
         line = "Task, Effect Size") +
    geom_hline(yintercept = 0.8) + 
    scale_color_brewer(type = 'div', palette="Spectral") + 
    theme(legend.title = element_text(size=12),
          legend.justification=c(1,0), 
          legend.position=c(0.95, 0.05),  
          legend.background = element_blank(),
          legend.key = element_blank())

