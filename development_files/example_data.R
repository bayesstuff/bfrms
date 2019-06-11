
data("fhch2010", package = "afex")
str(fhch2010)

library("tidyverse")

fhch <- as_tibble(fhch2010) %>% 
  filter(correct)

fhch %>% 
  group_by(task, stimulus) %>% 
  summarise(n())

library("afex")
afex_options("method_mixed" = "S")

m_task_1 <- mixed(rt ~ task + (1|id), fhch)
m_task_1

m_task_2 <- mixed(rt ~ task + (1|id) + (1|item), fhch)
m_task_2

m_task_3 <- mixed(rt ~ task + (1|id) + (task||item), fhch, expand_re = TRUE)
m_task_3
summary(m_task_3)

m_task_4 <- mixed(rt ~ task + (1|id) + (task|item), fhch, expand_re = TRUE)
m_task_4
summary(m_task_4)
