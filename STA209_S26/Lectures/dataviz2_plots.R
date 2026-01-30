library(dplyr)
library(ggplot2)
colleges = read.csv("https://remiller1450.github.io/data/Colleges2019_Complete.csv")

colleges %>% ggplot(aes(x = Adm_Rate, y = Net_Tuition)) +
                      geom_point()

# color, symbols                    
colleges %>% ggplot(aes(x = Adm_Rate, y = Net_Tuition,
                        color = Private, shape = Private)) +
  geom_point(size=1, stroke=1.1) + 
  scale_shape_manual(values = c(3, 4))

# grayscale, symbols
colleges %>% ggplot(aes(x = Adm_Rate, y = Net_Tuition,
                        color = Private, shape = Private)) +
  geom_point(size=1, stroke=1.1) + 
  scale_shape_manual(values = c(3, 4)) + 
  scale_color_grey(start = 0.2, end = 0.8) +
  theme_bw()

# plains, symbols, grayscale
colleges %>% filter(Region == "Plains") %>% 
  ggplot(aes(x = Adm_Rate, y = Net_Tuition,
             shape = Private,
             size = Enrollment)) +
  geom_point() + 
  scale_shape_manual(values = c(16, 1)) + 
  theme_bw()
