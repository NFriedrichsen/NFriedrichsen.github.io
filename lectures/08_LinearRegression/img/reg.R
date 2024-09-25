
library(ggplot2)
library(dplyr)
library(xtable)
library(alr4)

theme_set(theme_bw(base_size = 16))
college <- read.csv("https://collinn.github.io/data/college2019.csv")
dat <- read.csv("https://collinn.github.io/data/pearson.csv")

png("father_son.png", 400, 400)
ggplot(dat, aes(Father, Son)) + 
  geom_point() +
  labs(x = "Father's Height (Inches)", 
       y = "Son's Height (Inches)") +
  geom_segment(aes(x = mean(Father), y = 57,
                   xend = mean(Father), yend = 80), color = 'red',
               linetype = "dashed") +
  geom_segment(aes(y = mean(Son), x = 57,
                   yend = mean(Son), xend = 77), color = 'red',
               linetype = "dashed")
dev.off()

png("father_son_lm.png", 500, 400)
ggplot(dat, aes(Father, Son)) + 
  geom_point() +
  labs(x = "Father's Height (Inches)", 
       y = "Son's Height (Inches)") +
  geom_segment(aes(x = mean(Father), y = 57, 
                   xend = mean(Father), yend = 80), color = 'red', 
               linetype = "dashed") +
  geom_segment(aes(y = mean(Son), x = 57, 
                   yend = mean(Son), xend = 77), color = 'red', 
               linetype = "dashed") +
  geom_smooth(method = lm, se = FALSE, linewidth = 2, mapping = aes(color = "Son ~ Father"), 
              color = "#00BFC4") 
dev.off()


fs <- lm(Father ~ Son, dat)
fs2 <- lm(Son ~ Father, dat)

m <- coef(fs)[1]
b <- coef(fs)[2]

m1 <- coef(fs2)[1]
b1 <- coef(fs2)[2]

png("father_son_lm2.png", 500, 400)
ggplot(dat, aes(Father, Son)) + 
  geom_point() +
  labs(x = "Father's Height (Inches)", 
       y = "Son's Height (Inches)") +
  geom_segment(aes(x = mean(Father), y = 57, 
                   xend = mean(Father), yend = 80), color = 'red', 
               linetype = "dashed") +
  geom_segment(aes(y = mean(Son), x = 57, 
                   yend = mean(Son), xend = 77), color = 'red', 
               linetype = "dashed") +
  #geom_smooth(method = lm, se = FALSE, linewidth = 2, mapping = aes(color = "Son ~ Father")) +
  geom_abline(mapping = aes(color = "Predict Father with Son", intercept = -m/b, slope = 1/b), 
              linewidth = 2) +
  geom_abline(mapping = aes(color = "Predict Son with Father", intercept = m1, slope = b1), 
              linewidth = 2) +
  guides(color = guide_legend(title = "Line")) + theme(legend.position = "bottom")
dev.off()

## Assessing Quality of Fit
set.seed(434)
n <- 75
x <- rnorm(n)
y1 <- -2.5*x + rnorm(n, sd = 0.5)
y2 <- -2.5*x + rnorm(n, sd = 2)

min(y1[which(x > -0.1 & x < 0.1)])
min(y2[which(x > -0.1 & x < 0.1)])
max(y1[which(x > -0.1 & x < 0.1)])
max(y2[which(x > -0.1 & x < 0.1)])

df <- data.frame(x = c(x, x), 
                 y = c(y1, y2), 
                 group = rep(c("A", "B"), each = length(x)), 
                 ax = 0, 
                 yy1 = rep(c(-0.9, -4), each = length(x)), 
                 yy2 = rep(c(1.3, 3.124), each = length(x)))

png("qof.png", 500, 400)
ggplot(df, aes(x, y)) + 
  geom_point(size = 2) +
  geom_smooth(method = lm, se = FALSE) + 
  facet_wrap(~group)
dev.off()

png("qof2.png", 500, 400)
ggplot(df, aes(x, y)) + 
  geom_point(size = 2) +
  geom_smooth(method = lm, se = FALSE) + 
  geom_segment(aes(x = ax, xend = ax, y = yy1, yend = yy2), 
               color = 'tomato', linewidth = 1.25) + 
  facet_wrap(~group)
dev.off()
