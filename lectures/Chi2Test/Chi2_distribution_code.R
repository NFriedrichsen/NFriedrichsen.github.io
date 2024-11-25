library(dplyr)
library(xtable)
library(gridExtra)
library(ggpubr)
library(data.table)
library(ggplot2)
theme_set(theme_bw(base_size = 18))

gh <- function(bins = 8) {
  geom_histogram(color = "black", fill = "gray80", bins = bins)
}

## p-values
createShadeRegion <- function(x, df, ub = 15) {
  x1 <- c(x, seq(x, ub, by = 0.01), ub)
  n <- length(x1) - 1
  y1 <- c(0, dchisq(x1, df)[2:n], 0)
  df1 <- data.frame(x = x1, y = y1)
}

pv <- createShadeRegion(3, 4)

x <- seq(0, 15, by = 0.01)
x4 <- dchisq(x, 4)
df <- data.frame(x, y = x4)

1 - pchisq(3.2, df = 4)


ggplot(df, aes(x, y)) + geom_line(linewidth = 1.2) +
  geom_polygon(data = pv, fill = "hotpink", alpha = 0.6) + 
  geom_segment(aes(x = 10, y = 0.17, xend = 3.2, yend = 0), 
               arrow = arrow(length = unit(0.5, "cm"))) + 
  annotate("text", x = 10, y = 0.18, 
           label = expression(chi^2 == 3.2), parse = TRUE, size = 8) +
  annotate("text", x = 12, y = 0.1, label = "p-value = 0.53", size = 8) + 
  ggtitle("Chi-squared distribution with df = 4")  + 
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), 
        axis.title.y = element_blank()) + 
  xlab(expression(chi^2))

# simulated tables
tab <- rmultinom(10, 25, rep(1/5, 5)) %>% t()
colnames(tab) <- LETTERS[1:5]
rownames(tab) <- paste0("Sample ", 1:10)

xtable(tab, digits=0)

chi <- sapply(split(tab, row(tab)), function(x) chisq.test(x)[["statistic"]])

xtable(cbind(tab, chi), digits=0)



### Example in Class
obs <- c(780, 117, 114, 384, 58)
percent <- c(.54,.18,.12,.15,.01)
exp <- 1453*percent
exp
((obs - exp)^2 / exp) %>% sum()

# P-value
pchisq(357.3625, df=4, lower.tail = F)

