library(dplyr)
library(reshape2)
library(xtable)
library(ggplot2)

theme_set(theme_bw())

tail(tips, n = 20) %>% xtable() %>% print(include.rownames = FALSE)

ggplot(tips, aes(sex)) + geom_bar()

png("tip_day.png")
ggplot(tips, aes(day, fill = day)) + geom_bar() 
dev.off()

png("bill_bin_20.png")
ggplot(tips, aes(total_bill)) + geom_histogram(bins = 20, color = "black", fill = "gray")
dev.off()


p1 <- ggplot(tips, aes(total_bill)) + geom_histogram(bins = 5, color = "black", fill = "gray") + ggtitle("5 Bins")
p2 <- ggplot(tips, aes(total_bill)) + geom_histogram(bins = 10, color = "black", fill = "gray") + ggtitle("10 Bins")
p3 <- ggplot(tips, aes(total_bill)) + geom_histogram(bins = 25, color = "black", fill = "gray") + ggtitle("25 Bins")
p4 <- ggplot(tips, aes(total_bill)) + geom_histogram(bins = 50, color = "black", fill = "gray") + ggtitle("50 Bins")

png("bin_grid.png", width = 500, height = 350)
gridExtra::grid.arrange(p1, p2, p3, p4, nrow = 2)
dev.off()

## bivariate

png("bar_stacked.png")
ggplot(tips, aes(day, fill = smoker)) + geom_bar()
dev.off()

png("bar_dodge.png")
ggplot(tips, aes(day, fill = smoker)) + geom_bar(position = "dodge")
#ggplot(tips, aes(smoker, fill = day)) + geom_bar(position = "dodge")
dev.off()

png("bar_fill.png")
ggplot(tips, aes(day, fill = smoker)) + geom_bar(position = "fill")
#ggplot(tips, aes(smoker, fill = day)) + geom_bar(position = "dodge")
dev.off()

# numeric bivariate

png("scatter_tip.png")
ggplot(tips, aes(total_bill, tip)) + geom_point()
dev.off()





x <- rnorm(100)
y1 <- 2*x + rnorm(100, sd = 0.5)
y2 <- -2*x + rnorm(100, sd = 0.5)
df1 <- data.frame(x, y1)
ggplot(df1, aes(x, y1)) + geom_point()
df2 <- data.frame(x, y2)
ggplot(df2, aes(x, y2)) + geom_point()


y3 <- -2*x^2 + rnorm(100)
df3 <- data.frame(x, y3)
ggplot(df3, aes(x, y3)) + geom_point()

y4 <- rnorm(100)
df4 <- data.frame(x, y4)
ggplot(df4, aes(x, y4)) + geom_point()


# Strength
y5 <- 2*x + rnorm(100, sd = 0.1)
y6 <- 2*x + rnorm(100, sd = 0.5)
y7 <- 2*x + rnorm(100, sd = 2)

df5 <- data.frame(x, y5)
df6 <- data.frame(x, y6)
df7 <- data.frame(x, y7)

p1 <- ggplot(df5, aes(x, y5)) + geom_point()
p2 <- ggplot(df6, aes(x, y6)) + geom_point()
p3 <- ggplot(df7, aes(x, y7)) + geom_point()

png("linear_strong.png", width = 550, height = 350)
gridExtra::grid.arrange(p1, p2, p3, nrow = 1)
dev.off()



y3 <- -2*x^2 + rnorm(100)
df3 <- data.frame(x, y3)
p1 <- ggplot(df3, aes(x, y3)) + geom_point()
x <- rnorm(100)
y1 <- 2*x + rnorm(100, sd = 0.5)
df1 <- data.frame(x, y1)
p2 <- ggplot(df1, aes(x, y1)) + geom_point()

png("linear_nonlinear.png", width = 550, height = 350)
gridExtra::grid.arrange(p1, p2, nrow = 1)
dev.off()



y1 <- 2*x + rnorm(100, sd = 0.5)
df1 <- data.frame(x, y1)
y2 <- rnorm(100)
df2 <- data.frame(x, y2)
p1 <- ggplot(df1, aes(x, y1))+geom_point()
p2 <- ggplot(df2, aes(x, y2))+geom_point()

png("linear_none.png", width = 550, height = 350)
gridExtra::grid.arrange(p1, p2, nrow = 1)
dev.off()

