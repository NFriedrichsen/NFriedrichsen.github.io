
library(ggplot2)
library(dplyr)
library(xtable)
library(alr4)
library(visreg)

theme_set(theme_bw(base_size = 18))

college <- read.csv("https://collinn.github.io/data/college2019.csv")
college <- data.table(college)

mpg$cyl <- factor(mpg$cyl)
mpg$trans <- gsub("\\(..\\)", "", mpg$trans)

png("reg_reg1.png", 600, 400)
ggplot(mpg, aes(hwy, cty)) + 
  geom_point(color = 'gray30', position = position_jitter(seed = 69))
dev.off()

png("reg_reg2.png", 600, 400)
ggplot(mpg, aes(hwy, cty)) + 
  geom_point(color = 'gray30', position = position_jitter(seed = 69)) +
  geom_smooth(method = lm, color = 'tomato', se = FALSE)
dev.off()

ggplot(college, aes(Cost, Type)) +
  geom_boxplot(fill = 'gray')



lm(cty ~ hwy, mpg) %>% coef()

mm <- group_by(mpg, drv) %>% 
  summarize(cc = mean(cty))

png("cat_reg1.png", 600, 400)
ggplot(mpg, aes(drv, cty, group = drv)) + 
  geom_point(color = 'gray30', position = position_jitter(width = 0.15, seed = 69))
dev.off()

png("cat_reg2.png", 600, 400)
ggplot(mpg, aes(drv, cty, group = drv)) + 
  geom_point(color = 'gray30', position = position_jitter(width = 0.15, seed = 69)) +
  geom_segment(aes(x = 0.7, xend = 1.3, y = 14.3, yend=14.3), 
               color = 'tomato', linewidth = 1.5) +
  geom_segment(aes(x = 1.7, xend = 2.3, y = 20, yend=20), 
               color = 'tomato', linewidth = 1.5) +
  geom_segment(aes(x = 2.7, xend = 3.3, y = 14.1, yend=14.1), 
               color = 'tomato', linewidth = 1.5)
dev.off()
  

## Indicator vars
# pp <- group_by(mpg, manufacturer, model, trans) %>% 
#   summarize(cty = mean(cty)) %>% data.table() %>% 
#   unique(mpg, by = c("manufacturer", "model"))[, .(manufacturer, model, trans)][
#   , model := paste(manufacturer, model)
# ]

pp <- group_by(mpg, manufacturer, model, trans) %>% 
  summarize(cty = mean(cty)) %>% data.table()
pp[, model := paste(manufacturer, model)]
pp[, manufacturer := NULL]

tt <- model.matrix(model ~ trans, pp)
tt <- as.data.table(tt)
tt[, `:=`(Model = pp$model, Manual = transmanual, 
          Automatic = 1 - transmanual)]
tt <- tt[, .(Model, Manual, Automatic)]

names(pp) <- c("Model", "Transmission", "Cty")

pp[c(1, 2, 6, 12, 19, 26), c("Model", "Transmission")] %>% xtable() %>% print(include.rownames = FALSE)
tt[c(1, 2, 6, 12, 19, 26),] %>% xtable(digits = 0) %>% print(include.rownames = FALSE)


tt$cty <- pp$Cty
tt[c(1, 2, 6, 12, 19, 26),] %>% xtable(digits = c(0, 0, 0, 0, 3)) %>% print(include.rownames = FALSE)


png("trans_reg.png", 300, 300)
ggplot(pp, aes(Transmission, Cty)) + 
  geom_point(color = 'gray30', 
             position = position_jitter(width = 0.15, seed = 69)) +
  geom_segment(aes(x = 0.7, xend = 1.3, y = 16.370, yend=16.370), 
               color = 'tomato', linewidth = 1.5) +
  geom_segment(aes(x = 1.7, xend = 2.3, y = 18.457, yend=18.457), 
               color = 'tomato', linewidth = 1.5) +
  labs(y = "City MPG")
dev.off()

pp[,  mean(Cty), by = Transmission] %>% 
  xtable(digits = 3) %>% print(include.rownames = FALSE)

group_by(pp, Transmission) %>% 
  summarize(mean(Cty)) %>% 
  print(include.rownames = FALSE)

qq <- copy(pp)
names(qq) <- c("model", "trans", "cty2", "cty")
lm(cty ~ trans, qq)

lm(cty ~ drv, mpg)

# "#E41A1C" "#377EB8"
set.seed(69)
rr <- mpg[sample(1:nrow(mpg), 7), c("model", "cty", "drv")] %>% data.frame()


tt <- model.matrix(cty ~ drv, rr) %>% as.data.table()
rr <- as.data.table(rr)
xtable(rr) %>% print(include.rownames = FALSE)

tt[, drv4 := 1 - drvf - drvr]
tt[["(Intercept)"]] <- NULL

tt[, `:=`(model = rr$model, cty = rr$cty)]
tt <- tt[, c(4,5,1,2,3)]
tt %>% xtable(digits = 0) %>% print(include.rownames = FALSE)


mpg$trans <- gsub("\\(..\\)", "", mpg$trans)

group_by(mpg, trans, drv) %>% 
  summarize(mean(cty)) %>% 
  data.frame()

fit <- lm(cty ~ drv + trans, mpg)
df <- expand.grid(drv = unique(mpg$drv), 
                  trans = unique(mpg$trans))

df$pred <- predict(fit, df)


png("double_cat_pred1.png", 400, 400)
ggplot(mpg, aes(drv, y = cty, color = trans)) + 
  geom_jitter(width = 0.15, height = .15, alpha = 0.4)+ 
  scale_color_manual(values = c("tomato", "steelblue")) +
  theme(legend.position = "bottom")
dev.off()

png("double_cat_pred.png", 600, 400)
ggplot(mpg, aes(drv, y = cty, color = trans)) + 
  geom_jitter(width = 0.15, height = .15, alpha = 0.4) +
  geom_segment(aes(x = 0.7, xend = 1.3, y = 13.769, yend=13.769), 
               color = 'tomato', linewidth = 1.5) +
  geom_segment(aes(x = 1.7, xend = 2.3, y = 19.173, yend=19.173), 
               color = 'tomato', linewidth = 1.5) +
  geom_segment(aes(x = 2.7, xend = 3.3, y = 13.419, yend=13.419), 
               color = 'tomato', linewidth = 1.5) +
  geom_segment(aes(x = 0.7, xend = 1.3, y = 15.834, yend=15.834), 
               color = 'steelblue', linewidth = 1.5) +
  geom_segment(aes(x = 1.7, xend = 2.3, y = 21.238, yend=21.238), 
               color = 'steelblue', linewidth = 1.5) +
  geom_segment(aes(x = 2.7, xend = 3.3, y = 15.485, yend=15.485), 
               color = 'steelblue', linewidth = 1.5) + 
  scale_color_manual(values = c("tomato", "steelblue"))
dev.off()



