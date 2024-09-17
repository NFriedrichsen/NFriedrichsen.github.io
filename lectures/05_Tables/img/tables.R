library(dplyr)
library(reshape2)
library(xtable)
library(ggplot2)
library(gridExtra)
library(ggpubr)

theme_set(theme_bw(base_size = 12))


# Get mpg dataset from ggplot2 package and modify variables
mpg <- as.data.frame(mpg)
mpg <- mutate(mpg, cyl = factor(cyl), 
              year = factor(year))

# Load majors data
majors <- read.csv("https://collinn.github.io/data/majors.csv")

# College data
college <- read.csv("https://collinn.github.io/data/college2019.csv")

## Types of plots
p1 <- ggplot(college, aes(x = Region, fill = Type)) + 
  geom_bar() +
  theme(axis.text.x = element_text(angle = 45, vjust = .5)) +
  ggtitle("Stacked")

p2 <- ggplot(college, aes(x = Region, fill = Type)) + 
  geom_bar(position = "dodge") +
  theme(axis.text.x = element_text(angle = 45, vjust = .5)) +
  ggtitle("Clustered")

p3 <- ggplot(college, aes(x = Region, fill = Type)) + 
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 45, vjust = .5)) +
  ggtitle("Conditional")

png("bar_types.png", 650, 400)
ggarrange(p1, p2, p3, nrow = 1, common.legend = TRUE, 
          legend = "bottom")
dev.off()


theme_set(theme_bw(base_size = 18))

college <- mutate(college, Size = ifelse(Enrollment < 2000, "Small", "Large"))

with(college, table(Type, Size))[, c(2,1)] %>% xtable()
with(college, table(Type, Size))[, c(2,1)]  %>% 
  addmargins() %>% xtable(digits = 0)

png("univariate_bar.png", 400, 400)
ggplot(college, aes(Type)) + 
  geom_bar(fill = 'cadetblue') + 
  ggtitle("Univariate Bar Chart")
dev.off()

png("bivariate_row.png", 450, 400)
ggplot(college, aes(x = Type, fill = Size)) + 
  geom_bar(position = "fill") + 
  ggtitle("Conditional on Row") +
  scale_fill_brewer(palette = "Accent") +
  theme(legend.position = "bottom")
dev.off()

png("bivariate_col.png", 450, 400)
ggplot(college, aes(x = reorder(Size, Size, function(x)length(x)), fill = Type)) + 
  geom_bar(position = "fill") + 
  ggtitle("Conditional on Column") +
  xlab("Size")+
  scale_fill_brewer(palette = "Paired") +
  theme(legend.position = "bottom")
dev.off()


png("bivariate_cluster.png", 450, 400)
ggplot(college, aes(Type, fill = Size)) + 
  geom_bar(position = position_dodge()) +
  ggtitle("Clustered Bar Chart") +
  scale_fill_brewer(palette = "Set1") +
  theme(legend.position = "bottom")
dev.off()

png("bivariate_stack.png", 450, 400)
ggplot(college, aes(Type, fill = Size)) + 
  geom_bar() +
  ggtitle("Stacked Bar Chart") +
  scale_fill_brewer(palette = "Set1") +
  theme(legend.position = "bottom")
dev.off()

with(college, table(Type, Large))[, c(2,1)] %>% proportions() %>% xtable(digits = 4)
with(college, table(Type, Large))[, c(2,1)] %>% proportions(margin = 1) %>% xtable(digits = 4)
with(college, table(Type, Large))[, c(2,1)] %>% proportions(margin = 2) %>% xtable(digits = 4)







table(college$Type) %>% xtable()
table(college$Type) %>% prop.table() %>% xtable(digits = 4)



df <- as.data.table(HairEyeColor)
df <- df[rep(1:nrow(df), times = df$N), ]

table(df$Hair, df$Eye) %>% xtable()

p1 <- ggplot(df, aes(Hair, fill = Eye)) + geom_bar(position = "fill") +
  scale_fill_brewer(palette = "Paired") + theme(legend.position = "bottom")
p2 <- ggplot(df, aes(Eye, fill = Hair)) + geom_bar(position = "fill")+
  scale_fill_brewer(palette = "Paired")+ theme(legend.position = "bottom")

png("conditional_bars.png", 700, 400)
gridExtra::grid.arrange(p1, p2, nrow = 1)
dev.off()

p3 <- ggplot(df, aes(Eye, fill = Hair)) + geom_bar(position = "dodge")+
  scale_fill_brewer(palette = "Paired")+ theme(legend.position = "bottom")
p4 <- ggplot(df, aes(Hair, fill = Eye)) + geom_bar(position = "dodge") +
  scale_fill_brewer(palette = "Paired")+ theme(legend.position = "bottom")

png("dodge_bars.png", 700, 400)
gridExtra::grid.arrange(p3, p4, nrow = 1)
dev.off()
