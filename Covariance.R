library(ggplot2)
df <- read.table("C:\\Users\\AJ Thurston\\Documents\\GitHub\\lm\\lm.txt")

mod1 <- lm(data = df, V2 ~ V1)
df <- df %>%
  mutate(dev_x = 0-V1) %>%
  mutate(dev_y = 0-V2) %>%
  mutate(dev_x_real = V1-mean(V1)) %>%
  mutate(dev_y_real = V2-mean(V2)) %>%
  mutate(product = dev_y_real*dev_x_real)

cov1 <- sum(df$product)/(dim(df)[1]-1)
cov1/(sd(df$V1)*sd(df$V2))

cor(df$V1,df$V2)


glm(data = df, V2 ~ V1, family = "gaussian")

summary(mod1)
influence(mod1)
coef(mod1)
effects(mod1)
mod1

set.seed(12345)
df2 <- df %>% sample_n(20)

p <- ggplot(df2, aes(x = V1, y = V2))
p <- p + geom_hline(yintercept = 0, size = 1, color = "#C0392B", alpha = .75) 
p <- p + geom_vline(xintercept = 0, size = 1, color = "#30394F", alpha = .75) 
p <- p + geom_segment(aes(xend = V1, yend = 0), size = 1, color = "#C0392B", alpha = .75) 
p <- p + geom_segment(aes(xend = 0, yend = V2), size = 1, color = "#30394F", alpha = .75) 
p <- p + geom_point(color = "#336666", size = 4)
p <- p + scale_x_continuous(limits = c(-3,3), breaks = c(-3:3))
p <- p + scale_y_continuous(limits = c(-3,3), breaks = c(-3:3))
p <- p + theme_bw(base_size = 18)
# p <- p + theme(axis.title = element_blank())
p <- p + labs(x = "X", y = "Y")
p <- p + guides(color = FALSE)
p

ggsave(p,
       filename = "Covariance.png",
       device = png, 
       height = 4,
       width = 6,
       units = "in",
       type = "cairo", 
       dpi = 150)
getwd()

view(df2)
