library(ggplot2)
df <- read.table("C:\\Users\\AJ Thurston\\Documents\\GitHub\\lm\\lm.txt")

mod1 <- lm(data = df, V2 ~ V1)
df$predicted <- predict(mod1)
df$residuals <- residuals(mod1)

mean(df$V1)


glm(data = df, V2 ~ V1, family = "gaussian")

summary(mod1)
influence(mod1)
coef(mod1)
effects(mod1)
mod1

p <- ggplot(df, aes(x = V1, y = V2))
p <- p + geom_segment(aes(xend = V1, yend = predicted), size = 1, color = "#C0392B", alpha = .75) 
p <- p + geom_point(aes(y = predicted), size = 3, color = "#6ACEEB")
p <- p + geom_smooth(method = "lm", se = FALSE, color = "black")  # Plot regression slope
p <- p + geom_point(color = "#336666", size = 4)
p <- p + scale_x_continuous(limits = c(-2.5,3), breaks = c(-3:3))
# p <- p + scale_y_continuous(limits = c(-3,3), breaks = c(-3:3))
p <- p + theme_bw(base_size = 18)
p <- p + theme(axis.title = element_blank())
p <- p + guides(color = FALSE)
p

ggsave(p,
       filename = "C:/Users/AJ Thurston/Desktop/lm.png",
       device = png, 
       height = 4,
       width = 6,
       units = "in",
       type = "cairo", 
       dpi = 150)


tinytex::install_prebuilt("C:/Users/AJ Thurston/Desktop/TinyTeX")
tinytex::inst