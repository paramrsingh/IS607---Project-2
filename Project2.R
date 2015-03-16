# Param Singh - IS607 - Project 2 
require(ggplot2)
require(grid)
require(gridExtra)
# Data setup - I entered the data manually to prevent issues when the code was rerun or executed on rpubs, for larger data sets I would have loaded into a data frame from a csv file 
df1 <- data.frame( x = c(10.00, 8.00, 13.00, 9.00, 11.00, 14.00, 6.00, 4.00, 12.00, 7.00, 5.00), 
                   y = c(8.04, 6.95, 7.58, 8.81, 8.33, 9.96, 7.24, 4.26, 10.84, 4.82, 5.68) )

df2 <- data.frame( x = c(10.00, 8.00, 13.00, 9.00, 11.00, 14.00, 6.00, 4.00, 12.00, 7.00, 5.00), 
                   y = c(9.14, 8.14, 8.74, 8.77, 9.26, 8.10, 6.13, 3.10, 9.13, 7.26, 4.74) )

df3 <- data.frame( x = c(10.00, 8.00, 13.00, 9.00, 11.00, 14.00, 6.00, 4.00, 12.00, 7.00, 5.00), 
                   y = c(7.46, 6.77, 12.74, 7.11, 7.81, 8.84, 6.08, 5.39, 8.15, 6.42, 5.73) )

df4 <- data.frame( x = c(8.00, 8.00, 8.00, 8.00, 8.00, 8.00, 8.00, 19.00, 8.00, 8.00, 8.00), 
                   y = c(6.58, 5.76, 7.71, 8.84, 8.47, 7.04, 5.25, 12.50, 5.56, 7.91, 6.89) )

agg_df2 <- rbind(df1, df2, df3, df4)
agg_df2$grouping[1:11] <-  1
agg_df2$grouping[12:22] <-  2
agg_df2$grouping[23:33] <-  3
agg_df2$grouping[34:44] <-  4

p1 <- ggplot(df1, aes(x=x, y=y)) + geom_point(color='blue', size=3) + labs(title = '1')
p2 <- ggplot(df2, aes(x=x, y=y)) + geom_point(color ='green4', size=3, pch=17) + labs(title = '2')
p3 <- ggplot(df3, aes(x=x, y=y)) + geom_point(color='red', size=3, pch=23) + labs(title = '3')
p4 <- ggplot(df4, aes(x=x, y=y)) + geom_point() + labs(title = '4')
grid.arrange(p1, p2, p3, p4, ncol=2)

ggplot(agg_df2, aes(x=x, y=y)) + geom_point() + facet_wrap(~grouping)

#individual analysis
summary(df1)
lm(data=df1)
b1 <- ggplot(df1, aes(x=x, y=y)) + geom_boxplot(outlier.color = NULL, outlier.size=4) + labs(title = '1')

#Using 'grid' was keyto laying out different plot types next to each other
grid.arrange(p1 + geom_smooth(method="lm"), b1, ncol=2, main="First Group")

summary(df2)
lm(data=df2)
b2 <- ggplot(df2, aes(x=x, y=y)) + geom_boxplot(outlier.color = NULL, outlier.size=4) + labs(title = '2')
grid.arrange(p2 + geom_smooth(method="lm"), b2, ncol=2, main="Second Group")

# Special note -- Shapiro-Wilk tests for normality are not definitive 
# http://en.wikipedia.org/wiki/Shapiro–Wilk_test
shapiro.test(df2$x)
shapiro.test(df2$y)
# It's recommended to include a QQ plot with a Shapiro-Wilk
qqnorm(df2$y);qqline(df2$y, col = 2)
qqnorm(df2$x);qqline(df2$x, col = 2)

summary(df3)
lm(data=df3)
b3 <- ggplot(df3, aes(x=x, y=y)) + geom_boxplot(outlier.color = NULL, outlier.size=4) + labs(title = '3')
grid.arrange(p3 + geom_smooth(method="lm"), b3, ncol=2, main="Third Group")

summary(df4)
table(df4)
ggplot(df4, aes(x=x, y=y)) + geom_boxplot(outlier.color = NULL, outlier.size=4) + labs(title = '4')
#flipped scatter plot
ggplot(df4, aes(x=y, y=x)) + geom_point()

ggplot(df4, aes(x=x)) + geom_histogram(binwidth=.5)
chisq.test(df4$x)
ks.test(df4$x, "punif")




