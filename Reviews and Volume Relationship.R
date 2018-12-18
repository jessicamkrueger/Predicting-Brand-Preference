Data2kNN <- mutate(Data2kNN, Total_Reviews = (X5Stars + X4Stars + X3Stars +
                                                X2Stars + X1Stars))

relations <- select(Data2kNN, "Product_type", "Total_Reviews", "Volume", "X5Stars", "X4Stars", 
                    "X3Stars", "X2Stars", "X1Stars", "Positive_service_review",
                    "Negative_service_review")
ids <- c(1:186)

relations$ID <- ids

relation600 <- filter(relationVol, Volume < 657, Volume > 626)
relationVol$ID <- as.factor(relationVol$ID)

ggplot(relation600, aes(x=ID)) + 
  geom_point(aes(y = X1Stars, color ="X1Star"), size = 3) +
  geom_point(aes(y = X2Stars, color ="X2Stars"), size = 3) +
  geom_point(aes(y = Volume, color = "Volume"), size = 3) +
  geom_point(aes(y = X5Stars, color = "X5Stars"), size = 3) +
  geom_point(aes(y = X4Stars, color = "X4Stars"), size = 3) +
  geom_point(aes(y=Total_Reviews, color = "Total_Reviews"), size = 3) +
  scale_y_sqrt() +
  scale_colour_manual(name="Dot Color",
                      values=c(X1Star="blue", X2Stars="light blue", 
                               Volume="red", X5Stars="dark green",
                               X4Stars = "light green", Total_Reviews =
                                 "orange")) +
  ylab("Value") +
  xlab("Product Number")

relationVol <- arrange(relations, desc(Volume))

VolOver1000 <- filter(relationVol, Volume > 1000)
ggplot(VolOver1000, aes(x=ID)) + 
  geom_point(aes(y = X1Stars, color ="X1Star"), size = 3) +
  geom_point(aes(y = X2Stars, color ="X2Stars"), size = 3) +
  geom_point(aes(y = Volume, color = "Volume"), size = 3) +
  geom_point(aes(y = X5Stars, color = "X5Stars"), size = 3) +
  geom_point(aes(y = X4Stars, color = "X4Stars"), size = 3) +
  geom_point(aes(y=Total_Reviews, color = "Total_Reviews"), size = 3) +
  scale_y_sqrt() +
  scale_colour_manual(name="Dot Color",
                      values=c(X1Star="blue", X2Stars="light blue", 
                               Volume="red", X5Stars="dark green",
                               X4Stars = "light green", Total_Reviews =
                                 "orange")) +
  ylab("Value") +
  xlab("Product Number")

Vol30to40 <- filter(relationVol, Volume > 29, Volume < 41)
ggplot(Vol30to40, aes(x=ID)) + 
  geom_point(aes(y = X1Stars, color ="X1Star"), size = 3) +
  geom_point(aes(y = X2Stars, color ="X2Stars"), size = 3) +
  geom_point(aes(y = Volume, color = "Volume"), size = 3) +
  geom_point(aes(y = X5Stars, color = "X5Stars"), size = 3) +
  geom_point(aes(y = X4Stars, color = "X4Stars"), size = 3) +
  geom_point(aes(y=Total_Reviews, color = "Total_Reviews"), size = 3) +
  scale_colour_manual(name="Dot Color",
                      values=c(X1Star="blue", X2Stars="light blue", 
                               Volume="red", X5Stars="dark green",
                               X4Stars = "light green", Total_Reviews =
                                 "orange")) +
  ylab("Value") +
  xlab("Product Number")

X1andX2Stars <- mutate(relations, X1X2Stars = X1Stars + X2Stars)
median(X1andX2Stars$X1X2Stars)
mean(X1andX2Stars$X1X2Stars)

relations$ID <- as.factor(relations$ID)
High1Stars <- filter(X1andX2Stars, X1X2Stars > 50)
ggplot(High1Stars, aes(x=ID)) + 
  geom_point(aes(y = X1X2Stars, color ="X1X2Stars"), size = 3) +
  geom_point(aes(y = Volume, color = "Volume"), size = 3) +
  geom_point(aes(y = X5Stars, color = "X5Stars"), size = 3) +
  geom_point(aes(y = X4Stars, color = "X4Stars"), size = 3) +
  geom_point(aes(y=Total_Reviews, color = "Total_Reviews"), size = 3) +
  scale_colour_manual(name="Dot Color",
                      values=c(X1X2Stars="blue", X2Stars="light blue", 
                               Volume="red", X5Stars="dark green",
                               X4Stars = "light green", Total_Reviews =
                                 "orange")) +
  ylab("Value") +
  xlab("Product Number")


#plot total star ratings against volume
relations1 <- mutate(relationVol, Star_Rating = (X5Stars*5 + X4Stars*4 + X3Stars*3 +
                                                 X2Stars*2 + X1Stars*1)/Total_Reviews)

#line chart of star rating vs. volume by product type
ggplot(relations1, aes(x=Star_Rating, y=Volume)) +
  geom_line() +
  scale_x_continuous(breaks = seq(0, 5, .5)) +
  facet_wrap(~Product_type, scales = "free_y")
#appears to be more of a pattern for some product types over others
#smartphones and accessories show the pattern the most. Pattern is
#all over the place for other categories

#line chart of star rating vs. volume overall
ggplot(relations1, aes(x=Star_Rating, y=Volume)) +
  geom_line() +
  scale_x_continuous(breaks = seq(0, 5, .5))
#doesn't show clear pattern. We'd expect as star rating goes up, so does
#volume, but that's not really the case.
  
#create positive service review score
relations1 <- mutate(relations1, PosServScore = (Positive_service_review/Volume))

#create negative service review score
relations1 <- mutate(relations1, NegServScore = (Negative_service_review/Volume))
#plotting of these scores against volume wans't helpful

#create ratio of positive reviews to negative reviews
relations1 <- mutate(relations1, PosNegRatio = Positive_service_review/Negative_service_review)
#plot ratio to volume
ggplot(relations1, aes(x=PosNegRatio, y=Volume)) +
  geom_line() +
  scale_x_log10() +
  geom_smooth(method = "auto")
#plot shows no clear relationship between ratio of pos to neg reviews
#and volume. Anything under 1.0 means that there were more neg. reviews
#than positive reviews, but the volume values are still quite high.
#By dividing positive reviews by negative reviews, we get the ratio.