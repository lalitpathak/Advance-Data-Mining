##################################### Picking Relevant Columns ###################################################

elite_nrcSent_lm <- as.data.frame(elite_nrcSent[,c(3,14)])

nonelite_nrcSent_lm <- as.data.frame(nonelite_nrcSent[,c(3,14)])

elite_nonelite_sent <- merge(x = elite_nrcSent_lm, y = nonelite_nrcSent_lm, by = "index")

colnames(elite_nonelite_sent) <- c("Business_id", "Elite_Sent", "Nonelite_Sent")

##################################### Density PLot before Building Linear Regression Model ####################################

library(e1071)

par(mfrow = c(1,2))

plot(density(elite_nonelite_sent$Elite_Sent), main = "Density Plot: Elite Sentiments", ylab = "Frequency", sub = paste("Skewness:", round(e1071::skewness(elite_nonelite_sent$Elite_Sent),2)))

polygon(density(elite_nonelite_sent$Elite_Sent), col = "red")


plot(density(elite_nonelite_sent$Nonelite_Sent), main = "Density Plot: Non-Elite Sentiments", ylab = "Frequency", sub = paste("Skewness:", round(e1071::skewness(elite_nonelite_sent$Nonelite_Sent),2)))

polygon(density(elite_nonelite_sent$Nonelite_Sent), col = "red")

scatter.smooth(x = elite_nonelite_sent$Elite_Sent, y = elite_nonelite_sent$Nonelite_Sent, xlab = "Elite Sentiments", 
               ylab = "Non Elite Sentiments", 
               main = "Elite ~ Non Elite (Sentiments)")


cor(elite_nonelite_sent$Elite_Sent, elite_nonelite_sent$Nonelite_Sent)


linearMod <- lm(Nonelite_Sent ~ Elite_Sent, data = elite_nonelite_sent)

summary(linearMod)