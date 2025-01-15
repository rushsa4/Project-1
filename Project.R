#Loading the data
My_PCA_Data<- read.csv("Project Assignment_Rushsa.csv")[1:160,1:12]
#Missing value
colSums(is.na(My_PCA_Data))
#Impute missing value by mean
My_PCA_Data$AnnualIncomefromothersources[is.na(My_PCA_Data$AnnualIncomefromothersources)] <- 
  median(My_PCA_Data$AnnualIncomefromothersources, na.rm = TRUE)
#Check outlier
boxplot(My_PCA_Data$AnnualIncomefromothersources, 
        main = "Boxplot of Annual Income from Other Sources", 
        ylab = "Annual Income from other source")

# Calculate lower and upper bounds using MAD
lower_bound <- median(My_PCA_Data$AnnualIncomefromothersources, na.rm = TRUE) - 
  3 * mad(My_PCA_Data$AnnualIncomefromothersources, na.rm = TRUE)
upper_bound <- median(My_PCA_Data$AnnualIncomefromothersources, na.rm = TRUE) + 
  3 * mad(My_PCA_Data$AnnualIncomefromothersources, na.rm = TRUE)
# Identify indices of outliers
outliers <- which(My_PCA_Data$AnnualIncomefromothersources < lower_bound | 
                    My_PCA_Data$AnnualIncomefromothersources > upper_bound)
# Replace outliers with the calculated bounds
My_PCA_Data$AnnualIncomefromothersources[My_PCA_Data$AnnualIncomefromothersources < lower_bound] <- lower_bound
My_PCA_Data$AnnualIncomefromothersources[My_PCA_Data$AnnualIncomefromothersources > upper_bound] <- upper_bound
boxplot(My_PCA_Data$AnnualIncomefromothersources, 
        main = "Boxplot After Outlier Handling", 
        ylab = "Annual Income from other source")
# Perform PCA
correlation<- cor(My_PCA_Data)
mean(correlation)
eigen(correlation)
PCA_result <- prcomp(My_PCA_Data, scale. = TRUE)
summary(PCA_result)
install.packages("devtools")
library(devtools)
install_github("vqv/ggbiplot")
library(ggbiplot)
ggscreeplot(PCA_result)+aes(color="red")
ggbiplot(PCA_result)
library(psych)
bartlett.test(My_PCA_Data)
cortest.bartlett(My_PCA_Data)  # Test of sphericity

# KMO Test
KMO(My_PCA_Data)
# Perform factor analysis 
fact_result<-factanal(factors=2, covmat = cov(My_PCA_Data))
Rotation<-factanal(factors=2, covmat = cov(My_PCA_Data), rotation = "varimax")
print(fact_result)
loads<-fact_result$loadings
fa.diagram(loads)
#Plot
plot(load,type="n")
text(load,labels=names(My_PCA_Data), cex= .7)
plot(load)
saveRDS(My_PCA_Data, file = "Project.rds")
