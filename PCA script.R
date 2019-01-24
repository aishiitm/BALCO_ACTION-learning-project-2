#Principal_component_AnalysiS:Feature extraction
Three_month_data<-read.csv("Threemonth_lessthan8.csv",header=TRUE)
#Feature_extract
Three_month_data<-Three_month_data[,-c(1:2)]
Pca_Three_month<-prcomp(~.,data=Three_month_data[,-1], scale=TRUE,na.action=na.omit)
plot(Pca_Three_month)
summary_Three_month<-summary(Pca_Three_month)
biplot(Pca_Three_month)
std_dev <- Pca_Three_month$sdev
pr_var <- std_dev^2
prop_varex <- pr_var/sum(pr_var)
prop_varex[1:6]
#scree plot
plot(prop_varex, xlab = "Principal Component",
       ylab = "Proportion of Variance Explained",
       type = "b")
pca_scores_Three_month<-Pca_Three_month$x
Pca_Three_month_data<-as.data.frame(Pca_Three_month$rotation)
Pca_Three_month_scores<-as.data.frame(pca_scores_Three_month, row.names=TRUE)
write.csv(file="Pca_Three_month_data_less_than_8.csv",Pca_Three_month_data,row.names = TRUE)
write.csv(file="Pca_scores_Three_month_greater_than_15.csv",Pca_Three_month_scores,row.names=c(Three_month_data$Potno))
lm_model_pca <- lm(wb$Standard.deviation.of.bath.temp ~ pca_scores[,1:4])
summary(lm_model_pca)
