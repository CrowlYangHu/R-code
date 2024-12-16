library(ape)
library(corrplot)
library(cowplot)
library(ggbeeswarm)
library(ggExtra)
library(ggplot2)
library(ggpmisc)
library(ggpubr)
library(ggsci)
library(ggthemes)
library(gridExtra)
library(Hmisc)
library(metafor)
library(pheatmap)
library(plyr)
library(randomForest)
library(RColorBrewer)
library(reshape2)
library(rfPermute)
library(SuppDists)
library(tidyverse)
library(vegan)

###Columnar scatter plot
ggplot(f1.data, aes(X, BNC))+
  geom_jitter(aes(fill = Group),
              position = position_jitter(width = 0.3, height = 0, seed = NA),
              shape=21, size = 5,color="black")+
  scale_fill_manual(values = c("#a1c893","#c2c881","#dbc079","#E26844",
                               "#b7d7ac","#ced394","#dec78a","#e47e60",
                               "#c5dfbc","#dadfa7","#e2ce9b","#e5947c",
                               "#d2e6cb","#e6eaba","#e6d5ac","#e7aa98",
                               "#dfedda","#f2f5cd","#e9dcbd","#e8c0b4",
                               "#edf2eb","#f8fbd6","#ebe0c5","#e9cbc2"))+
  stat_summary(fun.data="mean_sdl", fun.args = list(mult=1),
               geom="pointrange", color = "black",size = 1)+
  stat_summary(fun.y="mean", fun.args = list(mult=1),
               geom="point", color = "white",size = 4)+
  theme_classic()+
  theme(panel.background=element_rect(fill="white",colour="black",size=0.25),
        axis.line=element_line(colour="black",size=0.25),
        axis.title=element_text(size=13,face="plain",color="black"),
        axis.text = element_text(size=12,face="plain",color="black"),
        legend.position="none")


###Effect size
summary_data <- raw_data %>%
  filter(Group != "A") %>%
  group_by(Group) %>%
  summarize(
    TMean = mean(SOC),
    TSD = sd(SOC),
    TN = n(),
    CMean = mean(raw_data$SOC[raw_data$Group == "A"]),
    CSD = sd(raw_data$SOC[raw_data$Group == "A"]),
    CN = n()
  )
print(summary_data)
d1 <- summary_data
d2 <- escalc(measure = "ROM", data = d1, m1i = TMean, sd1i = TSD, n1i = TN, m2i = CMean, sd2i = CSD, n2i = CN)
r1 <- rma(yi, vi, data = d2, method = "REML")
summary(r1)
kk1 <- summary(r1)
subgroups <- unique(d2$Group)
subgroup_results <- list()
for (subgroup in subgroups) {
  subgroup_data <- subset(d2, Group == subgroup)
  subgroup_result <- rma(yi, vi, data = subgroup_data, method = "REML") 
  subgroup_results[[subgroup]] <- subgroup_result
}
for (subgroup in subgroups) {
  print(subgroup_results[[subgroup]])
}
combined_data1 <- data.frame(
  estimate = kk1$b,
  se = kk1$se,
  pval = kk1$pval
)
combined_subgroup_data <- list()
for (subgroup in subgroups) {
  subgroup_result <- subgroup_results[[subgroup]]
  combined_subgroup_data[[subgroup]] <- data.frame(
    estimate = subgroup_result$b,
    se = subgroup_result$se,
    pval = subgroup_result$pval
  )
}
combined_data_subgroup <- do.call(rbind, combined_subgroup_data)
combined_data_subgroup$Subgroup <- rownames(combined_data_subgroup)
print(combined_data_subgroup)
data_df <- combined_data_subgroup
data_df$sig <- ""
for (i in 1:nrow(data_df)) {
  if (data_df$pval[i] < 0.001) {
    data_df$sig[i] <- "***"
  } else if (data_df$pval[i] < 0.01) {
    data_df$sig[i] <- "**"
  } else if (data_df$pval[i] < 0.05) {
    data_df$sig[i] <- "*"
  }
}
print(data_df)
ggplot(data_df,aes(Subgroup,estimate,fill=Subgroup))+
  geom_bar(data=data_df,mapping=aes(Subgroup,estimate),size = 1.2,
           position="dodge", stat="identity",width = 0.7)+
  geom_errorbar(aes(ymin = estimate + se, ymax = estimate - se,color=Subgroup),
                width = 0,position = position_dodge(width = 0.7),cex=0.9)+
  labs(y="", x="")+
  scale_color_manual(values=c("#AAD09D","#FCDC89","#E26844")) +
  scale_fill_manual(values=c("#AAD09D","#FCDC89","#E26844")) +
  theme_base()


###Linear fitting
ggplot(data = data, aes(x = BODY, y = CUE)) +
  geom_jitter(aes(x = BODY, y = CUE, fill = Group),
              width= 0.3,height= 0.3,size=5,alpha=1.0,pch=21)+
  geom_smooth(method = "lm", se=T,color = 'black',size=1.3,formula = y ~ x) +
  stat_poly_eq(formula = y ~ x,
               aes(label = paste(..eq.label.., 
                                 ..rr.label.., 
                                 ..p.value.label..,
                                 sep = "~~~")), 
               parse = TRUE) +
  scale_fill_manual(values = c("#AAD09D","#E3EA96","#E26844","#FCDC89"))+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.text=element_text(color="black"),
        legend.title = element_blank())


###Nonlinear fitting
ggplot(data = data, aes(x = BODY, y = CUE)) +
  geom_jitter(aes(x = BODY, y = CUE, fill = Group),
              width= 0.3,height= 0.3,size=5,alpha=1.0,pch=21)+
  geom_smooth(method = "lm", se=T,color = 'black',size=1.3,formula = y ~ poly(x, 2, raw = TRUE)) +
  stat_poly_eq(formula = y ~ poly(x, 2, raw = TRUE),
               aes(label = paste(..eq.label.., 
                                 ..rr.label.., 
                                 ..p.value.label..,
                                 sep = "~~~")), 
               parse = TRUE) +
  scale_fill_manual(values = c("#AAD09D","#E3EA96","#E26844","#FCDC89"))+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.text=element_text(color="black"),
        legend.title = element_blank())


###PCoA
data <- vegdist(t(aa), method = "bray")
pcoa<- pcoa(data, correction = "none", rn = NULL)
PCA1 = pcoa$vectors[,1]
PCA2 = pcoa$vectors[,2]
index<- data.frame(rownames(pcoa$vectors),PCA1,PCA2,bb$fenzu)
colnames(index) <-c("sample","PCA1","PCA2","group")
pca1 <-floor(pcoa$values$Relative_eig[1]*100)
pca2 <-floor(pcoa$values$Relative_eig[2]*100)
pp=ggplot(index,aes(PCA1,PCA2,colour=index$group))+ 
  geom_point(aes(shape = index$group),size=5,alpha=1)+
  scale_shape_manual(values = c(16,16,16,16,17,17,17,17)) +
  scale_fill_manual(values=c("#66BC98","#AAD09D","#FCDC89","#E26844"))+
  scale_color_manual(values=c("#66BC98","#AAD09D","#FCDC89","#E26844"))+
  xlab(paste("PCoA1 ( ",pca1,"%"," )",sep="")) +
  ylab(paste("PCoA2 ( ",pca2,"%"," )",sep=""))+
  stat_ellipse(aes(fill=index$group),geom = "polygon",size=1.2,level = 0.95,alpha = 0.03)+theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),legend.position = "none")+
  theme(axis.text = element_text(color = "black",size =11))
ggMarginal(pp,type="density",size=5,margins="both",groupColour=TRUE,groupFill=TRUE)
df_anosim <- anosim(data,bb$fenzu,permutations = 999)
MRPP <- mrpp(data,bb$fenzu,permutations = 999)
Adonis <- adonis2(data~bb$fenzu,data=aa,
                  distance = "bray",
                  permutations = 999)

###heatmap
data <- rcorr(as.matrix(df))
r_value<-data$r
p_value<-data$P
r_related<-r_value[1:30,31:56]
p_related<-p_value[1:30,31:56]
r_related[p_related>0.05] = 0
pheatmap(r_related,fontsize_number=10,fontsize =8, cluster_cols = FALSE, cluster_rows = FALSE,
         border="white", 
         color=colorRampPalette(c("#E26844","white","#B8DBB3"))(100))



###Random forest
set.seed(1234)
rf_results<-rfPermute(design$A~., data =design, 
                      importance = TRUE, ntree = 800)
predictor_var<- data.frame(importance(rf_results, scale = TRUE), 
                           check.names = FALSE)
predictor_var