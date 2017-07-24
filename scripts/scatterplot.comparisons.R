library(ggplot2)
library(dplyr)
library(tidyr)
library(tibble)
library(genefilter)

setwd("~/Documents/dimitra/Workspace/RNA-Seq/Kits_comparison/")
all = read.delim("DEG_lists/commons/Pico.V4.Truseq.commons.txt")
head(all)

#FCs
all %>%
  ggplot(aes(x = logFC.ClontechPico, y = logFC.ClontechV4)) +
  geom_point(shape=1) +
  geom_smooth(method = lm, se=TRUE) +
  coord_cartesian(xlim=c(-10,7), ylim = c(-10,7)) +
  theme_bw(base_size = 30)
ggsave("~/Dropbox/Kits/Plots/Pico.vs.V4.FCs.jpg", height = 40, width = 40, units ="cm")

all %>%
  ggplot(aes(x = logFC.ClontechPico, y = logFC.Truseq)) +
  geom_point(shape=1) +
  geom_smooth(method = lm, se=TRUE) +
  coord_cartesian(xlim=c(-10,7), ylim = c(-10,7)) +
  theme_bw(base_size = 30)
ggsave("~/Dropbox/Kits/Plots/Pico.vs.Truseq.FCs.jpg", height = 40, width = 40, units ="cm")

all %>%
  ggplot(aes(x = logFC.ClontechV4, y = logFC.Truseq)) +
  geom_point(shape=1) +
  geom_smooth(method = lm, se=TRUE) +
  coord_cartesian(xlim=c(-10,7), ylim = c(-10,7)) +
  theme_bw(base_size = 30)
ggsave("~/Dropbox/Kits/Plots/Truseq.vs.V4.FCs.jpg", height = 40, width = 40, units ="cm")


#Qvalues
all %>%
  ggplot(aes(x = log(BH.qvalue.ClontechPico), y = log(BH.qvalue.ClontechV4))) +
  geom_point(shape=1) +
  geom_smooth(method = lm, se=TRUE)+
  coord_cartesian(xlim=c(-35,0), ylim = c(-35,0)) +
  theme_bw(base_size = 30)
ggsave("~/Dropbox/Kits/Plots/Pico.vs.V4.logqvalues.jpg", height = 40, width = 40, units ="cm")

all %>%
  ggplot(aes(x = log(BH.qvalue.ClontechPico), y = log(BH.qvalue.Truseq))) +
  geom_point(shape=1) +
  geom_smooth(method = lm, se=TRUE)+
  coord_cartesian(xlim=c(-35,0), ylim = c(-35,0)) +
  theme_bw(base_size = 30)
ggsave("~/Dropbox/Kits/Plots/Pico.vs.Truseq.logqvalues.jpg", height = 40, width = 40, units ="cm")

all %>%
  ggplot(aes(x = log(BH.qvalue.ClontechV4), y = log(BH.qvalue.Truseq))) +
  geom_point(shape=1) +
  geom_smooth(method = lm, se=TRUE)+
  coord_cartesian(xlim=c(-35,0), ylim = c(-35,0)) +
  theme_bw(base_size = 30)
ggsave("~/Dropbox/Kits/Plots/Truseq.vs.V4.logqvalues.jpg", height = 40, width = 40, units ="cm")


#Qvalues with log10
all %>%
  ggplot(aes(x = log10(BH.qvalue.ClontechPico), y = log10(BH.qvalue.ClontechV4))) +
  geom_point(shape=1) +
  geom_smooth(method = lm, se=TRUE)+
  coord_cartesian(xlim=c(-20,0), ylim = c(-20,0)) +
  theme_bw(base_size = 30)
ggsave("~/Dropbox/Kits/Plots/Pico.vs.V4.log10qvalues.jpg", height = 40, width = 40, units ="cm")

all %>%
  ggplot(aes(x = log10(BH.qvalue.ClontechPico), y = log10(BH.qvalue.Truseq))) +
  geom_point(shape=1) +
  geom_smooth(method = lm, se=TRUE)+
  coord_cartesian(xlim=c(-20,0), ylim = c(-20,0)) +
  theme_bw(base_size = 30)
ggsave("~/Dropbox/Kits/Plots/Pico.vs.Truseq.log10qvalues.jpg", height = 40, width = 40, units ="cm")

all %>%
  ggplot(aes(x = log10(BH.qvalue.ClontechV4), y = log10(BH.qvalue.Truseq))) +
  geom_point(shape=1) +
  geom_smooth(method = lm, se=TRUE)+
  coord_cartesian(xlim=c(-20,0), ylim = c(-20,0)) +
  theme_bw(base_size = 30)
ggsave("~/Dropbox/Kits/Plots/Truseq.vs.V4.log10qvalues.jpg", height = 40, width = 40, units ="cm")



#Qvalues not logged
all %>%
  ggplot(aes(x = BH.qvalue.ClontechPico, y = BH.qvalue.ClontechV4)) +
  geom_point(shape=1) +
  geom_smooth(method = lm, se=TRUE)+
  theme_bw(base_size = 30)
ggsave("~/Dropbox/Kits/Plots/Pico.vs.V4.qvalues.jpg", height = 40, width = 40, units ="cm")

all %>%
  ggplot(aes(x = BH.qvalue.ClontechPico, y = BH.qvalue.Truseq)) +
  geom_point(shape=1) +
  geom_smooth(method = lm, se=TRUE)+
  theme_bw(base_size = 30)
ggsave("~/Dropbox/Kits/Plots/Pico.vs.Truseq.qvalues.jpg", height = 40, width = 40, units ="cm")

all %>%
  ggplot(aes(x = BH.qvalue.ClontechV4, y = BH.qvalue.Truseq)) +
  geom_point(shape=1) +
  geom_smooth(method = lm, se=TRUE)+
  theme_bw(base_size = 30)
ggsave("~/Dropbox/Kits/Plots/Truseq.vs.V4.qvalues.jpg", height = 40, width = 40, units ="cm")



#Means
all$mean.ClontechPico = all %>%
  select(ILB.9579.ClontechPico:UNT.9576.ClontechPico) %>%
  rowMeans() 

all$mean.ClontechV4 = all %>%
  select(ILB.9579.ClontechV4:UNT.9576.ClontechV4) %>%
  rowMeans() 

all$mean.Truseq = all %>%
  select(ILB_9579.Truseq:UNT_9576.Truseq) %>%
  rowMeans() 

head(all)

all %>%
  ggplot(aes(x = log(mean.ClontechPico), y = log(mean.ClontechV4))) +
  geom_point(shape=1) +
  geom_smooth(method = lm, se=TRUE) +
  coord_cartesian(xlim=c(-3, 15), ylim = c(-3,15)) +
  theme_bw(base_size = 30)
ggsave("~/Dropbox/Kits/Plots/Pico.vs.V4.means.jpg", height = 40, width = 40, units ="cm")

all %>%
  ggplot(aes(x = log(mean.ClontechPico), y = log(mean.Truseq))) +
  geom_point(shape=1) +
  geom_smooth(method = lm, se=TRUE)+
  coord_cartesian(xlim=c(-3, 15), ylim = c(-3,15)) +
  theme_bw(base_size = 30)
ggsave("~/Dropbox/Kits/Plots/Pico.vs.Truseq.means.jpg", height = 40, width = 40, units ="cm")

all %>%
  ggplot(aes(x = log(mean.ClontechV4), y = log(mean.Truseq))) +
  geom_point(shape=1) +
  geom_smooth(method = lm, se=TRUE)+
  coord_cartesian(xlim=c(-3, 15), ylim = c(-3,15)) +
  theme_bw(base_size = 30)
ggsave("~/Dropbox/Kits/Plots/Truseq.vs.V4.means.jpg", height = 40, width = 40, units ="cm")



#Means by condition
all$mean.ClontechPico.UNT = all %>%
  select(UNT.9574.ClontechPico:UNT.9576.ClontechPico) %>%
  rowMeans() 
all$mean.ClontechV4.UNT = all %>%
  select(UNT.9574.ClontechV4:UNT.9576.ClontechV4) %>%
  rowMeans() 
all$mean.Truseq.UNT = all %>%
  select(UNT_9574.Truseq:UNT_9576.Truseq) %>%
  rowMeans() 

all$mean.ClontechPico.ILB = all %>%
  select(ILB.9579.ClontechPico:ILB.9583.ClontechPico) %>%
  rowMeans() 
all$mean.ClontechV4.ILB = all %>%
  select(ILB.9579.ClontechV4:ILB.9583.ClontechV4) %>%
  rowMeans() 
all$mean.Truseq.ILB = all %>%
  select(ILB_9579.Truseq:ILB_9583.Truseq) %>%
  rowMeans() 


all %>%
  ggplot(aes(x = log(mean.ClontechPico.UNT), y = log(mean.ClontechV4.UNT))) +
  geom_point(shape=1) +
  geom_smooth(method = lm, se=TRUE) +
  coord_cartesian(xlim=c(-3, 15), ylim = c(-3,15)) +
  theme_bw(base_size = 30)
ggsave("~/Dropbox/Kits/Plots/Pico.vs.V4.means.by.UNT.jpg", height = 40, width = 40, units ="cm")

all %>%
  ggplot(aes(x = log(mean.ClontechPico.ILB), y = log(mean.ClontechV4.ILB))) +
  geom_point(shape=1) +
  geom_smooth(method = lm, se=TRUE) +
  coord_cartesian(xlim=c(-3, 15), ylim = c(-3,15)) +
  theme_bw(base_size = 30)
ggsave("~/Dropbox/Kits/Plots/Pico.vs.V4.means.by.ILB.jpg", height = 40, width = 40, units ="cm")

all %>%
  ggplot(aes(x = log(mean.ClontechPico.UNT), y = log(mean.Truseq.UNT))) +
  geom_point(shape=1) +
  geom_smooth(method = lm, se=TRUE)+
  coord_cartesian(xlim=c(-3, 15), ylim = c(-3,15)) +
  theme_bw(base_size = 30)
ggsave("~/Dropbox/Kits/Plots/Pico.vs.Truseq.means.by.UNT.jpg", height = 40, width = 40, units ="cm")

all %>%
  ggplot(aes(x = log(mean.ClontechPico.ILB), y = log(mean.Truseq.ILB))) +
  geom_point(shape=1) +
  geom_smooth(method = lm, se=TRUE)+
  coord_cartesian(xlim=c(-3, 15), ylim = c(-3,15)) +
  theme_bw(base_size = 30)
ggsave("~/Dropbox/Kits/Plots/Pico.vs.Truseq.means.by.ILB.jpg", height = 40, width = 40, units ="cm")

all %>%
  ggplot(aes(x = log(mean.ClontechV4.UNT), y = log(mean.Truseq.UNT))) +
  geom_point(shape=1) +
  geom_smooth(method = lm, se=TRUE)+
  coord_cartesian(xlim=c(-3, 15), ylim = c(-3,15)) +
  theme_bw(base_size = 30)
ggsave("~/Dropbox/Kits/Plots/Truseq.vs.V4.means.by.UNT.jpg", height = 40, width = 40, units ="cm")

all %>%
  ggplot(aes(x = log(mean.ClontechV4.ILB), y = log(mean.Truseq.ILB))) +
  geom_point(shape=1) +
  geom_smooth(method = lm, se=TRUE)+
  coord_cartesian(xlim=c(-3, 15), ylim = c(-3,15)) +
  theme_bw(base_size = 30)
ggsave("~/Dropbox/Kits/Plots/Truseq.vs.V4.means.by.ILB.jpg", height = 40, width = 40, units ="cm")



#Variances
all$variance.ClontechPico = all %>%
  select(ILB.9579.ClontechPico:UNT.9576.ClontechPico) %>%
  rowVars()

all$variance.ClontechV4 = all %>%
  select(ILB.9579.ClontechV4:UNT.9576.ClontechV4) %>%
  rowVars() 

all$variance.Truseq = all %>%
  select(ILB_9579.Truseq:UNT_9576.Truseq) %>%
  rowVars() 

all %>%
  ggplot(aes(x = log(variance.ClontechPico), y = log(variance.ClontechV4))) +
  geom_point(shape=1) +
  geom_smooth(method = lm, se=TRUE)+
  coord_cartesian(xlim=c(-5, 25), ylim = c(-5,25)) +
  theme_bw(base_size = 30)
ggsave("~/Dropbox/Kits/Plots/Pico.vs.V4.variances.jpg", height = 40, width = 40, units ="cm")

all %>%
  ggplot(aes(x = log(variance.ClontechPico), y = log(variance.Truseq))) +
  geom_point(shape=1) +
  geom_smooth(method = lm, se=TRUE)+
  coord_cartesian(xlim=c(-5, 25), ylim = c(-5,25)) +
  theme_bw(base_size = 30)
ggsave("~/Dropbox/Kits/Plots/Pico.vs.Truseq.variances.jpg", height = 40, width = 40, units ="cm")

all %>%
  ggplot(aes(x = log(variance.ClontechV4), y = log(variance.Truseq))) +
  geom_point(shape=1) +
  geom_smooth(method = lm, se=TRUE)+
  coord_cartesian(xlim=c(-5, 25), ylim = c(-5,25)) +
  theme_bw(base_size = 30)
ggsave("~/Dropbox/Kits/Plots/Truseq.vs.V4.variances.jpg", height = 40, width = 40, units ="cm")



#Variances by condition
all$variance.ClontechPico.UNT = all %>%
  select(UNT.9574.ClontechPico:UNT.9576.ClontechPico) %>%
  rowVars()
all$variance.ClontechV4.UNT = all %>%
  select(UNT.9574.ClontechV4:UNT.9576.ClontechV4) %>%
  rowVars() 
all$variance.Truseq.UNT = all %>%
  select(UNT_9574.Truseq:UNT_9576.Truseq) %>%
  rowVars() 

all$variance.ClontechPico.ILB = all %>%
  select(ILB.9579.ClontechPico:ILB.9583.ClontechPico) %>%
  rowVars()
all$variance.ClontechV4.ILB = all %>%
  select(ILB.9579.ClontechV4:ILB.9583.ClontechV4) %>%
  rowVars() 
all$variance.Truseq.ILB = all %>%
  select(ILB_9579.Truseq:ILB_9583.Truseq) %>%
  rowVars() 


all %>%
  ggplot(aes(x = log(variance.ClontechPico.UNT), y = log(variance.ClontechV4.UNT))) +
  geom_point(shape=1) +
  geom_smooth(method = lm, se=TRUE)+
  coord_cartesian(xlim=c(-5, 25), ylim = c(-5,25)) +
  theme_bw(base_size = 30)
ggsave("~/Dropbox/Kits/Plots/Pico.vs.V4.variances.by.UNT.jpg", height = 40, width = 40, units ="cm")

all %>%
  ggplot(aes(x = log(variance.ClontechPico.ILB), y = log(variance.ClontechV4.ILB))) +
  geom_point(shape=1) +
  geom_smooth(method = lm, se=TRUE)+
  coord_cartesian(xlim=c(-5, 25), ylim = c(-5,25)) +
  theme_bw(base_size = 30)
ggsave("~/Dropbox/Kits/Plots/Pico.vs.V4.variances.by.ILB.jpg", height = 40, width = 40, units ="cm")

all %>%
  ggplot(aes(x = log(variance.ClontechPico.UNT), y = log(variance.Truseq.UNT))) +
  geom_point(shape=1) +
  geom_smooth(method = lm, se=TRUE)+
  coord_cartesian(xlim=c(-5, 25), ylim = c(-5,25)) +
  theme_bw(base_size = 30)
ggsave("~/Dropbox/Kits/Plots/Pico.vs.Truseq.variances.by.UNT.jpg", height = 40, width = 40, units ="cm")

all %>%
  ggplot(aes(x = log(variance.ClontechPico.ILB), y = log(variance.Truseq.ILB))) +
  geom_point(shape=1) +
  geom_smooth(method = lm, se=TRUE)+
  coord_cartesian(xlim=c(-5, 25), ylim = c(-5,25)) +
  theme_bw(base_size = 30)
ggsave("~/Dropbox/Kits/Plots/Pico.vs.Truseq.variances.by.ILB.jpg", height = 40, width = 40, units ="cm")

all %>%
  ggplot(aes(x = log(variance.ClontechV4.UNT), y = log(variance.Truseq.UNT))) +
  geom_point(shape=1) +
  geom_smooth(method = lm, se=TRUE)+
  coord_cartesian(xlim=c(-5, 25), ylim = c(-5,25)) +
  theme_bw(base_size = 30)
ggsave("~/Dropbox/Kits/Plots/Truseq.vs.V4.variances.by.UNT.jpg", height = 40, width = 40, units ="cm")

all %>%
  ggplot(aes(x = log(variance.ClontechV4.ILB), y = log(variance.Truseq.ILB))) +
  geom_point(shape=1) +
  geom_smooth(method = lm, se=TRUE)+
  coord_cartesian(xlim=c(-5, 25), ylim = c(-5,25)) +
  theme_bw(base_size = 30)
ggsave("~/Dropbox/Kits/Plots/Truseq.vs.V4.variances.by.ILB.jpg", height = 40, width = 40, units ="cm")




#Mean vs Variance within each kit
all %>%
  # ggplot(aes(x = log(mean.ClontechPico), y = log(variance.ClontechPico))) +
  # ggplot(aes(x = log(mean.ClontechV4), y = log(variance.ClontechV4))) +
  ggplot(aes(x = log(mean.Truseq), y = log(variance.Truseq))) +
  geom_point(shape=1) +
  geom_smooth(method = lm, se=TRUE) +
  coord_cartesian(xlim=c(-5, 15), ylim = c(-5,25)) +
  theme_bw(base_size = 30)
# ggsave("~/Dropbox/Kits/Plots/mean.vs.var.Pico.jpg", height = 40, width = 40, units ="cm")
# ggsave("~/Dropbox/Kits/Plots/mean.vs.var.V4.jpg", height = 40, width = 40, units ="cm")
ggsave("~/Dropbox/Kits/Plots/mean.vs.var.Truseq.jpg", height = 40, width = 40, units ="cm")


#Mean -UNT vs Variance -UNT within each kit
all %>%
  # ggplot(aes(x = log(mean.ClontechPico.UNT), y = log(variance.ClontechPico.UNT))) +
  # ggplot(aes(x = log(mean.ClontechV4.UNT), y = log(variance.ClontechV4.UNT))) +
  ggplot(aes(x = log(mean.Truseq.UNT), y = log(variance.Truseq.UNT))) +
  geom_point(shape=1) +
  geom_smooth(method = lm, se=TRUE) +
  coord_cartesian(xlim=c(-5, 15), ylim = c(-5,25)) +
  theme_bw(base_size = 30)
# ggsave("~/Dropbox/Kits/Plots/mean.vs.var.Pico.by.UNT.jpg", height = 40, width = 40, units ="cm")
# ggsave("~/Dropbox/Kits/Plots/mean.vs.var.V4.by.UNT.jpg", height = 40, width = 40, units ="cm")
ggsave("~/Dropbox/Kits/Plots/mean.vs.var.Truseq.by.UNT.jpg", height = 40, width = 40, units ="cm")


#Mean -ILB vs Variance -ILB within each kit
all %>%
  # ggplot(aes(x = log(mean.ClontechPico.ILB), y = log(variance.ClontechPico.ILB))) +
  # ggplot(aes(x = log(mean.ClontechV4.ILB), y = log(variance.ClontechV4.ILB))) +
  ggplot(aes(x = log(mean.Truseq.ILB), y = log(variance.Truseq.ILB))) +
  geom_point(shape=1) +
  geom_smooth(method = lm, se=TRUE) +
  coord_cartesian(xlim=c(-5, 15), ylim = c(-5,25)) +
  theme_bw(base_size = 30)
# ggsave("~/Dropbox/Kits/Plots/mean.vs.var.Pico.by.ILB.jpg", height = 40, width = 40, units ="cm")
# ggsave("~/Dropbox/Kits/Plots/mean.vs.var.V4.by.ILB.jpg", height = 40, width = 40, units ="cm")
ggsave("~/Dropbox/Kits/Plots/mean.vs.var.Truseq.by.ILB.jpg", height = 40, width = 40, units ="cm")





#Rank by differences in ranks (Sort by the mean per kit. Assign rank. Take the difference of ranks. Sort by diff.rank.)
all$rank.ClontechPico = dense_rank(all$mean.ClontechPico)  
all$rank.ClontechV4 = dense_rank(all$mean.ClontechV4) 
all$rank.Truseq = dense_rank(all$mean.Truseq)

all$rank.Pico_V4 = all$rank.ClontechPico - all$rank.ClontechV4
all$rank.Pico_Truseq = all$rank.ClontechPico - all$rank.Truseq
all$rank.Truseq_V4 = all$rank.Truseq - all$rank.ClontechV4

all.by.rank.Pico_V4 = all %>%
  arrange(desc(rank.Pico_V4))
write.table(all.by.rank.Pico_V4, file="~/Dropbox/Kits/all.kits.ranked.by.rank.Pico_V4.txt", sep="\t", row.names = FALSE)

all.by.rank.Pico_Truseq = all %>%
  arrange(desc(rank.Pico_Truseq))
write.table(all.by.rank.Pico_Truseq, file="~/Dropbox/Kits/all.kits.ranked.by.rank.Pico_Truseq.txt", sep="\t", row.names = FALSE)

all.by.rank.Truseq_V4 = all %>%
  arrange(desc(rank.Truseq_V4))
write.table(all.by.rank.Truseq_V4, file="~/Dropbox/Kits/all.kits.ranked.by.rank.Truseq_V4.txt", sep="\t", row.names = FALSE)


all.by.rank.Pico_V4 %>%
  arrange(desc(rank.Pico_V4)) %>%
  ggplot(aes(rank.Pico_V4)) +
  geom_histogram(bins = 100) +
  theme_bw(base_size = 30)
ggsave("~/Dropbox/Kits/Plots/rank.Pico_V4.histogram.jpg", height = 40, width = 40, units ="cm")

all.by.rank.Pico_Truseq %>%
  arrange(desc(rank.Pico_Truseq)) %>%
  ggplot(aes(rank.Pico_Truseq)) +
  geom_histogram(bins = 100) +
  theme_bw(base_size = 30)
ggsave("~/Dropbox/Kits/Plots/rank.Pico_Truseq.histogram.jpg", height = 40, width = 40, units ="cm")

all.by.rank.Truseq_V4 %>%
  arrange(desc(rank.Truseq_V4)) %>%
  ggplot(aes(rank.Truseq_V4)) +
  geom_histogram(bins = 100) +
  theme_bw(base_size = 30)
ggsave("~/Dropbox/Kits/Plots/rank.Truseq_V4.histogram.jpg", height = 40, width = 40, units ="cm")



#Rank by row number
all$rank2.ClontechPico = row_number(all$mean.ClontechPico)
all$rank2.ClontechV4 = row_number(all$mean.ClontechV4) 
all$rank2.Truseq = row_number(all$mean.Truseq)

all$rank2.Pico_V4 = all$rank2.ClontechPico - all$rank2.ClontechV4
all$rank2.Pico_Truseq = all$rank2.ClontechPico - all$rank2.Truseq
all$rank2.Truseq_V4 = all$rank2.Truseq - all$rank2.ClontechV4

all.by.rank2.Pico_V4 = all %>%
  arrange(desc(rank2.Pico_V4)) 
all.by.rank2.Pico_V4 %>%
  ggplot(aes(rank2.Pico_V4)) +
  geom_histogram(bins = 100) +
  theme_bw(base_size = 30)
ggsave("~/Dropbox/Kits/Plots/rank2.Pico_V4.histogram.jpg", height = 40, width = 40, units ="cm")
write.table(all.by.rank2.Pico_V4, file="~/Dropbox/Kits/all.kits.ranked.by.rank2.Pico_V4.txt", sep="\t", row.names = FALSE)

all.by.rank2.Pico_Truseq = all %>%
  arrange(desc(rank2.Pico_Truseq)) 
all.by.rank2.Pico_Truseq %>%
  ggplot(aes(rank2.Pico_Truseq)) +
  geom_histogram(bins = 100) +
  theme_bw(base_size = 30)
ggsave("~/Dropbox/Kits/Plots/rank2.Pico_Truseq.histogram.jpg", height = 40, width = 40, units ="cm")
write.table(all.by.rank2.Pico_Truseq, file="~/Dropbox/Kits/all.kits.ranked.by.rank2.Pico_Truseq.txt", sep="\t", row.names = FALSE)

all.by.rank2.Truseq_V4 = all %>%
  arrange(desc(rank2.Truseq_V4)) 
all.by.rank2.Truseq_V4 %>%
  ggplot(aes(rank2.Truseq_V4)) +
  geom_histogram(bins = 100) +
  theme_bw(base_size = 30)
ggsave("~/Dropbox/Kits/Plots/rank2.Truseq_V4.histogram.jpg", height = 40, width = 40, units ="cm")
write.table(all.by.rank2.Truseq_V4, file="~/Dropbox/Kits/all.kits.ranked.by.rank2.Truseq_V4.txt", sep="\t", row.names = FALSE)



# FCs with Soum's method (+20)
all$FC20.ClontechPico = (all$mean.ClontechPico.ILB + 20) / (all$mean.ClontechPico.UNT + 20)
all$FC20.ClontechV4 = (all$mean.ClontechV4.ILB + 20) / (all$mean.ClontechV4.UNT + 20)
all$FC20.Truseq = (all$mean.Truseq.ILB + 20) / (all$mean.Truseq.UNT + 20)

# all$log2FC20.ClontechPico = log2((all$mean.ClontechPico.ILB + 20) / (all$mean.ClontechPico.UNT + 20))
# all$log2FC20.ClontechV4 = log2((all$mean.ClontechV4.ILB + 20) / (all$mean.ClontechV4.UNT + 20))
# all$log2FC20.Truseq = log2((all$mean.Truseq.ILB + 20) / (all$mean.Truseq.UNT + 20))
# #or
# all$logeFC20.ClontechPico = log((all$mean.ClontechPico.ILB + 20) / (all$mean.ClontechPico.UNT + 20))
# all$logeFC20.ClontechV4 = log((all$mean.ClontechV4.ILB + 20) / (all$mean.ClontechV4.UNT + 20))
# all$logeFC20.Truseq = log((all$mean.Truseq.ILB + 20) / (all$mean.Truseq.UNT + 20))
#or
all$log10FC20.ClontechPico = log10((all$mean.ClontechPico.ILB + 20) / (all$mean.ClontechPico.UNT + 20))
all$log10FC20.ClontechV4 = log10((all$mean.ClontechV4.ILB + 20) / (all$mean.ClontechV4.UNT + 20))
all$log10FC20.Truseq = log10((all$mean.Truseq.ILB + 20) / (all$mean.Truseq.UNT + 20))


all %>%
  ggplot(aes(x = log10FC20.ClontechPico, y = log10FC20.ClontechV4)) +
  geom_point(shape=1) +
  geom_smooth(method = lm, se=TRUE) +
  coord_cartesian(xlim=c(-3,3), ylim = c(-3,3.5)) +
  theme_bw(base_size = 30)
ggsave("~/Dropbox/Kits/Plots/Pico.vs.V4.log10FC20.jpg", height = 40, width = 40, units ="cm")

all %>%
  ggplot(aes(x = log10FC20.ClontechPico, y = log10FC20.Truseq)) +
  geom_point(shape=1) +
  geom_smooth(method = lm, se=TRUE) +
  coord_cartesian(xlim=c(-3,3), ylim = c(-3,3.5)) +
  theme_bw(base_size = 30)
ggsave("~/Dropbox/Kits/Plots/Pico.vs.Truseq.log10FC20.jpg", height = 40, width = 40, units ="cm")

all %>%
  ggplot(aes(x = log10FC20.ClontechV4, y = log10FC20.Truseq)) +
  geom_point(shape=1) +
  geom_smooth(method = lm, se=TRUE) +
  coord_cartesian(xlim=c(-3,3), ylim = c(-3,3.5)) +
  theme_bw(base_size = 30)
ggsave("~/Dropbox/Kits/Plots/Truseq.vs.V4.log10FC20.jpg", height = 40, width = 40, units ="cm")




# Spearman's rank correlation on sample level (same sample, across 3 kits)
#correlate multiple samples
library(corrplot)

all.ILB.9579 = all %>%
  select(ILB_9579.Truseq,ILB.9579.ClontechPico,ILB.9579.ClontechV4)
all.ILB.9579.cor = cor(all.ILB.9579, method = "spearman")
jpeg("Plots/correlation/ILB.9579.correlation.jpg", height = 25, width = 20, units ="cm", res = 150)
corrplot(all.ILB.9579.cor, type="upper", order="hclust", method = "number")
dev.off()


all.ILB.9582 = all %>%
  select(ILB_9582.Truseq,ILB.9582.ClontechPico,ILB.9582.ClontechV4)
all.ILB.9582.cor = cor(all.ILB.9582, method = "spearman")
jpeg("Plots/correlation/ILB.9582.correlation.jpg", height = 25, width = 20, units ="cm", res = 150)
corrplot(all.ILB.9582.cor, type="upper", order="hclust", method = "number")
dev.off()


all.ILB.9583 = all %>%
  select(ILB_9583.Truseq,ILB.9583.ClontechPico,ILB.9583.ClontechV4)
all.ILB.9583.cor = cor(all.ILB.9583, method = "spearman")
jpeg("Plots/correlation/ILB.9583.correlation.jpg", height = 25, width = 20, units ="cm", res = 150)
corrplot(all.ILB.9583.cor, type="upper", order="hclust", method = "number")
dev.off()


all.UNT.9574 = all %>%
  select(UNT_9574.Truseq,UNT.9574.ClontechPico,UNT.9574.ClontechV4)
all.UNT.9574.cor = cor(all.UNT.9574, method = "spearman")
jpeg("Plots/correlation/UNT.9574.correlation.jpg", height = 25, width = 20, units ="cm", res = 150)
corrplot(all.UNT.9574.cor, type="upper", order="hclust", method = "number")
dev.off()


all.UNT.9575 = all %>%
  select(UNT_9575.Truseq,UNT.9575.ClontechPico,UNT.9575.ClontechV4)
all.UNT.9575.cor = cor(all.UNT.9575, method = "spearman")
jpeg("Plots/correlation/UNT.9575.correlation.jpg", height = 25, width = 20, units ="cm", res = 150)
corrplot(all.UNT.9575.cor, type="upper", order="hclust", method = "number")
dev.off()


all.UNT.9576 = all %>%
  select(UNT_9576.Truseq,UNT.9576.ClontechPico,UNT.9576.ClontechV4)
all.UNT.9576.cor = cor(all.UNT.9576, method = "spearman")
jpeg("Plots/correlation/UNT.9576.correlation.jpg", height = 25, width = 20, units ="cm", res = 150)
corrplot(all.UNT.9576.cor, type="upper", order="hclust", method = "number")
dev.off()
