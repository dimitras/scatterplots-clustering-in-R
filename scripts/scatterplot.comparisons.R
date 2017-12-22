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
all$mean.Pico = all %>%
  select(ILB.9579.ClontechPico:UNT.9576.ClontechPico) %>%
  rowMeans() 

all$mean.V4 = all %>%
  select(ILB.9579.ClontechV4:UNT.9576.ClontechV4) %>%
  rowMeans() 

all$mean.Truseq = all %>%
  select(ILB_9579.Truseq:UNT_9576.Truseq) %>%
  rowMeans() 

head(all)

all %>%
  ggplot(aes(x = log(mean.Pico), y = log(mean.V4))) +
  geom_point(shape=1) +
  geom_smooth(method = lm, se=TRUE) +
  coord_cartesian(xlim=c(-3, 15), ylim = c(-3,15)) +
  theme_bw(base_size = 50)
ggsave("Plots/Pico.vs.V4.means_.jpg", height = 40, width = 40, units ="cm")

all %>%
  ggplot(aes(x = log(mean.Pico), y = log(mean.Truseq))) +
  geom_point(shape=1) +
  geom_smooth(method = lm, se=TRUE)+
  coord_cartesian(xlim=c(-3, 15), ylim = c(-3,15)) +
  theme_bw(base_size = 50)
ggsave("Plots/Pico.vs.Truseq.means_.jpg", height = 40, width = 40, units ="cm")

all %>%
  ggplot(aes(x = log(mean.V4), y = log(mean.Truseq))) +
  geom_point(shape=1) +
  geom_smooth(method = lm, se=TRUE)+
  coord_cartesian(xlim=c(-3, 15), ylim = c(-3,15)) +
  theme_bw(base_size = 50)
ggsave("Plots/Truseq.vs.V4.means_.jpg", height = 40, width = 40, units ="cm")



#Means by condition
all$mean.Pico.UNT = all %>%
  select(UNT.9574.ClontechPico:UNT.9576.ClontechPico) %>%
  rowMeans() 
all$mean.V4.UNT = all %>%
  select(UNT.9574.ClontechV4:UNT.9576.ClontechV4) %>%
  rowMeans() 
all$mean.Truseq.UNT = all %>%
  select(UNT_9574.Truseq:UNT_9576.Truseq) %>%
  rowMeans() 

all$mean.Pico.ILB = all %>%
  select(ILB.9579.ClontechPico:ILB.9583.ClontechPico) %>%
  rowMeans() 
all$mean.V4.ILB = all %>%
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
all$variance.Pico = all %>%
  select(ILB.9579.ClontechPico:UNT.9576.ClontechPico) %>%
  rowVars()

all$variance.V4 = all %>%
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
  # ggplot(aes(x = log(mean.Pico), y = log(variance.Pico))) +
  # ggplot(aes(x = log(mean.V4), y = log(variance.V4))) +
  ggplot(aes(x = log(mean.Truseq), y = log(variance.Truseq))) +
  geom_point(shape=1) +
  geom_smooth(method = lm, se=TRUE) +
  coord_cartesian(xlim=c(-5, 15), ylim = c(-5,25)) +
  theme_bw(base_size = 50)
ggsave("Plots/mean.vs.var.Pico_.jpg", height = 40, width = 40, units ="cm")
ggsave("Plots/mean.vs.var.V4_.jpg", height = 40, width = 40, units ="cm")
ggsave("Plots/mean.vs.var.Truseq_.jpg", height = 40, width = 40, units ="cm")


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
all$rank2.Pico = row_number(all$mean.Pico)
all$rank2.V4 = row_number(all$mean.V4) 
all$rank2.Truseq = row_number(all$mean.Truseq)

all$rank2.Pico_V4 = all$rank2.Pico - all$rank2.V4
all$rank2.Pico_Truseq = all$rank2.Pico - all$rank2.Truseq
all$rank2.Truseq_V4 = all$rank2.Truseq - all$rank2.V4

all.by.rank2.Pico_V4 = all %>%
  arrange(desc(rank2.Pico_V4)) 
all.by.rank2.Pico_V4 %>%
  ggplot(aes(rank2.Pico_V4)) +
  geom_histogram(bins = 100, fill = ble) +
  theme_bw(base_size = 50)
ggsave("Plots/rank2.Pico_V4.histogram.jpg", height = 40, width = 40, units ="cm")
write.table(all.by.rank2.Pico_V4, file="Plots/all.kits.ranked.by.rank2.Pico_V4.txt", sep="\t", row.names = FALSE)

all.by.rank2.Pico_Truseq = all %>%
  arrange(desc(rank2.Pico_Truseq)) 
all.by.rank2.Pico_Truseq %>%
  ggplot(aes(rank2.Pico_Truseq)) +
  geom_histogram(bins = 100, fill=ble) +
  theme_bw(base_size = 50)
ggsave("Plots/rank2.Pico_Truseq.histogram.jpg", height = 40, width = 40, units ="cm")
write.table(all.by.rank2.Pico_Truseq, file="Plots/all.kits.ranked.by.rank2.Pico_Truseq.txt", sep="\t", row.names = FALSE)

all.by.rank2.Truseq_V4 = all %>%
  arrange(desc(rank2.Truseq_V4)) 
all.by.rank2.Truseq_V4 %>%
  ggplot(aes(rank2.Truseq_V4)) +
  geom_histogram(bins = 100, fill = ble) +
  theme_bw(base_size = 50)
ggsave("Plots/rank2.Truseq_V4.histogram.jpg", height = 40, width = 40, units ="cm")
write.table(all.by.rank2.Truseq_V4, file="Plots/all.kits.ranked.by.rank2.Truseq_V4.txt", sep="\t", row.names = FALSE)



# FCs with Soum's method (+20)
all$FC20.Pico = (all$mean.Pico.ILB + 20) / (all$mean.Pico.UNT + 20)
all$FC20.V4 = (all$mean.V4.ILB + 20) / (all$mean.V4.UNT + 20)
all$FC20.Truseq = (all$mean.Truseq.ILB + 20) / (all$mean.Truseq.UNT + 20)

# all$log2FC20.ClontechPico = log2((all$mean.ClontechPico.ILB + 20) / (all$mean.ClontechPico.UNT + 20))
# all$log2FC20.ClontechV4 = log2((all$mean.ClontechV4.ILB + 20) / (all$mean.ClontechV4.UNT + 20))
# all$log2FC20.Truseq = log2((all$mean.Truseq.ILB + 20) / (all$mean.Truseq.UNT + 20))
# #or
# all$logeFC20.ClontechPico = log((all$mean.ClontechPico.ILB + 20) / (all$mean.ClontechPico.UNT + 20))
# all$logeFC20.ClontechV4 = log((all$mean.ClontechV4.ILB + 20) / (all$mean.ClontechV4.UNT + 20))
# all$logeFC20.Truseq = log((all$mean.Truseq.ILB + 20) / (all$mean.Truseq.UNT + 20))
#or
all$log10FC20.Pico = log10((all$mean.Pico.ILB + 20) / (all$mean.Pico.UNT + 20))
all$log10FC20.V4 = log10((all$mean.V4.ILB + 20) / (all$mean.V4.UNT + 20))
all$log10FC20.Truseq = log10((all$mean.Truseq.ILB + 20) / (all$mean.Truseq.UNT + 20))


all %>%
  ggplot(aes(x = log10FC20.Pico, y = log10FC20.V4)) +
  geom_point(shape=1) +
  geom_smooth(method = lm, se=TRUE) +
  coord_cartesian(xlim=c(-3,3), ylim = c(-3,3.5)) +
  theme_bw(base_size = 50)
ggsave("Plots/Pico.vs.V4.log10FC20_.jpg", height = 40, width = 40, units ="cm")

all %>%
  ggplot(aes(x = log10FC20.Pico, y = log10FC20.Truseq)) +
  geom_point(shape=1) +
  geom_smooth(method = lm, se=TRUE) +
  coord_cartesian(xlim=c(-3,3), ylim = c(-3,3.5)) +
  theme_bw(base_size = 50)
ggsave("Plots/Pico.vs.Truseq.log10FC20.jpg", height = 40, width = 40, units ="cm")

all %>%
  ggplot(aes(x = log10FC20.V4, y = log10FC20.Truseq)) +
  geom_point(shape=1) +
  geom_smooth(method = lm, se=TRUE) +
  coord_cartesian(xlim=c(-3,3), ylim = c(-3,3.5)) +
  theme_bw(base_size = 50)
ggsave("Plots/Truseq.vs.V4.log10FC20.jpg", height = 40, width = 40, units ="cm")




# Spearman's rank correlation on sample level (same sample, across 3 kits)
#correlate multiple samples
library(corrplot)

all.ILB.9579 = all %>%
  select(ILB_9579.Truseq,ILB.9579.ClontechPico,ILB.9579.ClontechV4)
colnames(all.ILB.9579) = c("Truseq","Pico","V4")
all.ILB.9579.cor = cor(all.ILB.9579, method = "spearman")
jpeg("Plots/correlation/ILB.9579.correlation.jpg", height = 40, width = 40, units ="cm", res = 300, pointsize = 40)
corrplot(all.ILB.9579.cor, type="upper", order="hclust", method = "number", mar=c(0,0,1,0), title="ILB.9579")
dev.off()


all.ILB.9582 = all %>%
  select(ILB_9582.Truseq,ILB.9582.ClontechPico,ILB.9582.ClontechV4)
colnames(all.ILB.9582) = c("Truseq","Pico","V4")
all.ILB.9582.cor = cor(all.ILB.9582, method = "spearman")
jpeg("Plots/correlation/ILB.9582.correlation.jpg", height = 40, width = 40, units ="cm", res = 300, pointsize = 40)
corrplot(all.ILB.9582.cor, type="upper", order="hclust", method = "number", mar=c(0,0,1,0), title="ILB.9582")
dev.off()


all.ILB.9583 = all %>%
  select(ILB_9583.Truseq,ILB.9583.ClontechPico,ILB.9583.ClontechV4)
colnames(all.ILB.9583) = c("Truseq","Pico","V4")
all.ILB.9583.cor = cor(all.ILB.9583, method = "spearman")
jpeg("Plots/correlation/ILB.9583.correlation.jpg", height = 40, width = 40, units ="cm", res = 300, pointsize = 40)
corrplot(all.ILB.9583.cor, type="upper", order="hclust", method = "number", mar=c(0,0,1,0), title="ILB.9583")
dev.off()


all.UNT.9574 = all %>%
  select(UNT_9574.Truseq,UNT.9574.ClontechPico,UNT.9574.ClontechV4)
colnames(all.UNT.9574) = c("Truseq","Pico","V4")
all.UNT.9574.cor = cor(all.UNT.9574, method = "spearman")
jpeg("Plots/correlation/UNT.9574.correlation.jpg", height = 40, width = 40, units ="cm", res = 300, pointsize = 40)
corrplot(all.UNT.9574.cor, type="upper", order="hclust", method = "number", mar=c(0,0,1,0), title="UNT.9574")
dev.off()


all.UNT.9575 = all %>%
  select(UNT_9575.Truseq,UNT.9575.ClontechPico,UNT.9575.ClontechV4)
colnames(all.UNT.9575) = c("Truseq","Pico","V4")
all.UNT.9575.cor = cor(all.UNT.9575, method = "spearman")
jpeg("Plots/correlation/UNT.9575.correlation.jpg", height = 40, width = 40, units ="cm", res = 300, pointsize = 40)
corrplot(all.UNT.9575.cor, type="upper", order="hclust", method = "number", mar=c(0,0,1,0), title="UNT.9575")
dev.off()


all.UNT.9576 = all %>%
  select(UNT_9576.Truseq,UNT.9576.ClontechPico,UNT.9576.ClontechV4)
colnames(all.UNT.9576) = c("Truseq","Pico","V4")
all.UNT.9576.cor = cor(all.UNT.9576, method = "spearman")
jpeg("Plots/correlation/UNT.9576.correlation.jpg", height = 40, width = 40, units ="cm", res = 300, pointsize = 40)
corrplot(all.UNT.9576.cor, type="upper", order="hclust", method = "number", mar=c(0,0,1,0), title="UNT.9576")
dev.off()


# all
all.samples = all %>%
  select(mean.Truseq,mean.Pico,mean.V4)
colnames(all.samples) = c("Truseq","Pico","V4")
all.samples.cor = cor(all.samples, method = "spearman")
jpeg("Plots/correlation/all.samples.correlation.jpg", height = 40, width = 40, units ="cm", res = 300, pointsize = 40)
corrplot(all.samples.cor, type="upper", order="hclust", method = "number", mar=c(0,0,1,0), title="Spearman's correlation at expression level")
dev.off()
