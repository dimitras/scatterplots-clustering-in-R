setwd("~/Dropbox/Kits/")

#Pico
data=read.delim("ClontechPico.Liver.ILB.vs.UNT.w_LimmaVoom_DEG_results.txt")
head(data)
d = as.matrix(data[,2:7]) #number of columns with the samples
rownames(d) = data$id #keep the gene names as row names

hc <- hclust(dist(t(d)))
png("ClontechPico.eucl_dist.png")
plot(hc, hang=-1, main="ClontechPico.Euclidian distance", xlab = "")
dev.off()

png("ClontechPico.cor_dist.png")
dd=as.dist((1-cor(d)) / 2)  # correlation-based distance
hc <- hclust(dd)
plot(hc, hang=-1, main="ClontechPico.Correlation distance", xlab = "")
dev.off()


#V4
data=read.delim("ClontechV4.Liver.ILB.vs.UNT.w_LimmaVoom_DEG_results.txt")
head(data)
d = as.matrix(data[,2:7]) #number of columns with the samples
rownames(d) = data$id #keep the gene names as row names

hc <- hclust(dist(t(d)))
png("ClontechV4.eucl_dist.png")
plot(hc, hang=-1, main="ClontechV4.Euclidian distance", xlab = "")
dev.off()

png("ClontechV4.cor_dist.png")
dd=as.dist((1-cor(d)) / 2)  # correlation-based distance
hc <- hclust(dd)
plot(hc, hang=-1, main="ClontechV4.Correlation distance", xlab = "")
dev.off()


#Truseq
data=read.delim("Truseq.Liver.ILB.vs.UNT.w_LimmaVoom_DEG_results.txt")
head(data)
d = as.matrix(data[,2:7]) #number of columns with the samples
rownames(d) = data$id #keep the gene names as row names

hc <- hclust(dist(t(d)))
png("Truseq.eucl_dist.png")
plot(hc, hang=-1, main="Truseq.Euclidian distance", xlab = "")
dev.off()

png("Truseq.cor_dist.png")
dd=as.dist((1-cor(d)) / 2)  # correlation-based distance
hc <- hclust(dd)
plot(hc, hang=-1, main="Truseq.Correlation distance", xlab = "")
dev.off()


#Pico.vs.V4
data=read.delim("Pico.V4.commons.txt")
d = as.matrix(data[,2:13]) #number of columns with the samples
rownames(d) = data$id #keep the gene names as row names

hc <- hclust(dist(t(d)))
png("Pico.V4.eucl_dist.png")
plot(hc, hang=-1, main="ClontechPico-ClontechV4 Euclidian distance", xlab = "")
dev.off()

png("Pico.V4.cor_dist.png")
dd=as.dist((1-cor(d)) / 2)  # correlation-based distance
hc <- hclust(dd)
plot(hc, hang=-1, main="ClontechPico-ClontechV4 Correlation distance", xlab = "")
dev.off()


#Pico vs Truseq
data=read.delim("Pico.Truseq.commons.txt")
d = as.matrix(data[,2:13])
rownames(d) = data$id

hc <- hclust(dist(t(d)))
png("Pico.Truseq.eucl_dist.png")
plot(hc, hang=-1, main="ClontechPico-Truseq Euclidian distance", xlab = "")
dev.off()

png("Pico.Truseq.cor_dist.png")
dd=as.dist((1-cor(d)) / 2)  # correlation-based distance
hc <- hclust(dd)
plot(hc, hang=-1, main="ClontechPico-Truseq Correlation distance", xlab = "")
dev.off()


#V4 vs Truseq
data=read.delim("V4.Truseq.commons.txt")
d = as.matrix(data[,2:13])
rownames(d) = data$id

hc <- hclust(dist(t(d)))
png("V4.Truseq.eucl_dist.png")
plot(hc, hang=-1, main="ClontechV4-Truseq Euclidian distance", xlab = "")
dev.off()

png("V4.Truseq.cor_dist.png")
dd=as.dist((1-cor(d)) / 2)  # correlation-based distance
hc <- hclust(dd)
plot(hc, hang=-1, main="ClontechV4-Truseq Correlation distance", xlab = "")
dev.off()


#Truseq.V4.Pico
setwd("~/Documents/dimitra/Workspace/RNA-Seq/Kits_comparison/")
data=read.delim("DEG_lists/commons/Truseq.V4.Pico.commons.txt")
d = as.matrix(data[,2:19])
rownames(d) = data$id

hc <- hclust(dist(t(d)))
png("Truseq.V4.Pico.eucl_dist.png")
plot(hc, hang=-1, main="ClontechPico-ClontechV4-Truseq Euclidian distance", xlab = "")
dev.off()

ble = rgb(56,111,176, maxColorValue = 255)
kokkino = rgb(183,58,62, maxColorValue = 255)
prasino = rgb(138,180,62, maxColorValue = 255)

colorCodes = c(ILB.9579.P = ble,
              ILB.9582.P = ble,
              ILB.9583.P = ble,
              UNT.9574.P = ble,
              UNT.9575.P = ble,
              UNT.9576.P = ble,
              ILB_9579.T = prasino,
              ILB_9582.T = prasino,
              ILB_9583.T = prasino,
              UNT_9574.T = prasino,
              UNT_9575.T = prasino,
              UNT_9576.T = prasino,
              ILB.9579.V = kokkino,
              ILB.9582.V = kokkino,
              ILB.9583.V = kokkino,
              UNT.9574.V = kokkino,
              UNT.9575.V = kokkino,
              UNT.9576.V = kokkino)

labelCol <- function(x) {
  if (is.leaf(x)) {
    ## fetch label
    label <- attr(x, "label")
    # code <- substr(label, 1, 1)
    ## use the following line to reset the label to one letter code
    # attr(x, "label") <- code
    attr(x, "nodePar") <- list(lab.col=colorCodes[label])
  }
  return(x)
}

par(mar=c(50,0,0,0))
jpeg("HC/Truseq.V4.Pico.cor_dist.jpg", width=1500, height=1500, units="px", pointsize = 50)
dd=as.dist((1-cor(d)) / 2)  # correlation-based distance
hc <- hclust(dd)
da <- dendrapply(as.dendrogram(hc), labelCol)
plot(da, main="Pico-V4-Truseq Correlation Distance")
legend('topright', c("Pico","V4","Truseq"), col = c(ble,kokkino,prasino), pch = 16, bty='n')
dev.off()


#BRAIN
setwd("~/Dropbox/workspace/Brain/")
data=read.delim("DE.w.limma.voom/Sleep.vs.SleepDep.w_LimmaVoom_DEG_results.txt")
head(data)
d = as.matrix(data[,2:53]) #number of columns with the samples
rownames(d) = data$id #keep the gene names as row names

ble = rgb(56,111,176, maxColorValue = 255)
kokkino = rgb(183,58,62, maxColorValue = 255)
prasino = rgb(138,180,62, maxColorValue = 255)

colorCodes = c(B2.0hr = prasino,
               B3.SS3hr = ble,
               B4.SS6hr = ble,
               B5.SS9hr = ble,
               B6.SS12hr = ble,
               B7.SD3hr = kokkino,
               B8.SD6hr = kokkino,
               B9.SD9hr = kokkino,
               B10.SD12hr = kokkino,
               B11.0hr = prasino,
               C2.SS3hr = ble,
               C3.SS6hr = ble,
               C4.SS9hr = ble,
               C5.SS12hr = ble,
               C6.SD3hr = kokkino,
               C7.SD6hr = kokkino,
               C8.SD9hr = kokkino,
               C9.SD12hr = kokkino,
               C10.0hr = prasino,
               C11.SS3hr = ble,
               D2.SS6hr = ble,
               D3.SS9hr = ble,
               D4.SS12hr = ble,
               D5.SD3hr = kokkino,
               D6.SD6hr = kokkino,
               D7.SD9hr = kokkino,
               D9.0hr = prasino,
               D10.SS3hr = ble,
               D11.SS6hr = ble,
               E2.SS9hr = ble,
               E3.SS12hr = ble,
               E4.SD3hr = kokkino,
               E5.SD6hr = kokkino,
               E6.SD9hr = kokkino,
               E7.SD12hr = kokkino,
               E8.0hr = prasino,
               E9.SS3hr = ble,
               E10.SS6hr = ble,
               F2.SS12hr = ble,
               F3.SD3hr = kokkino,
               F4.SD6hr = kokkino,
               F5.SD9hr = kokkino,
               F6.SD12hr = kokkino,
               F7.0hr = prasino,
               F8.SS3hr = ble,
               F9.SS6hr = ble,
               F10.SS9hr = ble,
               F11.SS12hr = ble,
               G2.SD3hr = kokkino,
               G3.SD6hr = kokkino,
               G4.SD9hr = kokkino,
               G5.SD12hr = kokkino)

labelCol <- function(x) {
  if (is.leaf(x)) {
    label <- attr(x, "label")
    attr(x, "nodePar") <- list(lab.col=colorCodes[label])
  }
  return(x)
}

jpeg("hclust.cor_dist.jpg", width=1600, height=1000, units="px", pointsize = 30)
dd=as.dist((1-cor(d)) / 2)  # correlation-based distance
hc <- hclust(dd)
da <- dendrapply(as.dendrogram(hc), labelCol)
plot(da, main="Hierarchical clustering with correlation distance")
legend('topright', c("SS","SD","Baseline"), col = c(ble,kokkino,prasino), pch = 16, bty='n')
dev.off()




