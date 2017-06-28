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
data=read.delim("Truseq.V4.Pico.commons.txt")
d = as.matrix(data[,2:19])
rownames(d) = data$id

hc <- hclust(dist(t(d)))
png("Truseq.V4.Pico.eucl_dist.png")
plot(hc, hang=-1, main="ClontechPico-ClontechV4-Truseq Euclidian distance", xlab = "")
dev.off()

png("Truseq.V4.Pico.cor_dist.png")
dd=as.dist((1-cor(d)) / 2)  # correlation-based distance
hc <- hclust(dd)
plot(hc, hang=-1, main="ClontechPico-ClontechV4-Truseq Correlation distance", xlab = "")
dev.off()

