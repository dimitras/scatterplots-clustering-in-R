library(ggplot2)
library(dplyr)
library(tidyr)

# histograms for counts of NM values
setwd("~/Documents/dimitra/Workspace/RNA-Seq/Kits_comparison/Plots/")
fdata <- read.table("fwd.NMs.txt")
fdata$group <- 'FWD'
fdata.w.perc = fdata %>%  
  mutate(Percent = V1/sum(V1))
fdata.w.perc %>% 
  ggplot(aes(x=V1)) + 
  geom_histogram(binwidth=.5, position = "identity", bins = 100) +
  geom_density(position = "stack") +
  theme_bw(base_size = 30) +
  xlab("NM values for FWD") +
  coord_cartesian(xlim=c(0,150), ylim = c(0,1.5e+07))
ggsave("fwd.mismatches.jpg", height = 50, width = 50, units ="cm")


rdata <- read.table("rev.NMs.txt")
rdata$group <- 'REV'
rdata.w.perc = rdata %>%
  mutate(Percent = V1/sum(V1))
rdata.w.perc %>%
  ggplot(aes(x=V1)) + 
  geom_histogram(binwidth=.5, position = "identity", bins = 100) +
  geom_density(position = "stack") +
  theme_bw(base_size = 30) +
  xlab("NM values for REV") +
  coord_cartesian(xlim=c(0,150), ylim = c(0,1.5e+07))
ggsave("rev.mismatches.jpg", height = 50, width = 50, units ="cm")


# plot the percentages of the counts (normalize)
# plot the density
library(plotly)
library(scales)
cdata = rbind(fdata,rdata)

# bw = function(b,x){b/bw.nrd0(x)}
# p = 
  ggplot(cdata, aes(x=V1, fill=group, colour = group)) + 
  geom_bar(aes(y=..count../sum(..count..)), position = "dodge") +
  scale_y_continuous(labels=scales::percent) +
  labs(y="Count percent") +
  # geom_density(alpha=0.2, lwd=1, adjust=bw(.5, cdata$V1)) +
  # geom_density(alpha=0.2, lwd=1, bw=1) +
  theme_bw(base_size = 30) +
  xlab("NM values")  
  # coord_cartesian(xlim=c(0,50)) #to crop the tail
# ggplotly(p)

ggsave("mismatches.distributions_bw0.5.jpg", height = 40, width = 40, units ="cm")
ggsave("mismatches.distributions_cropped.jpg", height = 40, width = 40, units ="cm")
ggsave("mismatches.distributions.jpg", height = 40, width = 40, units ="cm")
ggsave("mismatches.percent.bars.dodge.jpg", height = 40, width = 40, units ="cm")




# plot the differences of the percentages
library(data.table)
library(DT)

cdata.dt = data.table(cdata)
cdata.dt.formatted = cdata.dt[,.(count=length(V1)),by=.(V2,group)]
cdata.dt.formatted[, norm_count := count/sum(count), by = .(group)]

# melt(cdata.dt.formatted, id=V2, measure=c["group","norm_count"])
# setkey(cdata.dt.formatted, x, y)
# cdata.dt.formatted[CJ(unique(x), unique(y))]

cdata = rbind(fdata.w.perc,rdata.w.perc)
cdata %>%
  group_by(V1,group) %>%
  spread(key=V1, value = Percent) %>%
  mutate(Difference = )
ggplot(cdata, aes(x=V1, fill=group, colour = group)) + 
  geom_bar(aes(y=..count../sum(..count..)), position = "dodge") +
  scale_y_continuous(labels=scales::percent) +
  theme_bw(base_size = 30) +
  ylab("Count percent") +
  xlab("NM values")  

ggsave("mismatches.differences.jpg", height = 40, width = 40, units ="cm")
# write.table(cdata, file="NM_values.txt", sep="\t")


##################################################################
# plot the differences of the percentages of the normalized counts
setwd("~/Documents/dimitra/Workspace/RNA-Seq/Kits_comparison/Plots/")

fdata <- read.table("fwd.NM.dist.txt")
colnames(fdata) = c('NM_fwd','count_fwd')
total_count_fwd = sum(fdata$count_fwd)
fdata$norm_count_fwd = fdata$count_fwd/total_count_fwd

rdata <- read.table("rev.NM.dist.txt")
colnames(rdata) = c('NM_rev','count_rev')
total_count_rev = sum(rdata$count_rev)
rdata$norm_count_rev = rdata$count_rev/total_count_rev

cdata = merge(rdata, fdata, by.x = "NM_rev", by.y = "NM_fwd", sort = TRUE)
cdata$norm_count_diff = cdata$norm_count_rev - cdata$norm_count_fwd
cdata_new = cdata[-1,] # remove the NM=0 -no mismatches
sum_of_diff = sum(cdata_new$norm_count_diff)
 
ggplot(cdata_new, aes(x=NM_rev)) + 
geom_bar(aes(y=norm_count_diff), fill = "darkgreen", color="darkgreen", stat = "identity") +
scale_y_continuous(labels=scales::percent) +
theme_bw(base_size = 30) +
ylab("Normalized counts differences % (rev-fwd)") +
xlab("NM values") +
annotate("text", x=85, y=0.041, label=paste("Sum of differences = ", round(sum_of_diff,7)), size = 10, color = "darkgreen")

ggsave("mismatches.differences.jpg", height = 40, width = 40, units ="cm")
