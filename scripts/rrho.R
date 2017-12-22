source("https://bioconductor.org/biocLite.R")
biocLite("RRHO")
library(RRHO)

### Graph raw counts
setwd("~/Documents/dimitra/Workspace/RNA-Seq/Kits_comparison/")

pico = read.delim("gene_lists_filtered/Pico.counts.ILB.9579.txt")
v4 = read.delim("gene_lists_filtered/V4.counts.ILB.9579.txt")
truseq = read.delim("gene_lists_filtered/Truseq.counts.ILB.9579.txt")

#Pico vs V4
RRHO.pico.v4 <- RRHO(pico, v4, alternative='enrichment') #alternative='two.sided'
jpeg("RRHO.plots/RRHO.pico.v4.jpg")
image(RRHO.pico.v4$hypermat)
dev.off()
pval.pico.v4 <- pvalRRHO(RRHO.pico.v4,50)

# Pico vs Truseq
RRHO.pico.truseq <- RRHO(pico, truseq, alternative='enrichment')
jpeg("RRHO.plots/RRHO.pico.truseq.jpg")
image(RRHO.pico.truseq$hypermat)
dev.off()
pval.pico.truseq <- pvalRRHO(RRHO.pico.truseq,50)

# V4 vs Truseq
RRHO.v4.truseq <- RRHO(v4, truseq, alternative='enrichment')
jpeg("RRHO.plots/RRHO.v4.truseq.jpg")
image(RRHO.v4.truseq$hypermat)
dev.off()
pval.v4.truseq <- pvalRRHO(RRHO.v4.truseq,50)

#To determine if the overlap between pico and v4 is statistically significant different from the overlap between pico and truseq
rrho.comp = RRHOComparison(v4, pico, truseq, stepsize=10, labels=c("v4","pico","truseq"), plots=TRUE, outputdir="RRHO.plots")


### Graph FCs
all = read.delim("gene_lists_filtered/common.genes.full.info.txt")
colnames(all)

fcs = all %>%
  select(id, geneSymbol.Truseq, geneCoordinate.Truseq, logFC.Truseq, logFC.ClontechPico, logFC.ClontechV4)

pico.fc = fcs %>%
  select(id, logFC.ClontechPico)
v4.fc = fcs %>%
  select(id, logFC.ClontechV4)
truseq.fc = fcs %>%
  select(id, logFC.Truseq)

#Pico vs V4
RRHO.fc.pico.v4 <- RRHO(pico.fc, v4.fc, alternative='enrichment')
jpeg("RRHO.plots/RRHO.fc.pico.v4.jpg")
image(RRHO.fc.pico.v4$hypermat)
dev.off()
pval.fc.pico.v4 <- pvalRRHO(RRHO.fc.pico.v4,50)
#Pico vs Truseq
RRHO.fc.pico.truseq <- RRHO(pico.fc, truseq.fc, alternative='enrichment')
jpeg("RRHO.plots/RRHO.fc.pico.truseq.jpg")
image(RRHO.fc.pico.truseq$hypermat)
dev.off()
pval.fc.pico.truseq <- pvalRRHO(RRHO.fc.pico.truseq,50)
#V4 vs Truseq
RRHO.fc.v4.truseq <- RRHO(v4.fc, truseq.fc, alternative='enrichment')
jpeg("RRHO.plots/RRHO.fc.v4.truseq.jpg")
image(RRHO.fc.v4.truseq$hypermat)
dev.off()
pval.fc.v4.truseq <- pvalRRHO(RRHO.fc.v4.truseq,50)

rrho.comparison.fc = RRHOComparison(v4.fc, pico.fc, truseq.fc, stepsize=10, labels=c("v4","pico","truseq"), plots=TRUE, outputdir="RRHO.plots")


### Graph Pvalues
all = read.delim("gene_lists_filtered/common.genes.full.info.txt")

pvalues = all %>%
  select(id, geneSymbol.Truseq, geneCoordinate.Truseq, LimmaVoom.pvalue.Truseq, LimmaVoom.pvalue.ClontechPico, LimmaVoom.pvalue.ClontechV4)

pico.pv = pvalues %>%
  select(id, LimmaVoom.pvalue.ClontechPico)
v4.pv = pvalues %>%
  select(id, LimmaVoom.pvalue.ClontechV4)
truseq.pv = pvalues %>%
  select(id, LimmaVoom.pvalue.Truseq)

#Pico vs V4
RRHO.pvalues.pico.v4 <- RRHO(pico.pv, v4.pv, alternative='enrichment')
jpeg("RRHO.plots/RRHO.pvalues.pico.v4.jpg")
image(RRHO.pvalues.pico.v4$hypermat)
dev.off()
pval.pvalues.pico.v4 <- pvalRRHO(RRHO.pvalues.pico.v4,50)
#Pico vs Truseq
RRHO.pvalues.pico.truseq <- RRHO(pico.pv, truseq.pv, alternative='enrichment')
jpeg("RRHO.plots/RRHO.pvalues.pico.truseq.jpg")
image(RRHO.pvalues.pico.truseq$hypermat)
dev.off()
pval.pvalues.pico.truseq <- pvalRRHO(RRHO.pvalues.pico.truseq,50)
#V4 vs Truseq
RRHO.pvalues.v4.truseq <- RRHO(v4.pv, truseq.pv, alternative='enrichment')
jpeg("RRHO.plots/RRHO.pvalues.v4.truseq.jpg")
image(RRHO.pvalues.v4.truseq$hypermat)
dev.off()
pval.pvalues.v4.truseq <- pvalRRHO(RRHO.pvalues.v4.truseq,50)

rrho.comparison.pvalues = RRHOComparison(v4.pv, pico.pv, truseq.pv, stepsize=10, labels=c("v4","pico","truseq"), plots=TRUE, outputdir="RRHO.plots")
