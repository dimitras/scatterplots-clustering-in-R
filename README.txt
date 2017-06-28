Compare the efficiency of different kits (amplified stranded Pico, amplified unstranded V4, stranded Truseq).

- Perform hierarchical clustering to check whether there is a distinction between kits, conditions, samples, etc.
- Scatterplot qvalues, fold changes, means, variances across kits, across conditions and kits, among each kit. (pairwise comparisons)
- Filter the DEGs lists:
    - genes that have high pvalue in one kit and very low in the other kit (ranges defined by looking at the log-qvalues scatterplots)
    - genes that are highly expressed in one kit only and not expressed in the others kits
    - genes that are highly expressed in two of the kits and not expressed in the third one


hierarchical.clustering.R
scatterplot.comparisons.R
find.genes.in.qvalue.ranges.rb
find.genes.activated.in.1.condition.only.rb
find.genes.activated.in.1.condition.only.w.min_max.rb
find.genes.deactivated.in.1.condition.only.w.min_max.rb
