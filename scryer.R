#! /usr/bin/env Rscript

library("reshape2")
library("ggplot2")
library("data.table")

args <- commandArgs(trailingOnly = TRUE)
# args <- c(6)
if (length(args) != 1) {
  cat("Usage: scryer <num-output>\n")
  quit(save="no", status=1)
}

n_output = args[1]

cat("Reading the lotto history...\n")
lotto = fread("data/MondayWednesdayLotto.csv", header=F)
round_count = nrow(lotto)

cat("================================\n")
cat("MondayWednesdayLotto.csv summary:\n")
cat("Number of rows: ", round_count, "\n", sep="")
molten_lotto = melt(lotto, id.vars=c("V1"),
                    measure.vars=c("V3", "V4", "V5", "V6", "V7", "V8"))
lotto_freq = molten_lotto[, list(freq = .N), by="value"]
cat("Mean occurrence: ", mean(lotto_freq$freq), "\n", sep="")
cat("Median occurrence: ", median(lotto_freq$freq), "\n", sep="")
cat("================================\n")

cat("Generating histograms...\n")
hist_plot = ggplot(molten_lotto, aes(x=as.numeric(value))) +
  geom_histogram(binwidth=0.5) +
  scale_x_continuous(breaks=unique(as.numeric(molten_lotto$value)))
output_filename = "data/MondayWednesdayLotto_Histogram.pdf"
ggsave(hist_plot, file=output_filename, width=12, height=6)
cat("Histogram saved in \"", output_filename, "\"\n", sep="")

sample_beta = function(lotto_freq, round_count) {
  lotto_beta = lotto_freq[, list(value=value, alpha=freq + 1,
                                 beta=round_count - freq + 1)]
  scores = runif(nrow(lotto_beta))
  tmp_lotto_beta = lotto_beta
  tmp_lotto_beta$scores = scores
  lotto_score = tmp_lotto_beta[, list(value=value,
                                      prob=qbeta(scores, alpha, beta))]
  setkey(lotto_score, prob)
  # tail(lotto_score, 6)$value
  sample(lotto_score$value, 6, prob=lotto_score$prob)
}

cat("\n")
cat("Predicted Wining Numbers:\n")
cat("============================================\n")
for (i in 1:n_output) {
  selection = sample_beta(lotto_freq, round_count)
  cat(selection, sep=",\t")
  cat("\n")
}
cat("============================================\n")

cat("Good Luck!\n")
