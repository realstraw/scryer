#! /usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
# args <- c(6)
if (length(args) != 5) {
  cat("Usage: scryer <lotto> <output-dir> <num-output> <ball-count> <model>\n")
  quit(save="no", status=1)
}

library("reshape2")
library("ggplot2")
library("data.table")

lotto_name = args[1]
output_dir = args[2]
n_output = as.numeric(args[3])
ball_count = as.numeric(args[4])
model = args[5]

cat("Reading the lotto history...\n")
if (lotto_name == "MondayWednesday") {
  lotto_filename = "data/MondayWednesdayLotto.csv"
} else if (lotto_name == "Saturday") {
  lotto_filename = "data/LottoSaturday.csv"
} else {
  cat("Lotto name must be one of \"MondayWednesday\" or \"Saturday\"\n")
  quit(save="no", status=1)
}
lotto = fread(lotto_filename, header=F)
# download.file("https://tatts.com/LottoHistoricWinningNumbers/MondayWednesdayLotto.csv",
#           lotto_filename, method="wget")
lotto = fread(lotto_filename,
              header = F)
round_count = nrow(lotto)

cat("================================\n")
cat(lotto_filename, " summary:\n", sep="")
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
dir.create(output_dir, recursive=T)
output_filename = paste(path.expand(output_dir), "/Data_Histogram.pdf",
                        sep="")
ggsave(hist_plot, file=output_filename, width=12, height=6)
cat("Histogram saved in \"", output_filename, "\"\n", sep="")

predict = function(lotto_freq, round_count, model) {
  if (model == "fair") {
    sort(as.numeric(sample(lotto_freq$value, ball_count), prob=1))
  } else if (model == "nofair") {
    sort(as.numeric(sample(lotto_freq$value, ball_count,
                           prob=lotto_freq$freq)))
  } else if (model == "beta") {
    lotto_beta = lotto_freq[, list(value=value, alpha=freq + 1,
                                   beta=round_count - freq + 1)]
    scores = runif(nrow(lotto_beta))
    tmp_lotto_beta = lotto_beta
    tmp_lotto_beta$scores = scores
    lotto_score = tmp_lotto_beta[, list(value=value,
                                        prob=qbeta(scores, alpha, beta))]
    setkey(lotto_score, prob)
    # oail(lotto_score, 6)$value
    sort(as.numeric(sample(lotto_score$value, ball_count,
                           prob=lotto_score$prob)))
  }
}

if (n_output > 1) {
  cat("\n")
  cat("Predicted Wining Numbers:\n")
  cat("============================================\n")
  for (i in 1:n_output) {
    selection = predict(lotto_freq, round_count, model)
    cat(i, ":\t", sep="")
    cat(selection, sep=",\t")
    cat("\n")
  }
  cat("============================================\n")
} else {
  cat("\n")
  cat("Testing for winning numbers:\n")
  cat("Wining number: ")
  # win_number = sort(as.numeric(tail(lotto, 1)[, c(V3, V4, V5, V6, V7, V8)]))
  win_number = sort(as.numeric(lotto[sample(nrow(lotto), 1), c(V3, V4, V5, V6, V7, V8)]))
  cat(win_number, sep=", ")
  cat("\n")
  i = 1
  highest_match = 0
  repeat {
    guess = predict(lotto_freq, round_count, model)
    highest_match = max(length(intersect(win_number, guess)), highest_match)
    if (i %% 1000 == 0) {
      cat("Current count: ", i, "\n", sep="")
      cat("Current highest match: ", highest_match, "\n", sep="")
    }
    if (highest_match >= 6) {
      cat("=======\n")
      cat("Hit Jackpot at: ", i, "\n", sep="")
      cat("The guess is: ")
      cat(guess, sep=", ")
      cat("\n")
      cat("Wining number: ")
      cat(win_number, sep=", ")
      cat("\n")
      cat("=======\n")
      break
    }
    i = i + 1
  }
}

cat("Good Luck!\n")
