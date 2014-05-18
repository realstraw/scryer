
library("reshape2")
library("ggplot2")

lotto = fread("data/MondayWednesdayLotto.csv", header=F)
# lotto = fread("~/Downloads/LottoSaturday.csv", header=F)
round_count = nrow(lotto)
# lotto = head(lotto, 250)
molten_lotto = melt(lotto, id.vars=c("V1"), measure.vars=c("V3", "V4", "V5", "V6", "V7", "V8"))

# ggplot(tail(molten_lotto, 10*6), aes(x=value)) + geom_histogram(binwidth=.5)
ggplot(molten_lotto, aes(x=as.numeric(value))) +
  geom_histogram(binwidth=0.5) +
  scale_x_continuous(breaks=unique(as.numeric(molten_lotto$value)))

lotto_freq = molten_lotto[, list(freq=.N), by="value"]
lotto_beta = lotto_freq[, list(value=value, alpha=freq+1, beta=round_count-freq+1)]

sample_beta = function() {
  scores = runif(nrow(lotto_beta))
  tmp_lotto_beta = lotto_beta
  tmp_lotto_beta$scores = scores
  lotto_score = tmp_lotto_beta[, list(value=value, prob=qbeta(scores, alpha, beta))]
  setkey(lotto_score, prob)
  # tail(lotto_score, 6)$value
  sample(lotto_score$value, 6, prob=lotto_score$prob)
}


length(unique(molten_lotto$value))
nrow(lotto) * (6/45)

num_freq = molten_lotto[, list(count = .N), by="value"]
num_freq

sample(num_freq$value, 6, prob=num_freq$count)
sample(num_freq$value, 6, prob=(1/num_freq$count))
sample(num_freq$value, 6)

sample_beta()

