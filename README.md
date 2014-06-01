# Scryer

> Scryer can look into a crystal ball or other material to devine the paste,
> present, or future.

Scyer tells which lotto ball to choose by using a statistic model.

# Requirement

Scryer is implemented in R and requires the following R packages:

    install.packages("reshape2")
    install.packages("ggplot2")
    install.packages("data.table")

# Run

    ./scryer <lotto> <output-dir> <num-output> <ball-count> <model>
        <lotto>: either "WednesdayMonday" or "Saturday", choose which lotto
            history to use for prediction.
        <output-dir>: the output directory, which contains some descriptive 
            analysis about the lotto history.
        <num-output>: number of predictions (rows) to output. Use -1 will
            execute experiment mode, and will keep guessing until it hits the
            jackpot.
        <ball-count>: the number of numbers (columns) expected (typically 6).
        <model>: prediction models ("fair", "unfair", "beta")to use.

# Model

## Fair

Fair will randomly choose the numbers assumes all numbers are chosen with equal
probability.

## Unfair

Will use history frequency as the probability to choose the numbers.

## Beta

Use beta distribution on from the history to perform predictions (Similar to
the Bayesian Bandits problem).
