install.packages("quanteda")
install.packages("readtext")
install.packages("stringr")
install.packages("quanteda.textstats")
install.packages("quanteda.textplots")
library(quanteda)
library(stringr)
library(quanteda.textstats)
library(quanteda.textplots)
# Load the text "Project Gutenberg eBook of The Money Market" from the Project Gutenberg website (https://www.gutenberg.org/ebooks/61605).
data_char_moneymarket <- as.character(readtext::readtext("https://www.gutenberg.org/cache/epub/61605/pg61605.txt"))

# Extract the 1st through 100th characters of the text.
library("stringi")
stri_sub(data_char_moneymarket, 1, 100)

# Find the main text of the article, and find the indices of the start and end.
(start <- stri_locate_first_fixed(data_char_moneymarket, "CHAPTER I
    THE BEGINNING OF BANKING IN ENGLAND")[1]) #[1] means the location of the beginning of this item.
(end <- stri_locate_first_fixed(data_char_moneymarket, "=MINING AND MINING INVESTMENTS.= By A. MOIL.")[2])
#[2] means the location of the end of this item.

# Extract the main text from the article, and inspect the first 200 characters.
mt <- stri_sub(data_char_moneymarket, start, end)
stri_sub(mt, 1, 200) |> cat()

# Convert all letters in the text to lower case, and remove all punctuation. Finally, create a document
# frequency matrix.
mt_dfm <- mt |>
  tokens() |>
  tokens_tolower() |> #to lower case
  tokens(remove_punct = TRUE) |> #remove punctuation
  dfm()

# Find the most frequently used tokens.
topfeatures(mt_dfm) #The most frequently used token is "the".

# Use the 100 most frequently used tokens to plot a wordcloud.
png(filename = "wordcloud_mt.png")
set.seed(100)
wordcloud_mt <- textplot_wordcloud(mt_dfm, min_size = 1, max_size = 5, max_words = 100, color = "#AA2219")
dev.off()

# Weight the dfm.
mt_dfm_w <- dfm_weight(mt_dfm, scheme = "prop") * 100

# Plot the 10 most frequent terms using ggplot2.
install.packages("ggplot2")

png(filename = "ggplot_mt.png", width = 1000, height = 600)

library(ggplot2)
textstat_frequency(mt_dfm_w, n = 10) |>
  ggplot(aes(x = reorder(feature, -rank), y = frequency)) +
  geom_bar(stat = "identity", fill = "#01AADE") + coord_flip() + 
  labs(x = "", y = "Term Frequency as a Percentage")

dev.off()

# Produce dispersion plots to analyse the distributions of tokens "bank" and "stock".
mt_toks <- mt |>
  tokens()

png(filename = "dispersion_plot_mt.png", width = 1000, height = 500)

textplot_xray(kwic(mt_toks, pattern = "bank"),
              kwic(mt_toks, pattern = "stock")) +
  ggtitle("Lexical dispersion")

dev.off()







