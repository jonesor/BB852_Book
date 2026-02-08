# Spell check script
library(spelling)

rmd_files <- list.files(pattern = "*.Rmd", full.names = TRUE)

wordlist <- readLines("spellCheck_wordlist.txt")


(x <- spelling::spell_check_files(
  path = rmd_files,
  ignore = wordlist,
  lang = "en_GB"
))
writeLines(x[["word"]], "spellcheckOut.csv")
