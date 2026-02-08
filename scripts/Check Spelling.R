# 1) Install once (adds 'spelling' and the WORDLIST scaffold if it's a package)
#install.packages("spelling")
# usethis::use_spell_check()  # optional if this is an R package; creates inst/WORDLIST

# 2) Gather all .Rmd files (recursively)
rmd_files <- list.files(pattern = "\\.Rmd$", recursive = TRUE, full.names = TRUE)

# 3) Optional project wordlist (create 'inst/WORDLIST' or 'WORDLIST' at project root)
wordlist <- character(0)
if (file.exists("WORDLIST"))      wordlist <- c(wordlist, readLines("WORDLIST", warn = FALSE))

# 4) Run the spell check in British English
res <- spelling::spell_check_files(
  rmd_files,
  ignore = unique(wordlist),
  lang   = "en_GB"
)

# 5) View results
res
