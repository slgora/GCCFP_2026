norm_taxa <- function(name) {
  name %>%
    str_replace_all("'[^']*'", "") %>%
    str_replace_all("\\b[×x]\\b", " × ") %>%
    str_replace_all("\\+", " + ") %>%
    str_replace_all("\\.", "") %>%
    str_squish() %>%
    str_split("\\s+") %>%
    map_chr(~ {
      tokens <- .x[.x != "×" & .x != "+"]
      if (length(tokens) >= 1) tokens[1] <- str_to_title(tokens[1])
      if (length(tokens) >= 2) tokens[2:length(tokens)] <- str_to_lower(tokens[2:length(tokens)])
      rank_abbrevs <- c("var", "subsp", "f", "subf", "sect", "subsect", "ser", "subser",
                        "aff", "cf", "nothovar", "nothosubsp", "nothof", "aggr",
                        "sensu", "sl", "ss", "ssp", "cv", "forma", "tribe",
                        "subtribe", "convar")
      rank_index <- which(str_to_lower(tokens) %in% rank_abbrevs)[1]
      if (!is.na(rank_index) && length(tokens) >= rank_index + 1) {
        paste(c(tokens[1:2], paste0(str_to_lower(tokens[rank_index]), "."), tokens[rank_index + 1]), collapse = " ")
      } else if (length(tokens) >= 2) {
        paste(tokens[1:2], collapse = " ")
      } else {
        NA_character_
      }
    })
}