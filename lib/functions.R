number_of_otus_samples <- function(X){
  n <- tibble(n_otus = nrow(X), n_samples = ncol(X) - 1)
  return(n)
}

get_summary <- function(X){
  otutable_summary <- X %>%
    gather(label, counts, -id) %>%
    group_by(label) %>%
    mutate(rel_abun = counts/sum(counts)) %>%
    arrange(id, label) %>%
    filter(counts > 0)
  return(otutable_summary)
}
