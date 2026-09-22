# Caches the population and endemic equilibrium state for each population size, for the endemic
# benchmark to resume from. Each burn-in simulates BURNIN_DAYS from an approximate flu equilibrium,
# in a fresh memory-capped process. Existing burn-ins are kept unless --refresh=true.
#
# Usage: Rscript bench/burnin.R [--sizes=10000,30000] [--mem-max=20G] [--refresh=true]
source("bench/common.R")

args <- parse_args(list(
  sizes = paste(DEFAULT_SIZES, collapse = ","),
  mem_max = DEFAULT_MEM_MAX,
  refresh = "false"
))

for (human_population in sort(split_numbers(args$sizes))) {
  path <- burnin_path(human_population)
  if (file.exists(path) && !as.logical(args$refresh)) {
    message("keeping burn-in for N=", human_population, ": ", path)
    next
  }
  message("burning in N=", human_population)
  status <- run_capped("bench/case.R", c("burnin", human_population, 0, ""), args$mem_max)
  # Larger populations would fail too
  if (status != "ok") break
}
