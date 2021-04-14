source("cmd_initiate.R")

## Change oldweek from NULL to newweek
## and newweek to NULL when finishing updating
neweeks <- 26:27
oldweeks <- 1:25

dd <- "input_data/pulse"
if (!dir.exists(dd)) dir.create(dd)
map(neweeks,
    function(wk) {
      yy <- if (wk <= 21) 2020 else 2021
      f1 <- sprintf(
        "https://www2.census.gov/programs-surveys/demo/datasets/hhp/%d/wk%d/HPS_Week%02d_PUF_CSV.zip",
        yy, wk, wk)
      f2 <- str_c("week", wk, ".zip")
      download.file(f1, destfile = file.path(dd, f2))
    })


## read data
allweeks <- c(oldweeks, neweeks)
df.puf <- map_dfr(
  allweeks,
  function(wk) {
    yy <- if (wk <= 21) 2020 else 2021
    f.zip <- file.path(dd, sprintf("week%d.zip", wk))
    f.in <- sprintf("pulse%d_puf_%02d.csv", yy, wk)
    unzip(zipfile = f.zip,
          files = f.in,
          exdir = dd)
    f.tmp <- file.path(dd, f.in)
    df <- read_csv(f.tmp)
    file.remove(f.tmp)
    return(df)
  })


names(df.puf) <- str_to_lower(names(df.puf))
f.out <- file.path("output_data", sprintf("hps_individual_week%d_%d.rds", 1, max(allweeks)))
saveRDS(df.puf, f.out)

## f.out <- file.path("output_data", sprintf("hps_individual_week%d_%d.rds", 1, max(allweeks)))
## write_csv(df.puf, path = f.out)
