## source("cmd_initiate.R")

## Downloading ----------------------------------------------------------------

dd <- "input_data/pulse"
if (!dir.exists(dd)) dir.create(dd, recursive = TRUE)

## This is a helper function to handle downloading error
handle_download_file <- function(f1, f2) {
  tryCatch(
    error = function(e) conditionMessage(e),
    download.file(f1, destfile = f2)
  )
}

success <- vector(length = length(neweeks))
for (i in seq_along(neweeks)) {
  ## format the url and destination file
  wk <- neweeks[i]
  yy <- if (wk <= 21) 2020 else 2021
  f1 <- sprintf(
    "https://www2.census.gov/programs-surveys/demo/datasets/hhp/%d/wk%d/HPS_Week%02d_PUF_SAS.zip",
    yy, wk, wk)
  f2 <- file.path(dd, str_c("week", wk, ".zip"))
  ## set the initial timeout
  ot <- 360
  options(timeout = ot)
  flag <- TRUE
  while (flag) {
    ## download a file
    e <- handle_download_file(f1, f2)
    ## if the downloading fails, extend the timeout with additional 360 seconds
    ## If the timeout greater than 1080 seconds, stop the downloading and go
    ## ahead with the next week.
    if (e != 0) {
      ot <- ot + 360
      if (ot > 1080) {
        success[i] <- 0
        message("Failed!")
        break
      }
      options(timeout = ot)
    } else {
      success[i] <- 1
      message("Done!")
      flag <- FALSE
    }
  }
}

## An error message for weeks not being downloaded.
if (any(success == 0)) {
  s <- glue::glue("Downloading weeks {paste0(neweeks[success == 0], collapse = ',')} failed. Manually download them.")
  stop(s, call. = FALSE)
}


## read all downloaded data for each week ------------------------------------
allweeks <- c(oldweeks, neweeks)
df.puf <- map(
  allweeks,
  function(wk) {
    ## define the file name for each week
    yy <- if (wk <= 21) 2020 else 2021
    f.zip <- file.path(dd, sprintf("week%d.zip", wk))
    f.in <- sprintf("pulse%d_puf_%02d.sas7bdat", yy, wk)
    ## unzip and read
    unzip(zipfile = f.zip,
          files = f.in,
          exdir = dd)
    f.tmp <- file.path(dd, f.in)
    df <- read_sas(f.tmp)
    file.remove(f.tmp)
    return(df)
  })

## Check variable availability in each week -----------------------------------
all.vars <- map(df.puf, names)
all.vars <- map2_dfr(all.vars, 1:length(all.vars),
                     function(x, y) tibble(vars = x, week = y))

tb.vars <- all.vars %>%
  mutate(value = "yes") %>%
  pivot_wider(id_cols = vars,
              names_from = week,
              values_from = value,
              names_prefix = "week_")
write_csv(tb.vars, "documentation/variable_available.csv")

## row-bind every data frame and save -----------------------------------------
df.puf <- bind_rows(df.puf)
## change the variable names to the lower case
names(df.puf) <- str_to_lower(names(df.puf))


if (output.format == "csv") {
  f.out <- file.path("output_data", sprintf("hps_individual_week%d_%d.csv", 1, max(allweeks)))
  write_csv(df.puf, file = f.out)
} else {
  ## define the new file name
  f.out <- file.path("output_data", sprintf("hps_individual_week%d_%d.rds", 1, max(allweeks)))
  saveRDS(df.puf, f.out)
}

