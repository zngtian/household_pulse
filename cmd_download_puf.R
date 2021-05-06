source("cmd_initiate.R")

## Downloading ----------------------------------------------------------------
## Change oldweek from NULL to newweek
## and newweek to NULL when finishing updating
neweeks <- NULL
oldweeks <- 1:27

## increase timeout if needed
options(timeout = 150)
dd <- "input_data/pulse/sas"
if (!dir.exists(dd)) dir.create(dd)
map(neweeks,
    function(wk) {
      ## define the downloading url
      yy <- if (wk <= 21) 2020 else 2021
      f1 <- sprintf(
        "https://www2.census.gov/programs-surveys/demo/datasets/hhp/%d/wk%d/HPS_Week%02d_PUF_SAS.zip",
        yy, wk, wk)
      f2 <- str_c("week", wk, ".zip")
      download.file(f1, destfile = file.path(dd, f2))
    })


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
## define the new file name
f.out <- file.path("output_data", sprintf("hps_individual_week%d_%d.rds", 1, max(allweeks)))
saveRDS(df.puf, f.out)

## f.out <- file.path("output_data", sprintf("hps_individual_week%d_%d.csv", 1, max(allweeks)))
## write_csv(df.puf, path = f.out)
