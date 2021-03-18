select_tidy_puf <- function(df.puf, vars.use = NULL, vars.selstr = NULL,
                            vars.meta = c("scram", "week", "est_st", "pweight"),
                            exclude.18y = TRUE,
                            long.format = TRUE) {

    ## exclude people whose age < 18
    if (exclude.18y) {
        df.puf <- df.puf %>%
            filter((week <= 21 & tbirth_year <= 2002) | (week > 21 & tbirth_year <= 2003))
    }

    if (!is.null(vars.use)) {
        df <- df.puf %>%
            select(all_of(c(vars.meta, vars.use)))
    }

    if (!is.null(vars.selstr)) {
        sel.expr <- rlang::parse_expr(vars.selstr)
        df <- df.puf %>%
            select(all_of(vars.meta), {{ sel.expr }})
    }

    if (long.format) {
        ## convert the long format for easy aggregation
        df <- df %>%
            pivot_longer(cols = -any_of(vars.meta), names_to = "vars", values_to = "value")
    }

    return(df)
}


compute_value_count <- function(df.puf.lg, grp.vars = c("week", "est_st", "vars", "value")) {

  ## compute count
  df.agg <- df.puf.lg %>%
    group_by(across({{ grp.vars }})) %>%
    ## pweight is the weight to assign to each respondent;
    ## the sum of pweight is an estimate of total count in population.
    summarise(count = sum(pweight, na.rm = TRUE)) %>%
    ungroup()

  return(df.agg)
}


remove_missing <- function(data, var) {
  data %>% filter(!.data[[var]] %in% c(-88, -99))
}


aggregate_state_count_nocrosstab <- function(df.puf, vars.use) {

    df.use <- select_tidy_puf(df.puf, vars.use, long.format = TRUE)

    df.st <- compute_value_count(df.use,
                                 grp.vars = c("week", "est_st", "vars", "value"))

    df.us <- compute_value_count(df.use,
                                 grp.vars = c("week", "vars", "value")) %>%
        mutate(est_st = "99")

    df.st <- rbind(df.st, df.us)

    return(list(data = df.st %>% filter(!value %in% c(-99, -88)),
                noresp = df.st %>% filter(value %in% c(-99, -88))))

}


aggregate_state_count_crosstab <- function(df.puf, vars.use) {

    df.use <- select_tidy_puf(df.puf, vars.use = vars.use,
                              long.format = FALSE)

    s <- str_c(vars.use, " %in% c(-99, -88)")
    s <- str_c(s, collapse = " | ")
    ee <- rlang::parse_expr(s)

    df.nonresp <- df.use %>%
        filter(!! ee)

    df.use <- df.use %>%
        filter(!(!! ee))

    df.st <- df.use %>%
        compute_value_count(grp.vars = c("week", "est_st", vars.use))

    df.us <- df.use %>%
        compute_value_count(grp.vars = c("week", vars.use)) %>%
        mutate(est_st = "99")

    df.st <- rbind(df.st, df.us)

    return(list(data = df.st,
                noresp = df.nonresp))
}


gen_crosstab_vars <- function(df, vars.cross) {
    s <- map(vars.cross, function(x) str_c(x, df[[x]], sep = "_"))
    s <- pmap_chr(s, function(...) str_c(..., sep = "_"))
    df$vars <- s
    return(df)
}


## ##' This function is to compute the ratio variables, for example, the percentage
## ##' of respondents who answered (3) and (4) for the question of during the last
## ##' 7 days, did you feel anxious.
## ##'
## ##' @title Compute combined rate
## ##' @param df.st data frame. State level data of count of each survey item.
## ##' @param var string. The variable name of interest.
## ##' @param val vector of integre. The integer for answer items
## ##' @param newvar string. The new variable name.
## ##' @return a new data frame with both count and ratio variables.
## ##' @author Zheng Tian
## compute_combined_rate <- function(df.st, var, val, newvar) {
##   df.st %>%
##     filter(vars == var, value %in% val) %>%
##     group_by(week, est_st) %>%
##     summarise(
##       count = sum(count, na.rm = TRUE),
##       rate = sum(rate, na.rm = TRUE)) %>%
##     rename(!! sym(str_c(newvar, ".rate")) := rate,
##            !! sym(newvar) := count) %>%
##     ungroup()
## }
