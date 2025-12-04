#' Calculate and make table of descriptive stats (mean & SD)
#'
#' @param data = lipidomics
#'
#' @returns df/tibble

create_table_descriptive_stats <- function(data) {
  data |>
    dplyr::group_by(metabolite) |>
    dplyr::summarize(dplyr::across(value, list(mean = mean, sd = sd))) |>
    dplyr::mutate(dplyr::across(tidyselect::where(is.numeric), \(x) round(x, digits = 1))) |>
    dplyr::mutate(MeanSD = glue::glue("{value_mean} ({value_sd})")) |>
    dplyr::select(Metabolite = metabolite, "Mean SD" = MeanSD)
}

#' Making distibution plots for dataset
#'
#' @param df, lipidomics
#'
#' @returns histogram plot objects

create_plot_distributions <- function(data) {
  data |>
    ggplot2::ggplot(ggplot2::aes(x = value)) +
    ggplot2::geom_histogram() +
    ggplot2::facet_wrap(ggplot2::vars(metabolite), scales = "free") +
    ggplot2::theme_minimal()
}

#' Clean data to summarize repeat values
#' Keep the mean value for duplicate values when all other characteristics match
#'
#' @param data , lipidomics df
#'
#' @returns cleaned df

clean <- function(data) {
  data |>
    dplyr::group_by(dplyr::pick(-value)) |>
    dplyr::summarise(value = mean(value), .groups = "keep") |>
    dplyr::ungroup()
}

#' Preprocessing of the data
#' changes metabolites to a class variable and scaling the metabolite groups
#' @param data, lipidomics df
#'
#' @returns cleaned df (lipidomics)

preprocess <- function(data) {
  data |>
    dplyr::mutate(
      class = as.factor(class),
      value = scale(value) # so that it scales cholesterol to be compared to future metabolites
    )
}

#' Fitting a model function
#'
#' @param data, df (lipidomics)
#' @param model, pre-defined model (x ~  y)
#'
#' @returns table of model results

fit_model <- function(data, model) {
  stats::glm(
    formula = model,
    data = data,
    family = binomial()
  ) |>
    broom::tidy(exponentiate = TRUE) |> # model results into a more readable format using the broom::tidy function
    dplyr::mutate(
      metabolite = unique(data$metabolite),
      model = format(model),
      .before = dplyr::everything()
    )
}


#' Generating model results
#'
#' @param data df, lipidomics
#'
#' @returns model results, a tibble

create_model_results <- function(data) {
  data |>
    dplyr::filter(metabolite == "Cholesterol") |>
    preprocess() |> # calling our previous function
    fit_model(class ~ value) # calling our previous function
}

#' Fitting models to a general data frame
#'
#' @param data, lipidomics that has been filtered
#'
#' @returns model results in df

fit_all_models <- function(data) {
  list(
    class ~ value,
    class ~ value + gender + age
  ) |> # usint purr in map is a little bit confusing, explained more in website
    purrr::map(\(model) fit_model(data, model = model)) |> # \()is the shortcut for an annonymous function
    purrr::list_rbind()
}
