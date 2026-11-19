### _targets.R --- 
#----------------------------------------------------------------------
## Author: Sams
## Created: okt  8 2026 (13:03) 
## Version: 
## Last-Updated: nov 19 2026 (14:54) 
##           By: SADS0006
##     Update #: 251
#----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
#----------------------------------------------------------------------
## 
### Code:

pckgs <- c("targets", "tarchetypes", "data.table")
invisible(suppressWarnings(lapply(pckgs, library, character.only =  TRUE)))

targets::tar_source(
    c("c:/Users/sads0006/Desktop/rtmle/R/",
      "c:/Users/sads0006/Desktop/rTMLE-simulation-study/R/"
      ))

## targets::tar_source(
##              c("~/Desktop/Steno/rtmle/R/",
##                "~/Desktop/Steno/rTMLE-simulation-study/R/"))

#-----------------------------------------------------------------------------
#-----------------------------Helper-functions--------------------------------
#-----------------------------------------------------------------------------

simple_run <- function(setting, n, selector, alpha){
    learner <- list(
        name = paste("glmnet", selector, alpha, sep = "_"),
        fun = "learn_glmnet",
        alpha = alpha,
        selector = selector)    
    d <- do.call(
        simulate_cohort,
        c(
            list(
                n = n,
                post_baseline_visit_hook = randomize_baseline_treatment
            ),
            setting
        )
    )
    
    naive_risk <- naive_risk_est(d, 1.5, c("lira", "placebo"), "death")
    KM_risk <- KM_risk_est(d, 1.5, 0.5, c("lira", "placebo"), "death")
    rtmle_risk <- rtmle_risk_est(d, 1.5, 0.5, c("lira", "placebo"), "death", "dropout", learner)
    
    out <- data.table(
        naive = naive_risk$diff,
        KM = KM_risk$diff,
        rTMLE_lira = rtmle_risk[main_analysis.Regime == "lira"]$main_analysis.Estimate,
        rTMLE_placebo = rtmle_risk[main_analysis.Regime == "placebo"]$main_analysis.Estimate,
        rTMLE_diff = rtmle_risk[main_analysis.Regime == "lira"]$main_analysis.Estimate -
            rtmle_risk[main_analysis.Regime == "placebo"]$main_analysis.Estimate)
    
    out[]
}

#-----------------------------------------------------------------------------
#-----------------------------Main-Analysis-----------------------------------
#-----------------------------------------------------------------------------


targets::tar_option_set(packages = c("data.table", "htmlwidgets"))

list(
    
    ## tar_target(
    ##     name = fig_names,
    ##     command = {
    ##         c("reference_deviations_plot",
    ##           "boxplot00",
    ##           "rmse00",
    ##           "boxplot0ce",
    ##           "rmse0ce")
    ##     }),

    tar_target(
        name = example_cohort_minimal,
        command = {
            p <- get_minimal_setting()
            do.call(
                simulate_cohort,
                c(
                    list(
                        n = 1000,
                        post_baseline_visit_hook = randomize_baseline_treatment
                    ),
                    p
                ))}),

    
    tar_target(
        name = example_cohort_LEADER,
        command = {
            p <- get_LEADER_setting()
            # Baseline distributions
            p$parameter_values$intercept_age <- 60
            p$parameter_values$var_age <- 10
            p$parameter_values$intercept_bmi <- 32
            p$parameter_values$var_bmi <- 4
            p$parameter_values$intercept_egfr <- log(70)
            p$parameter_values$var_egfr <- 0.4
            p$parameter_values$intercept_diabetesduration <- log(10) 
            p$parameter_values$var_diabetesduration <- 0.3
            p$parameter_values$intercept_hba1c_level <- log(48)
            p$parameter_values$var_hba1c_level <- 0.4
            # Measurement distributions
            p$parameter_values$intercept_hba1c_change <- 0
            p$parameter_values$var_hba1c_change <- 10
            # Potentially add the other changing measurements.
            p$parameter_values$intercept_bmi_change <- 0
            p$parameter_values$var_bmi_change <- 1
            p$parameter_values$intercept_egfr_change <- 0
            p$parameter_values$var_egfr_change <- 10
            # Scale of events
            p$parameter_values$scale_dropout <- 0
            p$parameter_values$scale_all.cause.mortality <- 1/1000
            p$parameter_values$scale_cv.death <- 1/1000
            p$parameter_values$scale_stroke <- 1/1000
            # Visit schedule
            p$visit_schedule$sd <- 1/10
            p$visit_schedule$minimum_time_between_visits <- 1/10
            do.call(
                simulate_cohort,
                c(
                    list(
                        n = 1000,
                        post_baseline_visit_hook = randomize_baseline_treatment
                    ),
                    p
                ))}),
    
    #-------------------------------------------------------------------------
    #-------------------------Reference-Calculations--------------------------
    #-------------------------------------------------------------------------

    # Estimating reference error for different sample sizes
    tarchetypes::tar_map_rep(
                     name = reference_results,
                     command = {
                         d <- do.call(
                             simulate_cohort,
                             c(
                                 list(
                                     n = n,
                                     post_baseline_visit_hook = randomize_baseline_treatment
                                 ),
                                 simple_setting))
                         data.table::data.table(
                                         risk_est = naive_risk_est(d,1.5,c("lira", "placebo"),"death")$diff)
                     },
                     values = tibble::tibble(
                                          n = c(1000, 2000, 5000, 10000, 50000, 100000)
                                      ),
                     reps = 12
                 ),

    # Plotting deviaitons vs sample size
    tar_target(
        name = reference_deviations_plot,
        command = {
            results <- reference_results
            results[, mu := mean(risk_est), by = tar_group]
            summary <- results[
              , .(rmse = sqrt(mean((mu-risk_est)^2/mu^2))),
                by = n]
            ggplot2::ggplot(
                         summary,
                         ggplot2::aes(x = n, y = rmse)
                     ) +
                ggplot2::geom_line(
                             linewidth = 1.3,
                             color = "#2878B5"
                         ) +
                ggplot2::geom_point(
                             size = 3,
                             color = "#2878B5"
                         ) +
                ggplot2::geom_hline(
                             yintercept = 0,
                             linetype = "dashed",
                             linewidth = 0.5,
                             color = "grey60"
                         ) +
                ggplot2::labs(
                             x = "cohort size n",
                             y = "RMSE from mean",
                             title = "Deviations from the mean against increasing sample size"
                         ) +
                ggplot2::scale_x_continuous(
                             breaks = scales::breaks_width(10000),
                             labels = scales::label_comma()
                         ) +
                ggplot2::theme_minimal(base_size = 13) +
                ggplot2::theme(
                             plot.title = ggplot2::element_text(
                                                       hjust = 0.5,
                                                       face = "bold",
                                                       size = 15,
                                                       margin = ggplot2::margin(b = 15)
                                                   ),
                             axis.title = ggplot2::element_text(
                                                       face = "bold",
                                                       size = 12
                                                   ),
                             panel.grid.minor = ggplot2::element_blank(),
                             panel.grid.major.x = ggplot2::element_blank(),
                             panel.grid.major.y = ggplot2::element_line(
                                                               color = "grey90",
                                                               linewidth = 0.3
                                                           ),
                             plot.margin = ggplot2::margin(15, 20, 15, 15)
                         )}),
    
    # Reference risk estimate (for certain time could just make for multiple
    # different times later) (large cohort)
    tar_target(
        name = reference_minimal,
        command = {
            d <- do.call(
                simulate_cohort,
                c(
                    list(
                        n = 1000000,
                        post_baseline_visit_hook = randomize_baseline_treatment
                    ),
                    simple_setting
                ))
            est <- naive_risk_est(d, 1.5, c("lira", "placebo"), "death")
            data.table::data.table(
                            reference = c(est$lira,est$placebo,-est$diff),
                            estimand = c("rTMLE_lira","rTMLE_placebo","rTMLE_diff")
            )
        }),
    
    #-------------------------------------------------------------------------
    #-------------------------Minimal-simulation------------------------------
    #-------------------------------------------------------------------------

    # Defining the setting / parameter distributions
    tar_target(
        name = simple_setting,
        command = {
            p <- get_minimal_setting()
            p$parameter_values <- modifyList(
                p$parameter_values,
                list(
                    scale_death = 0.1,
                    scale_dropout = 0,
                    effect_lira_dropout = 0,
                    effect_lira_death = -1
                ))
            p[]
        }
    ),
    
    # Simulation results
    tarchetypes::tar_map_rep(
                     name = results00,
                     command = {
                         results <- simple_run(
                             simple_setting, n, selector, alpha
                         )
                     },
                     values = tidyr::expand_grid(
                                         n = c(500, 2000, 5000, 10000),
                                         selector = c("min", "1se", "undersmooth"),
                                         alpha = c(0, 0.5, 1)
                                     ),
                     reps = 4
                 ),

    ## # Graph for bias vs samplesize
    ## tar_target(
    ##     name = rmse00,
    ##     command = {
    ##         data <- data.table::as.data.table(results00)
    ##         reference <- as.numeric(reference_minimal)
    ##         summary_data <- data[,
    ##                              .(rmse = sqrt(mean((rTMLE-reference)^2))/reference,
    ##                                na.rm = TRUE),
    ##                              by = .(n, selector, alpha)]
    ##         naive_data <- data[,
    ##                            .(rmse = sqrt(mean((naive-reference)^2))/reference,
    ##                              na.rm = TRUE),
    ##                            by = .(n)]
    ##         plot <- plot_rmse(data = summary_data,
    ##                           x = "n",
    ##                           y = "rmse",
    ##                           title = "Relative RMSE of estimated treatment effects",
    ##                           subtitle = "Liraglutide vs placebo \u00B7 Reference scenario",
    ##                           y_label = "Relative RMSE",
    ##                           caption = paste(
    ##                               "100 simulation runs per configuration.",
    ##                               "Reference: simulated true treatment effect."
    ##                           ))
    ##         p_updated <- plot +
    ##             ggplot2::geom_line(
    ##                          data = naive_data,
    ##                          mapping = ggplot2::aes(x = n, y = rmse, linetype = "Naive estimates"),
    ##                          color = "grey",
    ##                          linetype = "dashed",
    ##                          linewidth = 1.2,
    ##                          inherit.aes = FALSE
    ##                          )
    ##     }
    ## ),

    tar_target(
        name = rmse00,
        command = {
            data <- data.table::as.data.table(results00)
            plot_rmse_interactive(results00, ref = reference_minimal)
        }
    ),

    tar_target(
        rmse00_html,
        {
            dir.create(
                "website/widgets",
                recursive = TRUE,
                showWarnings = FALSE
            )
            output <- "website/widgets/rmse00.html"
            htmlwidgets::saveWidget(
                             widget = rmse00,
                             file = output,
                             selfcontained = FALSE
                         )
            output
        },
        format = "file"
    ),

    tar_target(
        name = boxplot00,
        command = {
            data <- data.table::as.data.table(results00)
            plot_boxplot_interactive(results00, ref = reference_minimal)
        }
    ),


    tar_target(
        boxplot00_html,
        {
            dir.create(
                "website/widgets",
                recursive = TRUE,
                showWarnings = FALSE
            )
            output <- "website/widgets/boxplot00.html"
            htmlwidgets::saveWidget(
                             widget = boxplot00,
                             file = output,
                             selfcontained = FALSE
                         )
            output
        },
        format = "file"
    ),

    ## # Boxplot comparing bias for different model/samplesize combinations
    ## tar_target(
    ##     name = boxplot00,
    ##     command = {
    ##         data <- data.table::as.data.table(results00)
    ##         reference <- as.numeric(reference_minimal)
    ##         data <- data[n == 10000, norm_diff := (rTMLE - reference)/reference]
    ##         plot_boxplot_comparison(data = data, n = 10000,
    ##                                 y = "norm_diff",
    ##                                 title = "Distribution of relative treatment-effect errors",
    ##                                 subtitle = "Liraglutide vs placebo \u00B7 Reference scenario",
    ##                                 y_label = "Relative estimation error",
    ##                                 caption = "Dashed line indicates zero estimation error.",
    ##                                 limits = c(-0.5, 0.5),
    ##                                 sep = 0.1)
    ##     }),

    #-------------------------Adding-censoring--------------------------------

    # Same as minimal setting but with scale of dropout non-zero
    tar_target(
        name = simple_with_censoring,
        command = {
            p <- simple_setting
            p$parameter_values <- modifyList(
                p$parameter_values,
                list(
                    scale_death = 0.1,
                    scale_dropout = 0.1,
                    effect_lira_dropout = 0,
                    effect_lira_death = -1
                ))
            p[]
        }
    ),

    # Simulation results
    tarchetypes::tar_map_rep(
                     name = results0ce,
                     command = {
                         results <- simple_run(
                             simple_with_censoring, n, selector, alpha
                         )
                     },
                     values = tidyr::expand_grid(
                                         n = c(500, 2000, 5000, 10000),
                                         selector = c("min", "1se", "undersmooth"),
                                         alpha = c(0, 0.5, 1)
                                     ),
                     reps = 4
                 ),

    
    # Graph for bias vs samplesize
##     tar_target(
##         name = rmse0ce,
##         command = {
##             data <- data.table::as.data.table(results0ce)
##             reference <- as.numeric(reference_minimal)
##             summary_data <- data[,
##                                  .(rmse = sqrt(mean((rTMLE-reference)^2))/reference, na.rm = TRUE
##                                    ),
##                                  by = .(n, selector, alpha)]
##             KM_data <- data[,
##                                .(rmse = sqrt(mean((KM-reference)^2))/reference,
##                                  na.rm = TRUE),
##                                by = .(n)]
##             plot <- plot_rmse(data = summary_data,
##                               x = "n",
##                               y = "rmse",
##                               title = "Relative RMSE of estimated treatment effects",
##                               subtitle = "Liraglutide vs placebo \u00B7 Reference scenario",
##                               y_label = "Relative RMSE",
##                               caption = paste(
##                                   "100 simulation runs per configuration.",
##                                   "Reference: simulated true treatment effect."
##                               ))
##         p_updated <- plot +
##             ggplot2::geom_line(
##                          data = KM_data,
##                          mapping = ggplot2::aes(x = n, y = rmse, linetype = "KM estimates"),
##                          color = "grey",
##                          linetype = "dashed",
##                          linewidth = 1.2,
##                          inherit.aes = FALSE
##                      )
##     }
## ),

##     # Boxplot comparing bias for different model/samplesize combinations
##     tar_target(
##         name = boxplot0ce,
##         command = {
##             data <- data.table::as.data.table(results0ce)
##             reference <- as.numeric(reference_minimal)
##             data <- data[n == 10000, norm_diff := (rTMLE - reference)/reference]
##             plot_boxplot_comparison(data = data, n = 10000,
##                                     y = "norm_diff",
##                                     title = "Distribution of relative treatment-effect errors",
##                                     subtitle = "Liraglutide vs placebo \u00B7 Reference scenario",
##                                     y_label = "Relative estimation error",
##                                     caption = "Dashed line indicates zero estimation error.",
##                                     limits = c(-0.5, 0.5),
##                                     sep = 0.1)
##         }),


    #-------------------------Adding-confounding-variable(s)------------------

    
    # Same as minimal setting but with scale of dropout non-zero
    tar_target(
        name = simple_with_confounding,
        command = {
            p <- simple_setting
            p$parameter_values <- modifyList(
                p$parameter_values,
                list(
                    scale_death = 0.1,
                    scale_dropout = 0.1,
                    effect_lira_dropout = 0,
                    effect_lira_death = -1,
                    effect_hba1c_randomized_treatment = 1,
                    effect_hba1c_death = 1
                ))
        }
    ),

    ## # Simulation results
    ## tarchetypes::tar_map_rep(
    ##                  name = results0co,
    ##                  command = {
                         
    ##                      results <- simple_run(
    ##                          simple_with_confounding, n, selector, alpha
    ##                      )
    ##                  },
    ##                  values = tidyr::expand_grid(
    ##                                      n = c(500, 2000, 5000, 10000),
    ##                                      selector = c("min", "1se", "undersmooth"),
    ##                                      alpha = c(0, 0.5, 1)
    ##                                  ),
    ##                  reps = 4
    ##              )

    
    ## # Graph for bias vs samplesize
    ## tar_target(
    ##     name = rmse0co,
    ##     command = {
    ##         data <- data.table::as.data.table(results0ce)
    ##         reference <- as.numeric(reference_minimal)
    ##         summary_data <- data[,
    ##                              .(rmse = sqrt(mean((rTMLE-reference)^2))/reference, na.rm = TRUE
    ##                                ),
    ##                              by = .(n, selector, alpha)]
    ##         plot_rmse(summary_data, "n", "rmse", c("Sample size", "RMSE"),
    ##                   "RMSE for increasing sample sizes")
    ##     }
    ## ),

    ## # Boxplot comparing bias for different model/samplesize combinations
    ## tar_target(
    ##     name = boxplot0co,
    ##     command = {
    ##         data <- data.table::as.data.table(results0ce)
    ##         reference <- as.numeric(reference_minimal)
    ##         data <- data[n == 10000, norm_diff := (rTMLE - reference)/reference]
    ##         plot_boxplot_comparison(data, n = 10000, "norm_diff", c("Sample sizes", "Normalized difference wrt. reference_minimal"),
    ##                                 "Bias by different learner specs", c(-1,1), 0.1)
    ## })


    # Thought is right now to introduce different post_baseline_visit_hook
    # setting the treatment based on variables that also have affect on death


    #-------------------------Adding-confounding-&-censoring------------------


    #-------------------------------------------------------------------------
    #-------------------------1st-Extension-----------------------------------
    #-------------------------------------------------------------------------


    #-------------------------------------------------------------------------
    #-------------------------Scenario-Inspectors-----------------------------
    #-------------------------------------------------------------------------

    tar_target(
        simple_scenario_inspector,
        generate_scenario_inspector(
            setting = simple_setting,
            cohort = example_cohort_minimal,
            name = "simple_scenario"
        ),
        format = "file"
    ),

    tar_target(
        name = LEADER_setting,
        command = {
            p <- get_LEADER_setting()
            # Baseline distributions
            p$parameter_values$intercept_age <- 60
            p$parameter_values$var_age <- 10
            p$parameter_values$intercept_bmi <- 32
            p$parameter_values$var_bmi <- 4
            p$parameter_values$intercept_egfr <- log(70)
            p$parameter_values$var_egfr <- 0.4
            p$parameter_values$intercept_diabetesduration <- log(10) 
            p$parameter_values$var_diabetesduration <- 0.3
            p$parameter_values$intercept_hba1c_level <- log(48)
            p$parameter_values$var_hba1c_level <- 0.4
            # Measurement distributions
            p$parameter_values$intercept_hba1c_change <- 0
            p$parameter_values$var_hba1c_change <- 10
            # Potentially add the other changing measurements.
            p$parameter_values$intercept_bmi_change <- 0
            p$parameter_values$var_bmi_change <- 1
            p$parameter_values$intercept_egfr_change <- 0
            p$parameter_values$var_egfr_change <- 10
            # Scale of events
            p$parameter_values$scale_dropout <- 0
            p$parameter_values$scale_all.cause.mortality <- 1/1000
            p$parameter_values$scale_cv.death <- 1/1000
            p$parameter_values$scale_stroke <- 1/1000
            # Visit schedule
            p$visit_schedule$sd <- 1/10
            p$visit_schedule$minimum_time_between_visits <- 1/10
            p[]
        }
    ),

    tar_target(
        LEADER_scenario_inspector,
        generate_scenario_inspector(
            setting = LEADER_setting,
            cohort = example_cohort_LEADER,
            name = "LEADER_scenario"
        ),
        format = "file"
    )


    

)


######################################################################
### _targets.R ends here
