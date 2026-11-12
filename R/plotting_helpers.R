### plotting_helpers.R --- 
#----------------------------------------------------------------------
## Author: 
## Created: nov 12 2026 (14:42) 
## Version: 
## Last-Updated: nov 12 2026 (15:15) 
##           By: SADS0006
##     Update #: 8
#----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
#----------------------------------------------------------------------
## 
### Code:


make_learner_labels <- function(
    data,
    learner_vars = c("selector", "alpha"),
    prefix = "glmnet"
) {
    stopifnot(all(learner_vars %in% names(data)))

    labels <- do.call(
        paste,
        c(
            list(prefix),
            unname(as.list(data[learner_vars])),
            list(sep = " \u00B7 ")
        )
    )

    labels
}

theme_simulation <- function(base_size = 13) {
    ggplot2::theme_minimal(base_size = base_size) +
        ggplot2::theme(
            plot.title = ggplot2::element_text(
                face = "bold",
                size = 15,
                hjust = 0
            ),
            plot.subtitle = ggplot2::element_text(
                color = "grey40",
                size = 11,
                margin = ggplot2::margin(b = 12)
            ),
            plot.caption = ggplot2::element_text(
                color = "grey45",
                size = 9,
                hjust = 0
            ),
            legend.position = "bottom",
            panel.grid.minor = ggplot2::element_blank(),
            panel.grid.major.x = ggplot2::element_blank()
        )
}


plot_rmse <- function(
    data,
    x = "n",
    y = "rmse",
    title = "RMSE of treatment-effect estimates",
    subtitle = NULL,
    x_label = "Sample size (n)",
    y_label = "Root mean squared error",
    caption = NULL,
    learner_vars = c("selector", "alpha"),
    learner_prefix = "glmnet",
    show_points = TRUE,
    y_limits = NULL
    ) {
    browser()
    1
    stopifnot(
        all(c(x, y, learner_vars) %in% names(data))
    )

    data <- as.data.frame(data)

    data$learner <- make_learner_labels(
        data,
        learner_vars = learner_vars,
        prefix = learner_prefix
    )

    p <- ggplot2::ggplot(
        data,
        ggplot2::aes(
            x = .data[[x]],
            y = .data[[y]],
            color = .data[["learner"]],
            group = .data[["learner"]]
        )
    ) +
        ggplot2::geom_line(
            linewidth = 0.9
        ) +
        ggplot2::labs(
            title = title,
            subtitle = subtitle,
            x = x_label,
            y = y_label,
            color = "Learner configuration",
            caption = caption
        ) +
        ggplot2::scale_x_continuous(
            breaks = sort(unique(data[[x]])),
            labels = scales::label_comma()
        ) +
        ggplot2::scale_y_continuous(
            limits = y_limits,
            expand = ggplot2::expansion(mult = c(0.02, 0.08))
        ) +
        theme_simulation() +
        ggplot2::theme(
            plot.title = ggplot2::element_text(
                face = "bold",
                size = 15
            ),
            plot.subtitle = ggplot2::element_text(
                color = "grey40",
                size = 11,
                margin = ggplot2::margin(b = 14)
            ),
            plot.caption = ggplot2::element_text(
                color = "grey45",
                size = 9,
                hjust = 0
            ),
            legend.position = "bottom",
            legend.title = ggplot2::element_text(
                size = 10
            ),
            panel.grid.minor = ggplot2::element_blank(),
            panel.grid.major.x = ggplot2::element_blank()
        )

    if (show_points) {
        p <- p +
            ggplot2::geom_point(size = 2.2)
    }

    p
}


plot_boxplot_comparison <- function(
    data,
    n,
    y,
    title = "Distribution of treatment-effect estimation errors",
    subtitle = NULL,
    y_label = "Estimation error",
    caption = NULL,
    learner_vars = c("selector", "alpha"),
    learner_prefix = "glmnet",
    limits = NULL,
    sep = NULL,
    reference_line = 0
    ) {
    browser()
    1
    stopifnot(
        all(c("n", y, learner_vars) %in% names(data))
    )

    data <- as.data.frame(data)

    # Filter by sample size
    data <- data[
        !is.na(data[["n"]]) & data[["n"]] == n,
        ,
        drop = FALSE
    ]

    if (nrow(data) == 0L) {
        stop("No observations found for n = ", n)
    }

    # Construct learner labels
    data$learner <- make_learner_labels(
        data,
        learner_vars = learner_vars,
        prefix = learner_prefix
    )

    data$learner <- factor(
        data$learner,
        levels = unique(data$learner)
    )

    # Include sample size in subtitle
    subtitle <- paste(
        c(subtitle, paste0("n = ", format(n, big.mark = ","))),
        collapse = " \u00B7 "
    )

    p <- ggplot2::ggplot(
        data,
        ggplot2::aes(
            x = .data[["learner"]],
            y = .data[[y]],
            fill = .data[["learner"]]
        )
    ) +
        ggplot2::geom_boxplot(
            width = 0.65,
            alpha = 0.7,
            outlier.size = 1.4,
            linewidth = 0.5
        ) +
        ggplot2::labs(
            title = title,
            subtitle = subtitle,
            x = NULL,
            y = y_label,
            caption = caption
        ) +
        ggplot2::guides(
            fill = "none"
        ) +
        ggplot2::theme_minimal(
            base_size = 13
        ) +
        ggplot2::theme(
            plot.title = ggplot2::element_text(
                face = "bold",
                size = 15
            ),
            plot.subtitle = ggplot2::element_text(
                color = "grey40",
                size = 11,
                margin = ggplot2::margin(b = 14)
            ),
            plot.caption = ggplot2::element_text(
                color = "grey45",
                size = 9,
                hjust = 0
            ),
            axis.text.x = ggplot2::element_text(
                angle = 40,
                hjust = 1,
                size = 10
            ),
            panel.grid.minor = ggplot2::element_blank(),
            panel.grid.major.x = ggplot2::element_blank()
        )

    if (!is.null(reference_line)) {
        p <- p +
            ggplot2::geom_hline(
                yintercept = reference_line,
                linetype = "dashed",
                color = "grey50",
                linewidth = 0.5
            )
    }

    if (!is.null(limits)) {
        p <- p +
            ggplot2::coord_cartesian(ylim = limits)
    }

    if (!is.null(sep)) {
        breaks <- if (is.null(limits)) {
            scales::breaks_width(sep)
        } else {
            seq(limits[1], limits[2], by = sep)
        }

        p <- p +
            ggplot2::scale_y_continuous(
                breaks = breaks
            )
    }

    p
}




######################################################################
### plotting_helpers.R ends here
