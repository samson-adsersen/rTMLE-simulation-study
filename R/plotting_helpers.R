### plotting_helpers.R --- 
#----------------------------------------------------------------------
## Author: 
## Created: nov 12 2026 (14:42) 
## Version: 
## Last-Updated: nov 19 2026 (15:23) 
##           By: SADS0006
##     Update #: 51
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



plot_boxplot_interactive <- function(
    data,
    ref,
    estimand_vars = c("rTMLE_lira", "rTMLE_placebo", "rTMLE_diff"),
    title = "Distribution of relative estimation errors",
    subtitle = "Liraglutide vs placebo",
    x_label = "Learner",
    y_label = "Relative estimation error",
    caption = "Dashed line indicates zero estimation error.",
    learner_vars = c("selector", "alpha"),
    learner_prefix = "glmnet",
    show_points = TRUE,
    estimand_labels = NULL,
    y_limits = NULL
) {

    # Convert to a tibble and reshape estimands
    data <- tibble::as_tibble(data)

    data <- tidyr::pivot_longer(
        data,
        cols = dplyr::all_of(estimand_vars),
        names_to = "estimand",
        values_to = "estimate"
    )

    # Join reference values
    ref <- tibble::as_tibble(ref)

    data <- dplyr::left_join(
        data,
        ref[, c("estimand", "reference")],
        by = "estimand"
    )

    # Calculate relative error for each replicate
    data <- data |>
        dplyr::mutate(
            relative_error =
                (estimate - reference) / abs(reference)
        ) |>
        dplyr::filter(
            is.finite(relative_error),
            !is.na(n)
        )

    # Learner labels
    data$learner <- paste(
        learner_prefix,
        do.call(
            paste,
            c(data[learner_vars], sep = " \u00b7 ")
        ),
        sep = " \u00b7 "
    )

    learners <- unique(data$learner)
    sample_sizes <- sort(unique(data$n))

    # Estimand labels
    if (is.null(estimand_labels)) {
        estimand_labels <- stats::setNames(
            estimand_vars,
            estimand_vars
        )
    }

    # Ensure labels exist for all estimands
    for (e in estimand_vars) {
        if (is.null(estimand_labels[[e]])) {
            estimand_labels[e] <- e
        }
    }

    # Consistent colors across dropdown selections
    colors <- grDevices::hcl.colors(
        length(learners),
        palette = "Dark 3"
    )

    # Determine y-axis ranges per estimand
    estimand_ranges <- list()

    for (e in estimand_vars) {

        values <- data$relative_error[
            data$estimand == e
        ]

        values <- c(values, 0)

        if (!is.null(y_limits)) {
            estimand_ranges[[e]] <- y_limits
        } else {
            limits <- range(values, finite = TRUE)
            padding <- max(diff(limits) * 0.08, 0.01)

            estimand_ranges[[e]] <- c(
                limits[1] - padding,
                limits[2] + padding
            )
        }
    }

    # Construct traces
    fig <- plotly::plot_ly()

    trace_n <- numeric()
    trace_estimand <- character()

    for (e in estimand_vars) {
        for (sample_n in sample_sizes) {
            for (i in seq_along(learners)) {

                learner_name <- learners[i]

                subset <- data[
                    data$estimand == e &
                    data$n == sample_n &
                    data$learner == learner_name,
                    ,
                    drop = FALSE
                ]

                if (nrow(subset) == 0) {
                    next
                }

                values <- subset$relative_error

                hover_text <- paste0(
                    "Estimand: ",
                    estimand_labels[[e]],
                    "<br>Learner: ", learner_name,
                    "<br>Sample size: ", sample_n,
                    "<br>Replicates: ", length(values),
                    "<br>Mean relative error: ",
                    signif(mean(values), 4),
                    "<br>Median relative error: ",
                    signif(stats::median(values), 4),
                    "<br>Q1: ",
                    signif(stats::quantile(values, 0.25), 4),
                    "<br>Q3: ",
                    signif(stats::quantile(values, 0.75), 4),
                    "<br>Reference: ",
                    signif(subset$reference[1], 4)
                )

                fig <- plotly::add_trace(
                    fig,
                    type = "box",
                    x = rep(learner_name, length(values)),
                    y = values,
                    name = learner_name,
                    fillcolor = grDevices::adjustcolor(
                        colors[i],
                        alpha.f = 0.55
                    ),
                    line = list(
                        color = colors[i],
                        width = 1.2
                    ),
                    marker = list(
                        color = colors[i],
                        size = 5,
                        opacity = 0.6
                    ),
                    boxpoints = if (show_points) {
                        "all"
                    } else {
                        "outliers"
                    },
                    jitter = 0.3,
                    pointpos = 0,
                    text = rep(hover_text, length(values)),
                    hoverinfo = "text",
                    showlegend = FALSE,
                    visible = (
                        e == estimand_vars[1] &&
                        sample_n == sample_sizes[1]
                    )
                )

                trace_n <- c(trace_n, sample_n)
                trace_estimand <- c(trace_estimand, e)
            }
        }
    }

    # Dropdown buttons
    n_buttons <- lapply(sample_sizes, function(sample_n) {
        list(
            label = format(sample_n, big.mark = ","),
            method = "skip",
            args = list()
        )
    })

    estimand_buttons <- lapply(estimand_vars, function(e) {
        list(
            label = estimand_labels[[e]],
            method = "skip",
            args = list()
        )
    })

    # Horizontal zero line
    zero_line <- list(
        type = "line",
        xref = "paper",
        x0 = 0,
        x1 = 1,
        yref = "y",
        y0 = 0,
        y1 = 0,
        layer = "above",
        line = list(
            color = "#737373",
            width = 1.5,
            dash = "dash"
        )
    )

    # Layout
    fig <- plotly::layout(
        fig,
        title = list(
            text = paste0(
                "<b>", title, "</b>",
                "<br><span style='font-size:12px;color:#686868'>",
                subtitle,
                " \u00b7 ",
                estimand_labels[[estimand_vars[1]]],
                " \u00b7 n = ",
                format(sample_sizes[1], big.mark = ","),
                "</span>"
            ),
            x = 0.02,
            xanchor = "left"
        ),
        xaxis = list(
            title = x_label,
            categoryorder = "array",
            categoryarray = learners,
            showgrid = FALSE,
            tickangle = -25,
            zeroline = FALSE
        ),
        yaxis = list(
            title = y_label,
            range = estimand_ranges[[estimand_vars[1]]],
            gridcolor = "#DEDED8",
            zeroline = FALSE
        ),
        shapes = list(zero_line),
        updatemenus = list(
            list(
                type = "dropdown",
                direction = "down",
                x = 0.72,
                y = 1.02,
                xanchor = "right",
                yanchor = "bottom",
                active = 0,
                bgcolor = "white",
                bordercolor = "#DEDED8",
                buttons = n_buttons
            ),
            list(
                type = "dropdown",
                direction = "down",
                x = 1,
                y = 1.02,
                xanchor = "right",
                yanchor = "bottom",
                active = 0,
                bgcolor = "white",
                bordercolor = "#DEDED8",
                buttons = estimand_buttons
            )
        ),
        margin = list(
            l = 85,
            r = 30,
            t = 155,
            b = 150
        ),
        paper_bgcolor = "#FFFFFC",
        plot_bgcolor = "#FFFFFC",
        annotations = if (!is.null(caption)) {
            list(
                list(
                    text = caption,
                    xref = "paper",
                    yref = "paper",
                    x = 0,
                    y = -0.35,
                    xanchor = "left",
                    yanchor = "top",
                    showarrow = FALSE,
                    font = list(
                        size = 11,
                        color = "#686868"
                    )
                )
            )
        } else {
            list()
        }
    )

    # Metadata for JavaScript dropdown handling
    metadata <- list(
        trace_n = trace_n,
        trace_estimand = trace_estimand,
        sample_sizes = sample_sizes,
        estimand_vars = estimand_vars,
        estimand_ranges = estimand_ranges,
        estimand_labels = as.list(estimand_labels),
        title = title,
        subtitle = subtitle
    )

    js <- "
    function(el, x, meta) {

        var gd = el;

        var selectedN = Number(meta.sample_sizes[0]);
        var selectedEstimand = meta.estimand_vars[0];

        function updatePlot() {

            var visible = meta.trace_n.map(function(n, i) {
                return Number(n) === selectedN &&
                    meta.trace_estimand[i] === selectedEstimand;
            });

            var label = meta.estimand_labels[selectedEstimand];

            var subtitleParts = [
                meta.subtitle,
                label,
                'n = ' + selectedN.toLocaleString('en-US')
            ].filter(Boolean);

            // Always preserve zero reference line
            var shapes = [{
                type: 'line',
                xref: 'paper',
                x0: 0,
                x1: 1,
                yref: 'y',
                y0: 0,
                y1: 0,
                layer: 'above',
                line: {
                    color: '#737373',
                    width: 1.5,
                    dash: 'dash'
                }
            }];

            Plotly.restyle(gd, {
                visible: visible
            }).then(function() {

                return Plotly.relayout(gd, {
                    'title.text':
                        '<b>' + meta.title + '</b>' +
                        '<br><span style=\"font-size:12px;color:#686868\">' +
                        subtitleParts.join(' \u00b7 ') +
                        '</span>',

                    'yaxis.range':
                        meta.estimand_ranges[selectedEstimand],

                    'shapes': shapes
                });
            });
        }

        gd.on('plotly_buttonclicked', function(event) {

            var button = event.button;

            var nIndex = meta.sample_sizes.findIndex(function(n) {
                return Number(
                    String(button.label).replace(/,/g, '')
                ) === Number(n);
            });

            if (nIndex !== -1) {
                selectedN = Number(meta.sample_sizes[nIndex]);
            } else {

                var estimandIndex = meta.estimand_vars.findIndex(
                    function(e) {
                        return meta.estimand_labels[e] === button.label;
                    }
                );

                if (estimandIndex !== -1) {
                    selectedEstimand =
                        meta.estimand_vars[estimandIndex];
                }
            }

            updatePlot();
        });
    }
    "

    fig <- htmlwidgets::onRender(
        fig,
        js,
        data = metadata
    )

    fig
}


plot_rmse_interactive <- function(
    data,
    estimand_vars = c("rTMLE_lira", "rTMLE_placebo", "rTMLE_diff"),
    ref = NULL,
    title = "RMSE of treatment-effect estimates",
    subtitle = NULL,
    x_label = "Sample size (n)",
    y_label = "Relative root mean squared error",
    caption = NULL,
    learner_vars = c("selector", "alpha"),
    learner_prefix = "glmnet",
    show_points = TRUE,
    y_limits = NULL,
    estimand_labels = c(
        rTMLE_lira = "Liraglutide",
        rTMLE_placebo = "Placebo",
        rTMLE_diff = "Risk difference"
    )
) {
    stopifnot(
        all(c("n", learner_vars, estimand_vars) %in% names(data))
    )

    if (is.null(ref)) {
        stop("A reference data frame must be supplied.")
    }

    ref <- data.table::as.data.table(
        as.data.frame(ref)
    )

    stopifnot(
        all(c("estimand", "reference") %in% names(ref))
    )

    if (anyDuplicated(ref$estimand)) {
        stop("Reference values must be unique per estimand.")
    }

    if (!all(estimand_vars %in% ref$estimand)) {
        stop("Missing reference values for one or more estimands.")
    }

    if (!all(estimand_vars %in% names(estimand_labels))) {
        stop("Missing display labels for one or more estimands.")
    }

    # Prepare data
    data <- as.data.frame(data)

    data$learner <- make_learner_labels(
        data,
        learner_vars = learner_vars,
        prefix = learner_prefix
    )

    data <- tidyr::pivot_longer(
        data,
        cols = dplyr::all_of(estimand_vars),
        names_to = "estimand",
        values_to = "estimate"
    )

    data <- data.table::as.data.table(data)

    data <- merge(
        data,
        ref[, .(estimand, reference)],
        by = "estimand",
        all.x = TRUE,
        sort = FALSE
    )

    data <- data[
        !is.na(n) &
        !is.na(learner) &
        is.finite(estimate) &
        is.finite(reference)
    ]

    if (any(data$reference == 0)) {
        stop("Relative RMSE is undefined for zero reference values.")
    }

    # Summarize simulation replicates
    data <- data[
        ,
        .(
            rmse = sqrt(
                mean((estimate - reference)^2)
            ) / abs(reference[1]),
            reference = reference[1],
            replicates = .N
        ),
        by = .(n, estimand, learner)
    ]

    data <- as.data.frame(data)

    if (nrow(data) == 0L) {
        stop("No observations available for plotting.")
    }

    # Consistent y-axis across estimands
    if (is.null(y_limits)) {
        y_limits <- c(
            0,
            max(data$rmse, na.rm = TRUE) * 1.08
        )
    }

    learners <- unique(data$learner)

    colors <- grDevices::hcl.colors(
        length(learners),
        palette = "Dark 3"
    )

    fig <- plotly::plot_ly()

    trace_estimand <- character()

    # Create traces
    for (estimand in estimand_vars) {

        for (i in seq_along(learners)) {

            d <- data[
                data$estimand == estimand &
                data$learner == learners[i],
                ,
                drop = FALSE
            ]

            if (nrow(d) == 0L) next

            d <- d[order(d$n), , drop = FALSE]

            # Detailed hover information
            d$hover <- paste0(
                "<b>", estimand_labels[[estimand]], "</b>",
                "<br>Learner: ", d$learner,
                "<br>Sample size: ", format(
                    d$n, big.mark = ",", scientific = FALSE
                ),
                "<br>Relative RMSE: ", sprintf("%.4f", d$rmse),
                "<br>Reference risk/effect: ",
                sprintf("%.5f", d$reference),
                "<br>Simulation replicates: ", d$replicates
            )

            fig <- plotly::add_trace(
                fig,
                data = d,
                x = ~n,
                y = ~rmse,
                type = "scatter",
                mode = if (show_points) {
                    "lines+markers"
                } else {
                    "lines"
                },
                name = learners[i],
                legendgroup = learners[i],
                showlegend = TRUE,
                visible = (
                    estimand == estimand_vars[1]
                ),
                line = list(
                    color = colors[i],
                    width = 2.2
                ),
                marker = list(
                    color = colors[i],
                    size = 7
                ),
                text = ~hover,
                hovertemplate = "%{text}<extra></extra>"
            )

            trace_estimand <- c(
                trace_estimand,
                estimand
            )
        }
    }

    # Title and subtitle formatting
    make_title <- function(e) {
        subtitle_parts <- c(
            subtitle,
            estimand_labels[[e]]
        )

        paste0(
            "<b>", title, "</b>",
            "<br><span style='font-size:12px;color:#686868'>",
            paste(subtitle_parts, collapse = " \u00B7 "),
            "</span>"
        )
    }

    # Dropdown controls
    buttons <- lapply(estimand_vars, function(e) {
        list(
            label = estimand_labels[[e]],
            method = "update",
            args = list(
                list(
                    visible = as.list(trace_estimand == e)
                ),
                list(
                    title = list(text = make_title(e))
                )
            )
        )
    })

    # Caption as a bottom annotation
    annotations <- list()

    if (!is.null(caption)) {
        annotations <- list(
            list(
                text = caption,
                x = 0,
                y = -0.35,
                xref = "paper",
                yref = "paper",
                xanchor = "left",
                yanchor = "top",
                showarrow = FALSE,
                font = list(
                    size = 11,
                    color = "#737373"
                )
            )
        )
    }

    # Final layout
    fig <- plotly::layout(
        fig,
        title = list(
            text = make_title(estimand_vars[1]),
            x = 0,
            xanchor = "left",
            font = list(
                family = "Arial, sans-serif",
                size = 18,
                color = "#262626"
            )
        ),
        xaxis = list(
            title = list(
                text = x_label,
                font = list(size = 13)
            ),
            tickformat = ",d",
            tickvals = sort(unique(data$n)),
            showgrid = FALSE,
            zeroline = FALSE,
            showline = TRUE,
            linecolor = "#DEDED8",
            tickfont = list(
                size = 11,
                color = "#686868"
            )
        ),
        yaxis = list(
            title = list(
                text = y_label,
                font = list(size = 13)
            ),
            range = y_limits,
            showgrid = TRUE,
            gridcolor = "#ECECE8",
            zeroline = FALSE,
            showline = FALSE,
            tickfont = list(
                size = 11,
                color = "#686868"
            )
        ),
        updatemenus = list(
            list(
                type = "dropdown",
                direction = "down",
                x = 1,
                y = 1.18,
                xanchor = "right",
                yanchor = "top",
                active = 0,
                bgcolor = "white",
                bordercolor = "#DEDED8",
                borderwidth = 1,
                font = list(
                    size = 12,
                    color = "#262626"
                ),
                buttons = buttons
            )
        ),
        legend = list(
            orientation = "h",
            x = 0,
            y = -0.2,
            xanchor = "left",
            yanchor = "top",
            title = list(
                text = "Learner configuration"
            ),
            font = list(size = 11)
        ),
        annotations = annotations,
        margin = list(
            l = 85,
            r = 30,
            t = 125,
            b = if (is.null(caption)) 125 else 165
        ),
        hovermode = "closest",
        paper_bgcolor = "#FFFFFC",
        plot_bgcolor = "#FFFFFC",
        font = list(
            family = "Arial, sans-serif",
            color = "#262626"
        )
    )

    fig
}




######################################################################
### plotting_helpers.R ends here
