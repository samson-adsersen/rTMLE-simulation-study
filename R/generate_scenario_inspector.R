### generate_scenario_inspector.R --- 
#----------------------------------------------------------------------
## Author: 
## Created: nov 12 2026 (14:14) 
## Version: 
## Last-Updated: nov 12 2026 (14:17) 
##           By: SADS0006
##     Update #: 1
#----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
#----------------------------------------------------------------------
## 
### Code:


generate_scenario_inspector <- function(
    setting,
    cohort,
    name,
    example_id = NULL,
    output_dir = "website/generated"
) {
    dir.create(
        output_dir,
        recursive = TRUE,
        showWarnings = FALSE
    )

    # Escape text before inserting into HTML
    esc <- function(x) {
        x <- as.character(x)
        x <- gsub("&", "&amp;", x, fixed = TRUE)
        x <- gsub("<", "&lt;", x, fixed = TRUE)
        x <- gsub(">", "&gt;", x, fixed = TRUE)
        x <- gsub('"', "&quot;", x, fixed = TRUE)
        x
    }

    # Format individual values
    fmt <- function(x) {
        if (length(x) == 0L) return("")
        if (length(x) > 1L) {
            return(paste(vapply(
                as.list(x), fmt, character(1)
            ), collapse = ", "))
        }
        if (is.na(x)) return("NA")
        if (is.numeric(x)) {
            return(format(signif(x, 4), trim = TRUE))
        }
        as.character(x)
    }

    # Extract scalar, non-zero numeric parameters
    parameters <- setting$parameter_values

    keep <- vapply(
        parameters,
        function(x) {
            is.numeric(x) &&
                length(x) == 1L &&
                !is.na(x) &&
                x != 0
        },
        logical(1)
    )

    parameters <- parameters[keep]

    # Group parameters by naming convention
    classify <- function(x) {
        if (startsWith(x, "effect_")) {
            "Effects"
        } else if (startsWith(x, "scale_")) {
            "Event scales"
        } else if (startsWith(x, "mean_")) {
            "Means"
        } else if (startsWith(x, "var_")) {
            "Variances"
        } else {
            "Other parameters"
        }
    }

    parameter_names <- names(parameters)

    groups <- split(
        parameter_names,
        factor(
            vapply(parameter_names, classify, character(1)),
            levels = c(
                "Effects",
                "Event scales",
                "Means",
                "Variances",
                "Other parameters"
            )
        )
    )

    # Generate grouped parameter tables
    parameter_html <- paste(
        vapply(names(groups), function(group) {
            keys <- groups[[group]]

            rows <- paste(
                vapply(keys, function(key) {
                    sprintf(
                        "<tr><td><code>%s</code></td><td>%s</td></tr>",
                        esc(key),
                        esc(fmt(parameters[[key]]))
                    )
                }, character(1)),
                collapse = "\n"
            )

            sprintf(
                paste0(
                    '<section class="scenario-group">',
                    '<h4>%s</h4>',
                    '<table><tbody>%s</tbody></table>',
                    '</section>'
                ),
                esc(group),
                rows
            )
        }, character(1)),
        collapse = "\n"
    )

    if (!length(parameters)) {
        parameter_html <- "<p>No non-zero numeric parameters.</p>"
    }

    # ------------------------------------------
    # Visit schedule
    # ------------------------------------------

    visit_schedule <- setting$visit_schedule

    if (!is.null(visit_schedule) &&
        length(visit_schedule) > 0L) {

        # Handle a named list of schedule settings
        if (is.list(visit_schedule) &&
            !is.null(names(visit_schedule))) {

            schedule_rows <- paste(
                vapply(names(visit_schedule), function(key) {
                    value <- visit_schedule[[key]]

                    sprintf(
                        "<tr><td><code>%s</code></td><td>%s</td></tr>",
                        esc(key),
                        esc(fmt(value))
                    )
                }, character(1)),
                collapse = "\n"
            )

        } else {

            # Handle a simple vector of visit times
            schedule_rows <- sprintf(
                paste0(
                    "<tr><td>Scheduled visits</td>",
                    "<td>%s</td></tr>"
                ),
                esc(fmt(visit_schedule))
            )
        }

        schedule_html <- sprintf(
            paste0(
                '<section class="scenario-group">',
                '<h4>Visit scheduling</h4>',
                '<table><tbody>%s</tbody></table>',
                '</section>'
            ),
            schedule_rows
        )

    } else {
        schedule_html <- "<p>No visit schedule specified.</p>"
    }

    # ------------------------------------------
    # Example individual
    # ------------------------------------------

    if (is.null(example_id)) {
        example_id <- cohort$id[1]
    }

    example <- cohort[
        !is.na(cohort$id) & cohort$id == example_id,
        ,
        drop = FALSE
    ]

    if ("time" %in% names(example)) {
        example <- example[order(example$time), , drop = FALSE]
    }

    # Build longitudinal cohort table
    headers <- paste(
        sprintf("<th>%s</th>", esc(names(example))),
        collapse = ""
    )

    rows <- apply(example, 1, function(row) {
        paste0(
            "<tr>",
            paste(
                sprintf(
                    "<td>%s</td>",
                    esc(vapply(row, fmt, character(1)))
                ),
                collapse = ""
            ),
            "</tr>"
        )
    })

    cohort_html <- sprintf(
        paste0(
            '<div class="scenario-table-scroll">',
            '<table>',
            '<thead><tr>%s</tr></thead>',
            '<tbody>%s</tbody>',
            '</table>',
            '</div>'
        ),
        headers,
        paste(rows, collapse = "\n")
    )

    # ------------------------------------------
    # Generate HTML inspector
    # ------------------------------------------

    html <- sprintf(
        paste0(
            '<details class="scenario-inspector">',
            '<summary>Inspect scenario</summary>',
            '<div class="scenario-inspector-body">',

            '<details class="scenario-panel" open>',
            '<summary>Non-zero parameters (%d)</summary>',
            '%s',
            '</details>',

            '<details class="scenario-panel">',
            '<summary>Visit schedule</summary>',
            '%s',
            '</details>',

            '<details class="scenario-panel">',
            '<summary>Example individual (ID: %s)</summary>',
            '%s',
            '</details>',

            '</div>',
            '</details>'
        ),
        length(parameters),
        parameter_html,
        schedule_html,
        esc(example_id),
        cohort_html
    )

    path <- file.path(
        output_dir,
        paste0(name, ".html")
    )

    writeLines(html, path, useBytes = TRUE)

    # Return path for targets format = "file"
    path
}


######################################################################
### generate_scenario_inspector.R ends here
