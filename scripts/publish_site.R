### publish_site.R --- 
#----------------------------------------------------------------------
## Author: 
## Created: okt 29 2026 (10:19) 
## Version: 
## Last-Updated: okt 29 2026 (14:31) 
##           By: SADS0006
##     Update #: 25
#----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
#----------------------------------------------------------------------
## 
### Code:

pckgs <- c("targets", "ggplot2", "plotly", "htmlwidgets", "data.table",
           "pandoc")
invisible(suppressWarnings(lapply(pckgs, library, character.only = TRUE)))

# Create output directories
dir.create("website/figures",
           recursive = TRUE, showWarnings = FALSE)

dir.create("website/widgets",
           recursive = TRUE, showWarnings = FALSE)

# Read existing simulation results
figs <- as.data.table(
    tar_read(fig_names)
)

invisible(suppressWarnings(lapply(figs, function(fig){
    ggsave(
    filename = paste0("website/figures/",fig,".png"),
    plot = targets::tar_read_raw(fig),
    width = 8,
    height = 5,
    dpi = 300)})))


## # Interactive version
## interactive_plot <- ggplotly(p)

## saveWidget(
##     widget = interactive_plot,
##     file = "website/widgets/minimal_interactive.html",
##     selfcontained = FALSE
## )

## # Summary table
## summary_results <- results[
##     ,
##     .(
##         mean_difference = mean(difference, na.rm = TRUE),
##         sd_difference = sd(difference, na.rm = TRUE),
##         simulations = .N
##     ),
##     by = .(n, selector, alpha)
## ]

## fwrite(
##     summary_results,
##     "website/simulation_summary.csv"
## )


######################################################################
### publish_site.R ends here
