### build_html.R --- 
#----------------------------------------------------------------------
## Author: 
## Created: okt 29 2026 (11:54) 
## Version: 
## Last-Updated: okt 29 2026 (14:34) 
##           By: SADS0006
##     Update #: 9
#----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
#----------------------------------------------------------------------
## 
### Code:

targets::tar_make(reporter = "silent")
source("scripts/publish_site.R")

# ;; Make Emacs executable available to R/ESS
# (setenv "EMACS_EXECUTABLE"
#         (expand-file-name invocation-name invocation-directory))

find_emacs <- function() {

    emacs <- Sys.getenv("EMACS_EXECUTABLE")

    if (nzchar(emacs) && file.exists(emacs))
        return(emacs)

    emacs <- Sys.which("emacs")

    if (nzchar(emacs))
        return(unname(emacs))

    stop("Emacs executable not found.")
}

invisible(
status <- system2(
    find_emacs(),
    args = c("--batch", "-Q", "-l", "publish.el")
))

if (status != 0) {
    stop("Org to html publication failed")}

message("html generated in docs/")



######################################################################
### build_html.R ends here
