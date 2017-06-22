startReport <- function(template="Template_ReporteRs.docx", plotwidth=6, plotheight=7){

    plotwidth <<- plotwidth
    plotheight <<- plotheight

    ## usePackage <- function(p) {
    ##     if (!is.element(p, installed.packages()[,1]))
    ##     install.packages(p, dep = TRUE)
    ##     require(p, character.only = TRUE)
    ## }


    ## usePackage("ReporteRs")

    ## options("ReporteRs-fontsize" = 10,
    ##    "ReporteRs-default-font" = "Calibri")

    options( "ReporteRs-fontsize" = 10, 
         "ReporteRs-default-font" = "Calibri")

    # assign the template
    doc <<- docx(template = template, empty_template=TRUE)

    # display available styles
    # styles(doc)

}

