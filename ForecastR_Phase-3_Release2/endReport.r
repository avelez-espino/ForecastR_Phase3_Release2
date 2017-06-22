endReport <- function(docx.file="Report.docx"){

    # Produce report
    writeDoc(doc, file = docx.file)

    # Open report
    # browseURL(docx.file)

}