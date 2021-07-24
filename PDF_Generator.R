# remotes::install_github('rstudio/rmarkdown')
library(knitr)
library(rmarkdown)
# render("PMS-doc.Rmd")
# render("PMS-doc.Rmd", "pdf_document")
# 
rmarkdown::render("PMS-doc.Rmd", "pdf_document")

rmarkdown::render("PMS-doc.Rmd", "html_document")

rmarkdown::render("PMS-doc.Rmd", "word_document","Data/PMS.docx")


d <- structure(list(Name = structure(1:3, .Label = c("Amy", "Bob", 
                                                     "Charlie"), class = "factor"), Score = structure(1:3, .Label = c("A", 
                                                                                                                      "B", "C"), class = "factor")), class = "data.frame", row.names = c(NA, 
                                                                                                                                                                                         -3L))
# Perform operation:
  
  for(i in 1:nrow(d)){
    pdf(file = paste0(d[i, "Name"], ".pdf"))
    plot(0:1, 1:0, bty = "n", type = "n", axes = F, xlab = "", ylab = "")
    text(.5, .5, paste0(d[i, "Name"], ", your score for MATH 101 is ", d[i, "Score"]))
    dev.off()
  }
