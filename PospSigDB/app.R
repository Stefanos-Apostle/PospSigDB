#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
#install.packages("shinyFiles")
library(shinyFiles)


## setting working directory to PospSigDB path; should work on both hpc and local Rstudios
volumes = getVolumes()()
if (length(grep("secondary", volumes)) == 0) {
  root = "/secondary/projects/pospisilik/PospSigDB"
} else{
  secondary = volumes[grep('secondary', volumes)]
  root = paste(secondary, "/pospisilik/PospSigDB", sep = "")
}

## function from my package but adapted to accpet inputText values
write_gmt <- function(title, description, genes, path=".") {
  nt <- toupper(title)
  nt <- strsplit(nt, split = " ")[[1]]
  nt <- paste(nt, collapse = "_")

  fg <- strsplit(genes, ", ") ## genes should be seaprated by commma and space

  contents <- unlist(c(nt, paste(">", description, sep = " "), fg))

  write.table(x = t(as.data.frame(contents)), file = paste(paste(path, "/", sep=''), nt, ".gmt", sep = ""), sep = "\t",
              row.names = F, col.names = F, quote=F)
}


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("PospSigDB: Creating Custom Gene Sets"),

    helpText("This shiny app is meant to be an easy way for anyone to contribute to the Pospisilik lab gene set database.
             Although there are plenty of gene sets available on MSigDB, the generation and cataloging of our own gene sets
             will ease future research amongst our own lab's projects. Please follow the help text below to create your own
             custom gene set and submit it to the PospSigDB. If you are lacking inspiration, find some examples at http://www.gsea-msigdb.org/gsea/msigdb/genesets.jsp?collection=C2"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(width = 4,

            helpText("1. Select the folder you want to place gene set in PospSigDB. Follow the organization;
                     method used for data >> researcher >> project."),

            bootstrapPage(
              shinyDirButton('folder', 'Select a folder', 'Output Directory', FALSE,
                             style = "color: black; background-color: #FFF192; border-color: Black")
            ),

            #textOutput("sel_folder")

        ),

        # Show a plot of the generated distribution
        mainPanel(width = 5,
           textInput("title", "Name of Gene Set"),
           helpText("2. Create a simple name for the gene set that may reflect where
                    they are from or how they were compiled."),
           textInput("descr", "Description of Gene Set"),
           helpText("3. Provide necessary details on the project, tissue type, relevance of these genes etc.
                    so that the set is self explanatory."),
           textInput("genes", "Genes included in Gene Set"),
           helpText("4. Paste in genes with a comma and space separating them, for example;
                    NNAT, TRIM28, HDAC1, HDAC2. Do not add spaces or special characters."),
           actionButton(inputId = "write", label = "Write Gene Set to file",
                        style = "color: black; background-color: #FFF192; border-color: Black"),
           helpText("5. Finally, press this button to write the above values to a .gmt formatted gene set in the selected folder in step 1!")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  shinyDirChoose(input, 'folder', roots=c(wd=root), filetypes=c('', 'txt'))
  observe({
    selection = input$folder
    #print(selection)
    if (length(selection) > 1) {
      outdir = unlist(selection$path)
      outdir <<- paste(outdir, collapse = "/")
    }

    output$sel_folder <- renderText({
      paste("PospSigDB", outdir, sep = "")
    })

  })


  observeEvent(input$write, {
    gmt_path = paste(root, outdir,"/", sep = "")
    write_gmt(input$title, input$descr, input$genes, path = gmt_path)
  })

}

# Run the application
shinyApp(ui = ui, server = server)





















































