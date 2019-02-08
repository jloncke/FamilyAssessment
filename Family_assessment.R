####
# This is a Shiny web application. 

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  titlePanel("Family assessment"),
  
  navbarPage(
    "", # TITEL ON TOP OF NAV LIST
     # FIRST TAB: INSERT LABELS
        tabPanel("Labels",
                 h3('Labels of the family roles'),
                 h5('Here you can insert different labels for the four family members'),
             h5(textInput("Mother", label = "Label role 1 (e.g. Mother)",
                                 value = 'M'),
                       
                       textInput("Father", label = "Label role 2 (e.g. Father)",
                                 value = 'F'),
                       
                       textInput("C1", label = "Label role 3 (e.g. Oldest child)",
                                 value = 'C1'),
                       
                       textInput("C2", label = "Label role 4 (e.g. Youngest child)",
                                 value = 'C2')
                )
    ),
    # SECOND TAB: INSERT SINGLE FAMILY DYADIC VALUES
    tabPanel("Single family",
             h3('Dyadic values of a single family'),
             h5('Please insert the raw dyadic measurements of the family of interest'),
             DT::dataTableOutput('x1'),
             hr()
             #verbatimTextOutput("out")
    ),
    # THIRD TAB: INSERT POPULATION PARAMETERS
    navbarMenu("Norm population",
               tabPanel("SRM means & variances",
                        h3('SRM means and variances of the norm population'),
                        h5('Please provide the means and variances of the SRM effects of the normative sample. These might have been found in the family literature.'),
                        DT::dataTableOutput('x2'),
                        hr()),
               tabPanel("Relationship means & variances",
                        h3('Relationship means and variances of the norm population'),
                        h5('Please provide the means and variances of the relationship effects of the normative sample. These might have been found in the family literature.'),
                        DT::dataTableOutput('x3'),
                        hr()),
               tabPanel("Reciprocities",
                        h3('Generalized and dyadic reciprocities of the norm population'),
                        h4('1. Indicate whether the reciprocities are correlations or covariances'),
                        selectInput("checkGroup", label = NULL, 
                                           choices = list("Correlations" = 1, "Covariances" = 2),
                                           selected = 1),
                        h4('2. Generalized reciprocities'),
                        DT::dataTableOutput('x4'),
                        hr(),
                        h4('3. Dyadic reciprocities'),
                        DT::dataTableOutput('x5'),
                        hr()
               )),
    
    tabPanel("Results",
              # tabPanel("Anova scores",
              #          DT::dataTableOutput('it_anova'),
              #          hr()),
                        actionButton("do", "Click here to obtain the results"),
             h5('Note that only the Z scores for SRM effects that have a significant variance in the normative sample should be interpreted.'),
                        conditionalPanel("input.checkGroup == 1",
                                         DT::dataTableOutput('it_contents')),
                        conditionalPanel("input.checkGroup == 2",
                                         DT::dataTableOutput('it_contents2')),
                        hr()),
    inverse=TRUE #,theme = "bootstrap.css"
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # OBTAIN THE DIFFERENT LABELS
  Mother_lab <- reactive({
    as.character(input$Mother) # remember the list !
  })
  Father_lab <- reactive({
    as.character(input$Father) # remember the list !
  })
  C1_lab <- reactive({
    as.character(input$C1) # remember the list !
  })
  C2_lab <- reactive({
    as.character(input$C2) # remember the list !
  })
 
  
  # CASUS INPUT
  shinyInput <- function(FUN,id,num,...) {
    inputs <- character(num)
    for (i in seq_len(num)) {
      inputs[i] <- as.character(FUN(paste0(id,i),label=NULL,...))
    }
    inputs
  }
  # function to read DT inputs
  shinyValue <- function(id,num) {
    unlist(lapply(seq_len(num),function(i) {
      value <- input[[paste0(id,i)]]
      if (is.null(value)) NA else value
    }))
  }
  # render datatable with inputs
  output$x1 <- DT::renderDataTable({
    data.frame(Dyad = c(paste(Mother_lab(),'-',Father_lab()), # MF
                        paste(Mother_lab(),'-',C1_lab()), # MT
                        paste(Mother_lab(),'-',C2_lab()), # MS
                        paste(Father_lab(),'-',Mother_lab()), # FM
                        paste(Father_lab(),'-',C1_lab()), # FT
                        paste(Father_lab(),'-',C2_lab()), # FS
                        paste(C1_lab(),'-',Mother_lab()), # TM
                        paste(C1_lab(),'-',Father_lab()), # TF
                        paste(C1_lab(),'-',C2_lab()), # TS
                        paste(C2_lab(),'-',Mother_lab()), # SM
                        paste(C2_lab(),'-',Father_lab()), # SF
                        paste(C2_lab(),'-',C1_lab())), Value = shinyInput(numericInput,"case_dyadicval",12,value=0), row.names=NULL) # ST
  },server=FALSE,escape=FALSE,selection='none',
  options=list(pageLength = 12,paging = FALSE,searching = FALSE,info = FALSE,
  preDrawCallback = DT::JS('function() { Shiny.unbindAll(this.api().table().node()); }'),
  drawCallback = DT::JS('function() { Shiny.bindAll(this.api().table().node()); } ')))
  #options=list(preDrawCallback=DT::JS(
  #  'function() {
  #   Shiny.unbindAll(this.api().table().node());}'),
  #  drawCallback= DT::JS(
  #    'function(settings) {
  #     Shiny.bindAll(this.api().table().node());}')))
  # render output of entered values
  output$out <- renderPrint(
    data.frame(v1=shinyValue("case",12)))
  
  # NORM POPULATION INPUT
  # SRM means & variances
  output$x2 <- DT::renderDataTable({
    data.frame(SRM = c('Family effect',
                        paste('Actor effect', Mother_lab()),
                        paste('Actor effect', Father_lab()),
                        paste('Actor effect', C1_lab()),
                        paste('Actor effect', C2_lab()),
                        paste('Partner effect', Mother_lab()),
                        paste('Partner effect', Father_lab()),
                        paste('Partner effect', C1_lab()),
                        paste('Partner effect', C2_lab())),Mean = shinyInput(numericInput,"pop_SRM_mean",9,value=0), Variance = shinyInput(numericInput,"pop_SRM_var",9,value=0), row.names=NULL)
  },server=FALSE,escape=FALSE,selection='none',
  options=list(pageLength = 9,paging = FALSE,searching = FALSE,info = FALSE,
               preDrawCallback = DT::JS('function() { Shiny.unbindAll(this.api().table().node()); }'),
               drawCallback = DT::JS('function() { Shiny.bindAll(this.api().table().node()); } ')))

  # Residual variances
  output$x3 <- DT::renderDataTable({
    data.frame(Relationship = c(paste(Mother_lab(),'-',Father_lab()),
                        paste(Mother_lab(),'-',C1_lab()),
                        paste(Mother_lab(),'-',C2_lab()),
                        paste(Father_lab(),'-',Mother_lab()),
                        paste(Father_lab(),'-',C1_lab()),
                        paste(Father_lab(),'-',C2_lab()),
                        paste(C1_lab(),'-',Mother_lab()),
                        paste(C1_lab(),'-',Father_lab()),
                        paste(C1_lab(),'-',C2_lab()),
                        paste(C2_lab(),'-',Mother_lab()),
                        paste(C2_lab(),'-',Father_lab()),
                        paste(C2_lab(),'-',C1_lab())), Mean = shinyInput(numericInput,"pop_Rel_mean",12,value=0), Variance = shinyInput(numericInput,"pop_Rel_var",12,value=0), row.names=NULL)
  },server=FALSE,escape=FALSE,selection='none',
  options=list(pageLength = 12,paging = FALSE,searching = FALSE,info = FALSE,
               preDrawCallback = DT::JS('function() { Shiny.unbindAll(this.api().table().node()); }'),
               drawCallback = DT::JS('function() { Shiny.bindAll(this.api().table().node()); } ')))
  
  # Reciprocities
  output$reciproc <- renderPrint({ input$select })
  
  output$x4 <- DT::renderDataTable({
    data.frame(Person = c(paste(Mother_lab()),
                                paste(Father_lab()),
                                paste(C1_lab()),
                                paste(C2_lab())), Reciprocity = shinyInput(numericInput,"General_recip",4,value=0), row.names=NULL)
  },server=FALSE,escape=FALSE,selection='none',
  options=list(pageLength = 4,paging = FALSE,searching = FALSE, info=FALSE,
               preDrawCallback = DT::JS('function() { Shiny.unbindAll(this.api().table().node()); }'),
               drawCallback = DT::JS('function() { Shiny.bindAll(this.api().table().node()); } ')))
  
  output$x5 <- DT::renderDataTable({
    data.frame(Dyad = c(paste(Mother_lab(), '-', Father_lab()),
                        paste(Mother_lab(), '-', C1_lab()),
                        paste(Mother_lab(), '-', C2_lab()),
                        paste(Father_lab(), '-', C1_lab()),
                        paste(Father_lab(), '-', C2_lab()),
                        paste(C1_lab(), '-', C2_lab())), Reciprocity = shinyInput(numericInput,"Dyadic_recip",6,value=0), row.names=NULL)
  },server=FALSE,escape=FALSE,selection='none',
  options=list(pageLength = 6,paging = FALSE,searching = FALSE,info = FALSE,
               preDrawCallback = DT::JS('function() { Shiny.unbindAll(this.api().table().node()); }'),
               drawCallback = DT::JS('function() { Shiny.bindAll(this.api().table().node()); } ')))
  
  # Factor score coefficient (weight matrix)
  # MF   MT   MS   FM   FT   FS  TM   TF    TS   SM   SF  ST
  FSC <- matrix(c(1/12,1/12,1/12,1/12,1/12,1/12,1/12,1/12,1/12,1/12,1/12,1/12, # family effect
                    0.25,0.25,0.25,0,-0.125, -0.125,0, -0.125, -0.125,0, -0.125, -0.125, # Actor M
                    0, -0.125, -0.125,0.25,0.25,0.25, -0.125,0, -0.125, -0.125,0, -0.125, # Actor F
                    -0.125,0, -0.125, -0.125,0, -0.125,0.25,0.25,0.25, -0.125, -0.125,0, # Actor Child 1
                    -0.125, -0.125,0, -0.125, -0.125,0, -0.125, -0.125,0,0.25,0.25,0.25, # Actor Child 2
                    0,0,0,0.25,-0.125, -0.125,0.25, -0.125, -0.125,0.25, -0.125, -0.125, # Partner Mother
                    0.25, -0.125, -0.125,0,0,0, -0.125,0.25, -0.125, -0.125,0.25, -0.125,# Partner Father
                    -0.125,0.25, -0.125, -0.125,0.25, -0.125,0,0,0, -0.125, -0.125,0.25, # Partner Child 1
                    -0.125, -0.125,0.25, -0.125, -0.125,0.25, -0.125, -0.125,0.25,0,0,0, # Partner Child 2
                    5/12, -5/24, -5/24, -1/12, 1/24,  1/24, 1/24, -5/24, 1/6, 1/24,  -5/24, 1/6, # Rel MF 
                    -5/24, 5/12,-5/24, 1/24, -5/24, 1/6, -1/12, 1/24,  1/24, 1/24,  1/6, -5/24, # Rel MC1
                    -5/24, -5/24, 5/12, 1/24, 1/6, -5/24,1/24,1/6 , -5/24, -1/12, 1/24,1/24, # Rel MC2
                    -1/12, 1/24, 1/24, 5/12, -5/24, -5/24, -5/24,  1/24, 1/6, -5/24, 1/24, 1/6,# Rel FM
                    1/24, -5/24, 1/6, -5/24, 5/12, -5/24, 1/24, -1/12, 1/24, 1/6, 1/24, -5/24,# Rel FC1
                    1/24, 1/6, -5/24, -5/24, -5/24, 5/12, 1/6, 1/24, -5/24, 1/24, -1/12, 1/24, # Rel FC2
                    1/24, -1/12, 1/24, -5/24, 1/24, 1/6, 5/12, -5/24, -5/24, -5/24,  1/6, 1/24,# Rel C1M
                    -5/24, 1/24, 1/6, 1/24, -1/12, 1/24,-5/24,5/12,-5/24, 1/6, -5/24, 1/24, # Rel C1F
                    1/6, 1/24, -5/24, 1/6, 1/24, -5/24, -5/24, -5/24, 5/12,1/24, 1/24, -1/12,  # Rel C1C2
                    1/24, 1/24, -1/12, -5/24, 1/6, 1/24, -5/24,  1/6, 1/24, 5/12,-5/24, -5/24, # Rel C2M
                    -5/24, 1/6, 1/24,1/24, 1/24, -1/12, 1/6, -5/24,1/24, -5/24, 5/12, -5/24,  # Rel C2F
                    1/6,  -5/24, 1/24,1/6, -5/24, 1/24, 1/24, 1/24, -1/12, -5/24, -5/24,5/12  # Rel C2C1
  ), 
  nrow=12, ncol=21)

  LAMBDA = matrix(c(1, 1, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                    1, 1, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                    1, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                    1, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0,
                    1, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0,
                    1, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0,
                    1, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0,
                    1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0,
                    1, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0,
                    1, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0,
                    1, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0,
                    1, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1), ncol = 12, nrow=21)
  
  # ANOVA SCORES
  # it_anova <- reactive({
  #     x <- as.numeric(shinyValue('case_dyadicval',12))
  #     # dat <- data.frame(
  #     #    Bins = x
  #     # )
  #     #  x <- c(rep(1,12))
  #     round_df <- function(x, digits) {
  #       # round all numeric variables
  #       # x: data frame 
  #       # digits: number of digits to round
  #       numeric_columns <- sapply(x, mode) == 'numeric'
  #       x[numeric_columns] <-  round(x[numeric_columns], digits)
  #       x
  #     }
  #     SRM_effect <- round_df(as.data.frame(t(FSC)%*%(x)),3)
  #     data2 <- data.frame( SRM = c('Family effect',
  #                                 paste('Actor effect', Mother_lab()),
  #                                 paste('Actor effect', Father_lab()),
  #                                 paste('Actor effect', C1_lab()),
  #                                 paste('Actor effect', C2_lab()),
  #                                 paste('Partner effect', Mother_lab()),
  #                                 paste('Partner effect', Father_lab()),
  #                                 paste('Partner effect', C1_lab()),
  #                                 paste('Partner effect', C2_lab()),
  #                                 paste('Relationship', Mother_lab(),'-' ,Father_lab()),
  #                                 paste('Relationship', Mother_lab(),'-' , C1_lab()),
  #                                 paste('Relationship', Mother_lab(),'-' , C2_lab()),
  #                                 paste('Relationship', Father_lab(),'-' , Mother_lab()),
  #                                 paste('Relationship', Father_lab(),'-' , C1_lab()),
  #                                 paste('Relationship', Father_lab(),'-' , C2_lab()),
  #                                 paste('Relationship', C1_lab(),'-' , Mother_lab()),
  #                                 paste('Relationship', C1_lab(),'-' , Father_lab()),
  #                                 paste('Relationship', C1_lab(),'-' , C2_lab()),
  #                                 paste('Relationship', C2_lab(),'-' , Mother_lab()),
  #                                 paste('Relationship', C2_lab(),'-' , Father_lab()),
  #                                 paste('Relationship', C2_lab(),'-' , C1_lab())),
  #                          value  = SRM_effect
  #     )
  #     names(data2)[2] <- 'value'
  #     return(data2)
  # })
  # 
  # CORRELATIONS
  it_data <- reactive({
    # ANOVA SCORES 1 GEZIN
     x <- as.numeric(shinyValue('case_dyadicval',12))
     SRM_effect <- as.data.frame(t(FSC)%*%(x))
     
     # VERWACHTE VARIANTIE MATRIX TECHNISCH?
     # varianties
     var_pop_SRM <- as.numeric(shinyValue('pop_SRM_var',9))
     var_pop_rel <- as.numeric(shinyValue('pop_Rel_var',12))
     VETA <- diag(21)
     for(i in 1:9){
       VETA[i,i] <- var_pop_SRM[i]
     }
     for(i in 1:12){
       VETA[i+9,i+9] <- var_pop_rel[i]
     }
     # covarianties?
     gen_cor <- as.numeric(shinyValue('General_recip',4))
     VETA[2,6] <- gen_cor[1]*sqrt(var_pop_SRM[2])*sqrt(var_pop_SRM[6])
     VETA[3,7] <- gen_cor[2]*sqrt(var_pop_SRM[3])*sqrt(var_pop_SRM[7])
     VETA[4,8] <- gen_cor[3]*sqrt(var_pop_SRM[3])*sqrt(var_pop_SRM[8])
     VETA[5,9] <- gen_cor[4]*sqrt(var_pop_SRM[4])*sqrt(var_pop_SRM[9])
     
     VETA[6,2] <- VETA[2,6]
     VETA[7,3] <- VETA[3,7]
     VETA[8,4] <- VETA[4,8]
     VETA[9,5] <- VETA[5,9]
     
     dya_cor <- as.numeric(shinyValue('Dyadic_recip',6))
     VETA[10,13] <- dya_cor[1]*sqrt(var_pop_rel[1])*sqrt(var_pop_rel[4])
     VETA[11,16] <- dya_cor[2]*sqrt(var_pop_rel[2])*sqrt(var_pop_rel[7])
     VETA[12,19] <- dya_cor[3]*sqrt(var_pop_rel[3])*sqrt(var_pop_rel[10])
     VETA[14,17] <- dya_cor[4]*sqrt(var_pop_rel[5])*sqrt(var_pop_rel[8])
     VETA[15,20] <- dya_cor[5]*sqrt(var_pop_rel[6])*sqrt(var_pop_rel[11])
     VETA[18,21] <- dya_cor[6]*sqrt(var_pop_rel[9])*sqrt(var_pop_rel[12])
     
     VETA[13,10] <- VETA[10,13]
     VETA[16,11] <- VETA[11,16]
     VETA[19,12] <- VETA[12,19]
     VETA[17,14] <- VETA[14,17]
     VETA[20,15] <- VETA[15,20]
     VETA[21,18] <- VETA[18,21]
     
     S = t(FSC)%*%(t(LAMBDA)%*%VETA%*%(LAMBDA))%*%FSC
     
     # GEMIDDELDE? 
     mean_pop_SRM <- as.numeric(shinyValue('pop_SRM_mean',9))
     mean_pop_rel <- as.numeric(shinyValue('pop_Rel_mean',12))
     
     
     z_scores <- c(
       round((SRM_effect[1,1] - (mean_pop_SRM[1]))/sqrt(S[1,1]), digits=3),
       round((SRM_effect[2,1] - (mean_pop_SRM[2]))/sqrt(S[2,2]), digits=3),
       round((SRM_effect[3,1] - (mean_pop_SRM[3]))/sqrt(S[3,3]), digits=3),
       round((SRM_effect[4,1] - (mean_pop_SRM[4]))/sqrt(S[4,4]), digits=3),
       round((SRM_effect[5,1] - (mean_pop_SRM[5]))/sqrt(S[5,5]), digits=3),
       round((SRM_effect[6,1] - (mean_pop_SRM[6]))/sqrt(S[6,6]), digits=3),
       round((SRM_effect[7,1] - (mean_pop_SRM[7]))/sqrt(S[7,7]), digits=3),
       round((SRM_effect[8,1] - (mean_pop_SRM[8]))/sqrt(S[8,8]), digits=3),
       round((SRM_effect[9,1] - (mean_pop_SRM[9]))/sqrt(S[9,9]), digits=3),
       
       round((SRM_effect[10,1] - (mean_pop_rel[1]))/sqrt(S[10,10]), digits=3),
       round((SRM_effect[11,1] - (mean_pop_rel[2]))/sqrt(S[11,11]), digits=3),
       round((SRM_effect[12,1] - (mean_pop_rel[3]))/sqrt(S[12,12]), digits=3),
       round((SRM_effect[13,1] - (mean_pop_rel[4]))/sqrt(S[13,13]), digits=3),
       round((SRM_effect[14,1] - (mean_pop_rel[5]))/sqrt(S[14,14]), digits=3),
       round((SRM_effect[15,1] - (mean_pop_rel[6]))/sqrt(S[15,15]), digits=3),
       round((SRM_effect[16,1] - (mean_pop_rel[7]))/sqrt(S[16,16]), digits=3),
       round((SRM_effect[17,1] - (mean_pop_rel[8]))/sqrt(S[17,17]), digits=3),
       round((SRM_effect[18,1] - (mean_pop_rel[9]))/sqrt(S[18,18]), digits=3),
       round((SRM_effect[19,1] - (mean_pop_rel[10]))/sqrt(S[19,19]), digits=3),
       round((SRM_effect[20,1] - (mean_pop_rel[11]))/sqrt(S[20,20]), digits=3),
       round((SRM_effect[21,1] - (mean_pop_rel[12]))/sqrt(S[21,21]), digits=3)
     )
     
     pval <- c(
       round(2*pnorm(-abs((SRM_effect[1,1] - (mean_pop_SRM[1]))/sqrt(S[1,1]))), digits=3),
       round(2*pnorm(-abs((SRM_effect[2,1] - (mean_pop_SRM[2]))/sqrt(S[2,2]))), digits=3),
       round(2*pnorm(-abs((SRM_effect[3,1] - (mean_pop_SRM[3]))/sqrt(S[3,3]))), digits=3),
       round(2*pnorm(-abs((SRM_effect[4,1] - (mean_pop_SRM[4]))/sqrt(S[4,4]))), digits=3),
       round(2*pnorm(-abs((SRM_effect[5,1] - (mean_pop_SRM[5]))/sqrt(S[5,5]))), digits=3),
       round(2*pnorm(-abs((SRM_effect[6,1] - (mean_pop_SRM[6]))/sqrt(S[6,6]))), digits=3),
       round(2*pnorm(-abs((SRM_effect[7,1] - (mean_pop_SRM[7]))/sqrt(S[7,7]))), digits=3),
       round(2*pnorm(-abs((SRM_effect[8,1] - (mean_pop_SRM[8]))/sqrt(S[8,8]))), digits=3),
       round(2*pnorm(-abs((SRM_effect[9,1] - (mean_pop_SRM[9]))/sqrt(S[9,9]))), digits=3),
       
       round(2*pnorm(-abs((SRM_effect[10,1] - (mean_pop_rel[1]))/sqrt(S[10,10]))), digits=3),
       round(2*pnorm(-abs((SRM_effect[11,1] - (mean_pop_rel[2]))/sqrt(S[11,11]))), digits=3),
       round(2*pnorm(-abs((SRM_effect[12,1] - (mean_pop_rel[3]))/sqrt(S[12,12]))), digits=3),
       round(2*pnorm(-abs((SRM_effect[13,1] - (mean_pop_rel[4]))/sqrt(S[13,13]))), digits=3),
       round(2*pnorm(-abs((SRM_effect[14,1] - (mean_pop_rel[5]))/sqrt(S[14,14]))), digits=3),
       round(2*pnorm(-abs((SRM_effect[15,1] - (mean_pop_rel[6]))/sqrt(S[15,15]))), digits=3),
       round(2*pnorm(-abs((SRM_effect[16,1] - (mean_pop_rel[7]))/sqrt(S[16,16]))), digits=3),
       round(2*pnorm(-abs((SRM_effect[17,1] - (mean_pop_rel[8]))/sqrt(S[17,17]))), digits=3),
       round(2*pnorm(-abs((SRM_effect[18,1] - (mean_pop_rel[9]))/sqrt(S[18,18]))), digits=3),
       round(2*pnorm(-abs((SRM_effect[19,1] - (mean_pop_rel[10]))/sqrt(S[19,19]))), digits=3),
       round(2*pnorm(-abs((SRM_effect[20,1] - (mean_pop_rel[11]))/sqrt(S[20,20]))), digits=3),
       round(2*pnorm(-abs((SRM_effect[21,1] - (mean_pop_rel[12]))/sqrt(S[21,21]))), digits=3)
     )
     
      data2 <- data.frame(SRM = c('Family effect',
                                  paste('Actor effect', Mother_lab()),
                                  paste('Actor effect', Father_lab()),
                                  paste('Actor effect', C1_lab()),
                                  paste('Actor effect', C2_lab()),
                                  paste('Partner effect', Mother_lab()),
                                  paste('Partner effect', Father_lab()),
                                  paste('Partner effect', C1_lab()),
                                  paste('Partner effect', C2_lab()),
                                  paste('Relationship', Mother_lab(),'-' ,Father_lab()),
                                  paste('Relationship', Mother_lab(),'-' , C1_lab()),
                                  paste('Relationship', Mother_lab(),'-' , C2_lab()),
                                  paste('Relationship', Father_lab(),'-' , Mother_lab()),
                                  paste('Relationship', Father_lab(),'-' , C1_lab()),
                                  paste('Relationship', Father_lab(),'-' , C2_lab()),
                                  paste('Relationship', C1_lab(),'-' , Mother_lab()),
                                  paste('Relationship', C1_lab(),'-' , Father_lab()),
                                  paste('Relationship', C1_lab(),'-' , C2_lab()),
                                  paste('Relationship', C2_lab(),'-' , Mother_lab()),
                                  paste('Relationship', C2_lab(),'-' , Father_lab()),
                                  paste('Relationship', C2_lab(),'-' , C1_lab())
                                  ),
                          anova=round(SRM_effect, digits=3),
                          z = z_scores,
                          p = pval
      )
      names(data2) <- c('SRM-effect', 'ANOVA score','Z score', 'p-value')
      return(data2)
  })
  
  # COVARIANCES
  it_data2 <- reactive({
    # ANOVA SCORES 1 GEZIN
    x <- as.numeric(shinyValue('case_dyadicval',12))
    SRM_effect <- as.data.frame(t(FSC)%*%(x))
    
    # VERWACHTE VARIANTIE MATRIX TECHNISCH?
    # varianties
    var_pop_SRM <- as.numeric(shinyValue('pop_SRM_var',9))
    var_pop_rel <- as.numeric(shinyValue('pop_Rel_var',12))
    VETA <- diag(21)
    for(i in 1:9){
      VETA[i,i] <- var_pop_SRM[i]
    }
    for(i in 1:12){
      VETA[i+9,i+9] <- var_pop_rel[i]
    }
    # covarianties?
    gen_cor <- as.numeric(shinyValue('General_recip',4))
    VETA[2,6] <- gen_cor[1]
    VETA[3,7] <- gen_cor[2]
    VETA[4,8] <- gen_cor[3]
    VETA[5,9] <- gen_cor[4]
    
    VETA[6,2] <- VETA[2,6]
    VETA[7,3] <- VETA[3,7]
    VETA[8,4] <- VETA[4,8]
    VETA[9,5] <- VETA[5,9]
    var_pop_rel
    dya_cor <- as.numeric(shinyValue('Dyadic_recip',6))
    VETA[10,13] <- dya_cor[1]
    VETA[11,16] <- dya_cor[2]
    VETA[12,19] <- dya_cor[3]
    VETA[14,17] <- dya_cor[4]
    VETA[15,20] <- dya_cor[5]
    VETA[18,21] <- dya_cor[6]
    
    VETA[13,10] <- VETA[10,13]
    VETA[16,11] <- VETA[11,16]
    VETA[19,12] <- VETA[12,19]
    VETA[17,14] <- VETA[14,17]
    VETA[20,15] <- VETA[15,20]
    VETA[21,18] <- VETA[18,21]
    
    S = t(FSC)%*%(t(LAMBDA)%*%VETA%*%(LAMBDA))%*%FSC
    
    # GEMIDDELDE? 
    mean_pop_SRM <- as.numeric(shinyValue('pop_SRM_mean',9))
    mean_pop_rel <- as.numeric(shinyValue('pop_Rel_mean',12))
    
    z_scores <- c(
      round((SRM_effect[1,1] - (mean_pop_SRM[1]))/sqrt(S[1,1]), digits=3),
      round((SRM_effect[2,1] - (mean_pop_SRM[2]))/sqrt(S[2,2]), digits=3),
      round((SRM_effect[3,1] - (mean_pop_SRM[3]))/sqrt(S[3,3]), digits=3),
      round((SRM_effect[4,1] - (mean_pop_SRM[4]))/sqrt(S[4,4]), digits=3),
      round((SRM_effect[5,1] - (mean_pop_SRM[5]))/sqrt(S[5,5]), digits=3),
      round((SRM_effect[6,1] - (mean_pop_SRM[6]))/sqrt(S[6,6]), digits=3),
      round((SRM_effect[7,1] - (mean_pop_SRM[7]))/sqrt(S[7,7]), digits=3),
      round((SRM_effect[8,1] - (mean_pop_SRM[8]))/sqrt(S[8,8]), digits=3),
      round((SRM_effect[9,1] - (mean_pop_SRM[9]))/sqrt(S[9,9]), digits=3),
      
      round((SRM_effect[10,1] - (mean_pop_rel[1]))/sqrt(S[10,10]), digits=3),
      round((SRM_effect[11,1] - (mean_pop_rel[2]))/sqrt(S[11,11]), digits=3),
      round((SRM_effect[12,1] - (mean_pop_rel[3]))/sqrt(S[12,12]), digits=3),
      round((SRM_effect[13,1] - (mean_pop_rel[4]))/sqrt(S[13,13]), digits=3),
      round((SRM_effect[14,1] - (mean_pop_rel[5]))/sqrt(S[14,14]), digits=3),
      round((SRM_effect[15,1] - (mean_pop_rel[6]))/sqrt(S[15,15]), digits=3),
      round((SRM_effect[16,1] - (mean_pop_rel[7]))/sqrt(S[16,16]), digits=3),
      round((SRM_effect[17,1] - (mean_pop_rel[8]))/sqrt(S[17,17]), digits=3),
      round((SRM_effect[18,1] - (mean_pop_rel[9]))/sqrt(S[18,18]), digits=3),
      round((SRM_effect[19,1] - (mean_pop_rel[10]))/sqrt(S[19,19]), digits=3),
      round((SRM_effect[20,1] - (mean_pop_rel[11]))/sqrt(S[20,20]), digits=3),
      round((SRM_effect[21,1] - (mean_pop_rel[12]))/sqrt(S[21,21]), digits=3)
    )

    pval <- c(
      round(2*pnorm(-abs((SRM_effect[1,1] - (mean_pop_SRM[1]))/sqrt(S[1,1]))), digits=3),
      round(2*pnorm(-abs((SRM_effect[2,1] - (mean_pop_SRM[2]))/sqrt(S[2,2]))), digits=3),
      round(2*pnorm(-abs((SRM_effect[3,1] - (mean_pop_SRM[3]))/sqrt(S[3,3]))), digits=3),
      round(2*pnorm(-abs((SRM_effect[4,1] - (mean_pop_SRM[4]))/sqrt(S[4,4]))), digits=3),
      round(2*pnorm(-abs((SRM_effect[5,1] - (mean_pop_SRM[5]))/sqrt(S[5,5]))), digits=3),
      round(2*pnorm(-abs((SRM_effect[6,1] - (mean_pop_SRM[6]))/sqrt(S[6,6]))), digits=3),
      round(2*pnorm(-abs((SRM_effect[7,1] - (mean_pop_SRM[7]))/sqrt(S[7,7]))), digits=3),
      round(2*pnorm(-abs((SRM_effect[8,1] - (mean_pop_SRM[8]))/sqrt(S[8,8]))), digits=3),
      round(2*pnorm(-abs((SRM_effect[9,1] - (mean_pop_SRM[9]))/sqrt(S[9,9]))), digits=3),
      
      round(2*pnorm(-abs((SRM_effect[10,1] - (mean_pop_rel[1]))/sqrt(S[10,10]))), digits=3),
      round(2*pnorm(-abs((SRM_effect[11,1] - (mean_pop_rel[2]))/sqrt(S[11,11]))), digits=3),
      round(2*pnorm(-abs((SRM_effect[12,1] - (mean_pop_rel[3]))/sqrt(S[12,12]))), digits=3),
      round(2*pnorm(-abs((SRM_effect[13,1] - (mean_pop_rel[4]))/sqrt(S[13,13]))), digits=3),
      round(2*pnorm(-abs((SRM_effect[14,1] - (mean_pop_rel[5]))/sqrt(S[14,14]))), digits=3),
      round(2*pnorm(-abs((SRM_effect[15,1] - (mean_pop_rel[6]))/sqrt(S[15,15]))), digits=3),
      round(2*pnorm(-abs((SRM_effect[16,1] - (mean_pop_rel[7]))/sqrt(S[16,16]))), digits=3),
      round(2*pnorm(-abs((SRM_effect[17,1] - (mean_pop_rel[8]))/sqrt(S[17,17]))), digits=3),
      round(2*pnorm(-abs((SRM_effect[18,1] - (mean_pop_rel[9]))/sqrt(S[18,18]))), digits=3),
      round(2*pnorm(-abs((SRM_effect[19,1] - (mean_pop_rel[10]))/sqrt(S[19,19]))), digits=3),
      round(2*pnorm(-abs((SRM_effect[20,1] - (mean_pop_rel[11]))/sqrt(S[20,20]))), digits=3),
      round(2*pnorm(-abs((SRM_effect[21,1] - (mean_pop_rel[12]))/sqrt(S[21,21]))), digits=3)
    )
    
    data3 <- data.frame(SRM = c('Family effect',
                                paste('Actor effect', Mother_lab()),
                                paste('Actor effect', Father_lab()),
                                paste('Actor effect', C1_lab()),
                                paste('Actor effect', C2_lab()),
                                paste('Partner effect', Mother_lab()),
                                paste('Partner effect', Father_lab()),
                                paste('Partner effect', C1_lab()),
                                paste('Partner effect', C2_lab()),
                                paste('Relationship', Mother_lab(),'-' ,Father_lab()),
                                paste('Relationship', Mother_lab(),'-' , C1_lab()),
                                paste('Relationship', Mother_lab(),'-' , C2_lab()),
                                paste('Relationship', Father_lab(),'-' , Mother_lab()),
                                paste('Relationship', Father_lab(),'-' , C1_lab()),
                                paste('Relationship', Father_lab(),'-' , C2_lab()),
                                paste('Relationship', C1_lab(),'-' , Mother_lab()),
                                paste('Relationship', C1_lab(),'-' , Father_lab()),
                                paste('Relationship', C1_lab(),'-' , C2_lab()),
                                paste('Relationship', C2_lab(),'-' , Mother_lab()),
                                paste('Relationship', C2_lab(),'-' , Father_lab()),
                                paste('Relationship', C2_lab(),'-' , C1_lab())
    ),
    anova=round(SRM_effect, digits=3),
    z = z_scores,
    p = pval
    )
    names(data3)[1:4] <- c('SRM-effect', 'ANOVA score', 'Z score','p-value') 
    return(data3)
  })
  
  # ANOVA scores
 # output$it_anova <- DT::renderDataTable(
#    it_anova(),
#   # server=FALSE,escape=FALSE,selection='none',
 #   options=list(pageLength = 9 ,paging = FALSE,searching = FALSE,info = FALSE),
    #options = list(autoWidth = TRUE, scrollX = TRUE, dom = 't', ordering = FALSE),
#    rownames = TRUE, selection = 'none')

  
    data <- eventReactive(input$do,{
        it_data3 <- cbind.data.frame(it_data(), it_data2())
        it_data3
    })
  
  # CORRELATIONS
    output$it_contents <- DT::renderDataTable(
      data()[,1:4],
      options=list(pageLength = 9 ,paging = FALSE,searching = FALSE,info = FALSE),
      #options = list(autoWidth = TRUE, scrollX = TRUE, dom = 't', ordering = FALSE),
      rownames = TRUE, selection = 'none')
  
  # COVARIANCES
  output$it_contents2 <- DT::renderDataTable(
    data()[,5:8],
    options=list(pageLength = 9 ,paging = FALSE,searching = FALSE,info = FALSE),
    #options = list(autoWidth = TRUE, scrollX = TRUE, dom = 't', ordering = FALSE),
    rownames = TRUE, selection = 'none')
  
}

# Run the application 
shinyApp(ui = ui, server = server)

