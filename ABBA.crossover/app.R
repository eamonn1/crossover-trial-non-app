library(shiny)
library(RColorBrewer)

# based on https://gallery.shinyapps.io/multi_regression/
# code https://github.com/mwaskom/ShinyApps/tree/master/multi_regression

library(shiny)

fig.width <- 600
fig.height <- 450

ui <-shinyUI(pageWithSidebar(
    
    headerPanel("Modelling two period crossover trial"),
    
    sidebarPanel(
        
        div(p("Relate frequentist modeling choices to summaries of the models and plot the data.")),
        
        div(
            
            selectInput("model",
                        strong("Linear mixed model to evaluate"),
                        choices=c("Additive model",
                                  "Interactive model")),
            
            div(p("Select the true population parameters and a two period crossover data set is simulated (no missing data). A plot of the raw data is generated. You also have the choices of selecting a new sample or fitting an interaction.")),
            
            br(),
            br(),
            actionButton("resample", "New Sample"),
            br(),
            br(),
            br(),
            p(strong("Generate true population parameters:")),
            sliderInput("z",
                        "No of patients in each  treatment sequence",
                        min=4, max=2000, step=1, value=50, ticks=FALSE),
            sliderInput("a",
                        "True intercept",
                        min=0, max=10, step=.2, value=8, ticks=FALSE),
            sliderInput("b",
                        "True main effect",
                        min=0, max=4, step=.2, value=4, ticks=FALSE),
            sliderInput("c",
                        "True order effect",
                        min=0, max=4, step=.2, value=0, ticks=FALSE),
            sliderInput("d",
                        "True interaction",
                        min=0, max=2, step=.2, value=1, ticks=FALSE),
            sliderInput("e",
                        "True within person standard deviation",
                        min=0.1, max=5, step=.1, value=1, ticks=FALSE),
            sliderInput("f",
                        "True between person standard deviation",
                        min=0.1, max=5, step=.1, value=1, ticks=FALSE)
        )
    ),
    
    mainPanel(
        div(plotOutput("reg.plot", width=fig.width, height=fig.height)),
        div(class="span7", verbatimTextOutput("reg.summary"))
    )
    
))


server <- shinyServer(function(input, output) {
    
    # --------------------------------------------------------------------------
    # This is where a new sample is instigated only random noise is required to be generated
    random.sample <- reactive({
        # Dummy line to trigger off button-press
        foo <- input$resample
        n  <- input$z
        noise2 <- rnorm(2*n, 0, input$f)  # between
        noise1 <- rnorm(4*n, 0, input$e)  # within
        return(list(noise1=noise1, noise2=noise2))
    })
    
    # --------------------------------------------------------------------------
    # Set up the dataset based on the inputs 
    make.regression <- reactive({
        
        sample <- random.sample()
        
        n <- input$z
        beta <- c(input$a, input$b, input$c, input$d)
        Patient   <- as.factor(rep(1:(2*n), rep(2, 2*n)))
        Treatment <- c(rep(c("Treatment1", "Treatment2"), n),
                       rep(c("Treatment2", "Treatment1"), n))
        Order     <- rep(c("First", "Second"), 2*n)
        Data      <- data.frame(Patient, Treatment, Order)
        FMat      <- model.matrix(~ Treatment * Order, data=Data)
        RMat      <- model.matrix(~ 0 + Patient, data=Data)
        Response  <- FMat %*% beta + RMat %*% sample$noise2 + sample$noise1  # beta here
        Data$Response <- Response
        df <- as.data.frame(Data)
        return(list(df=df))     
        
    })  
    
    # --------------------------------------------------------------------------
    # Fit the specified regression model
    fit.regression <- reactive({
        
        # Get the current model structure
        data <- make.regression()
        
        df <- data$df
        
        # Conditionally fit the model
        if (input$model == "Simple regression") {
            
            fit.res<-nlme::lme(Response~  Treatment  , random=~1 | Patient,
                               data=df,
                               method="REML") 
            
        } else if (input$model == "Additive model") {
            
            fit.res<-nlme::lme(Response~  Treatment  , random=~1 | Patient,
                               data=df,
                               method="REML") 
            
            #    fit.res <- lm(y ~ x + group, df)
        } else if (input$model == "Interactive model") {
            fit.res<-nlme::lme(Response~  Treatment * Order, random=~1 | Patient,
                               data=df,
                               method="REML") 
        } else {
            fit.res <- NULL
        }
        
        # Get the model summary
        if (is.null(fit.res)) {
            fit.summary <- NULL
        } else {
            fit.summary <- summary(fit.res)
        }
        
        return(list(fit.res=fit.res, fit.summary=fit.summary))
        
    })
    
    #---------------------------------------------------------------------------
    # Plot a scatter of the data  
    output$reg.plot <- renderPlot({         
        
        # Get the current regression data
        data <- make.regression()
        
        
        d1 <- data$df
        require(reshape)
        d1$grp <- paste(d1$Treatment, d1$Order, sep=" ")
        d1 <- rename(d1, c(Response="count"))
        d1 <- rename(d1, c(grp="spray"))
        # https://stackoverflow.com/questions/17031039/how-to-sort-a-character-vector-according-to-a-specific-order
        ord <-  c("Treatment1 First", "Treatment2 Second", "Treatment2 First", "Treatment1 Second")
        d1$spray <- factor(d1$spray,levels=ord)
        d1 <- d1[order(d1$spray),]
        
        # create jitter vars
        d1$xj <- cumsum(c(TRUE, d1$spray[-1]!=d1$spray[-length(d1$spray)])) # numeric grouping var 1,2,3,4
        d1$xj <- d1$xj+rnorm(nrow(d1), 0, .10)
        d1$yj <- d1$count +  rnorm(nrow(d1), 0, .15) 
        
        
        
        attach(d1)
        
        sprayTypes <- unique(spray)
        
        y <- as.numeric(as.character(d1[,4]))
        
        plot(y, as.factor(d1[,5]) ,  ylim=c(min(y)-0,max(y)+0) , xlim=c(0.5, 4.5) , xaxt="n",
             main="Plot of AB sequence on left, BA on right\n Dashed lines join paired data. \n(Small amount of jitter added to data).",
             xlab="", ylab="Outcome measure", frame.plot  =F , col="white")  
        ##if not white I get a nasty boxplot
        
        axis(1,  at=1:4, labels=F )
        
        text(x=1:length(sprayTypes)*1.15, par("usr")[3]-.8, labels = sprayTypes, 
             srt = 15, pos = 2, xpd = TRUE,  
             cex.axis=.5 )
        
        ##make colours
        chk <- as.character(d1$spray)
        x <- as.data.frame(table(chk))
        
        clr <- c("blue","red")
        
        
        for (i in 1 : length(sprayTypes)) {
            
            n <- sum(spray == sprayTypes[i])
            
            y <- count[spray == sprayTypes[i]]  
            
            xi <- xj[spray == sprayTypes[i]]
            
            yi <- yj[spray == sprayTypes[i]]
            
            points(x=xi, y=yi, pch = 16, cex = .9, 
                   col =ifelse(i %in% c(1,4),clr[1],clr[2])
            )
            
            # y is used here, decided not to show this
            # lines(i +  c(.12, .28), rep(mean(y), 2), lwd = 2, col="darkgreen") # 
            
            # lines(rep(i + .2, 2),  # start and end
            #         mean(y) + c(-1.96, 1.96) * sd(y) / sqrt(n),  lwd = 2, col="darkgreen"
            #   ) # vertical
            
        }
        
        
        # join the pairs
        # manage the data, create a new wide data set of 4 coordinates
        
        w1 <- d1[,c(1,3,6,7)]
        
        data1 <- melt(w1 , id.vars = c("Patient", "Order"))
        
        data1$temp <- paste(data1$Order, data1$variable)
        
        wx <- reshape(data1[,c(1,4,5)], idvar="Patient",
                      v.names="value", timevar=c("temp"),
                      direction="wide")
        
        wx <- wx[,c(1,2,4,3,5)]
        
        names(wx) <- c("Patient","x1","y1","x2","y2")
        
        
        for(s in 1:nrow(wx)) {
            
            segments(wx$x1[s], wx$y1[s], wx$x2[s], wx$y2[s], col="brown", lwd=0.5, lty=2)
            
        }
        
        
    })
    
    #---------------------------------------------------------------------------
    # Show the summary for the 
    output$reg.summary <- renderPrint({
        
        summary <- fit.regression()$fit.summary
        
        if (!is.null(summary)) {
            
            return(fit.regression()$fit.summary)
        }
        
    })
    
    
    # output$table <- renderTable({
    #     data <- make.regression()
    #     data$df()
    # })
    
})



# Run the application 
shinyApp(ui = ui, server = server)