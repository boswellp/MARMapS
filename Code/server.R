#===================================================================================#
#-----------------------------------------------------------------------------------#
# This is the server for MARMapS v 1.1. Tutorials and instructions can              #
# be found at https://github.com/galengorski/MARMapS. Questions can be sent to      #
# ggorski@ucsc.edu                                                                  #
#-----------------------------------------------------------------------------------#
# GG                                                                                #
# 1/20/2020                                                                         #
#-----------------------------------------------------------------------------------#
#===================================================================================#


#===================================================================================#
#####SERVER#####
function(input, output, session) {
  #increase the maximum upload size to 30mb
  options(shiny.maxRequestSize=30*1024^2)
  
  #===============================================================#
  #####INITIAL ATTRIBUTE LAYER#####
  
  #---------------------------------------------------------------#
  #####Set the Reactive Values#####
  reclass <-   reactiveValues(gf = NA,   gt = NA,   bf = NA,   bt = NA)
  raster.list <- reactiveValues()
  composite <- reactiveValues(switch = NA)
  init_w <- reactiveValues(initial = 1)
  #####
  #---------------------------------------------------------------#
  
  #---------------------------------------------------------------#
  #####make the raster input box have a prompting message and ####
  #make myRaster() the primary plotting raster
  myRaster <- reactive({
    validate(
      need(input$file1, "Select a raster file to load")
    )
    inF <- input$file1
    raster(inF$datapath)
  })
  #####
  #---------------------------------------------------------------#
  
  #---------------------------------------------------------------#
  ####Raster code input message and upload box####
  myRastercodes <- reactive({
    validate(
      need(input$file_rast_codes, "Select a Raster File to Load")
    )
    inF <- input$file_rast_codes
    read.csv(inF$datapath, header = T, stringsAsFactors = F) 
  })
  #####
  #---------------------------------------------------------------#    
  
  #---------------------------------------------------------------#
  #####choose the plotting scale####
  #this is a dropdown menu that allows you to choose the plotting
  #scale, the choice is coded as the object input$scale_choice
  output$change_scale <- renderUI({
    z <- getValues(myRaster())
    z <- z[is.finite(z)]
    z <- round(z, digits=1)
    
    quant <- round(unique(quantile(z, seq(0,1, length.out=10), na.rm = T)), digits = 2)
    selectInput('scale_choice', "Choose Plotting Scale",
                choices = c('Decile','Linear','Log10','Ln','Categorical'), selected = 'Decile')
  })
  #####
  #---------------------------------------------------------------#
  
  #---------------------------------------------------------------#
  #####make the plotting scale####
  #this function receives input from the change_scale
  #creates a reactive object called plotting_scale()
  plotting_scale <- reactive({
    
    z <- getValues(myRaster()) %>%
      subset(.,!is.na(.)) %>%
      round(., digits=2)
    
    if(is.null(input$scale_choice)){
      NULL
    }else if(input$scale_choice == 'Decile'){
      #keep it at deciles
      round(unique(quantile(z, seq(0,1, length.out=10), na.rm = T)), digits = 2)
      
    }else if(input$scale_choice == 'Linear'){
      #make it linear
      #if the linear sequence is less than 3 make the length.out = 10
      if(length(seq(min(z), max(z))<3)){
        seq(min(z), max(z), length.out = 10) %>%
          round(digits = 2)
        
      }else{
        seq(min(z), max(z)) %>%
          round(digits = 2)
        
      }
    }else if(input$scale_choice == 'Log10'){
      #make it log
      #if the log sequence is less than three then make the length.out = 10
      if(length(10^(seq(log10(min(z[z != 0])), log10(max(z)))))<3){
        if(min(z) == 0){
          10^(seq(log10(min(z[z != 0])), log10(max(z)), length.out = 10))%>%
            round(digits = 2) %>%
            c(0,.)
        }else{
          10^(seq(log10(min(z)), log10(max(z)), length.out = 10))%>%
            round(digits = 2) 
          
        }
        
      }else{
        if(min(z) == 0){
          10^(seq(log10(min(z[z != 0])), log10(max(z))))%>%
            round(digits = 2) %>%
            c(0,.)
        }else{
          10^(seq(log10(min(z)), log10(max(z)), length.out = 10))%>%
            round(digits = 2) 
          
        }
      }
    }else if(input$scale_choice == 'Ln'){
      #make it natural log
      #if the ln sequence is less than three then make the length.out = 10
      if(length(exp(seq(log(min(z[z != 0])), log(max(z)))))<3){
        #if the lowest value is 0
        if(min(z) == 0){
          #get the second lowest value make a scale
          exp(seq(log(min(z[z != 0])), log(max(z)), length.out = 10))%>%
            round(digits = 2) %>%
            #and append a zero at the beginning
            c(0,.)
        }else{
          #if zero isn't the lowest value then just make a simple scale
          exp(seq(log(min(z)), log(max(z)), length.out = 10))%>%
            round(digits = 2) 
          
        }
        
      }else{
        if(min(z) == 0){
          exp(seq(log(min(z[z != 0])), log(max(z))))%>%
            round(digits = 2) %>%
            c(0,.)
        }else{
          exp(seq(log(min(z)), log(max(z)), length.out = 10))%>%
            round(digits = 2) 
          
        }
      }
    }else if(input$scale_choice == 'Categorical'){
      if(length(unique(z)) > 30){
        renderUI({
          
        })
      }
      #make it a categorical scale
      sort(unique(z))
    }
  })
  #####
  #---------------------------------------------------------------#
  
  #---------------------------------------------------------------#
  #####make the slider for the good range####
  #this produces output val_g that are then used to make the raster
  #reclassification matrix 
  output$good_range <- renderUI({
    quant <- plotting_scale()
    if(is.null(quant)){
      
    }else{
      
      if(check.null(input$scale_choice)){
        sliderTextInput("val_g",paste('Good Range',input$data_units),
                        choices=quant,
                        grid = T, selected = c(quant[1], quant[length(quant)]))
      }else{
        boxes <- quant
        if(is.null(input$file_rast_codes)){
          checkboxGroupInput("val_g", "Select the Good Values",
                             choices = c(quant))
        }else{
          rCodes <- myRastercodes()
          checkboxGroupInput("val_g", "Select the Good Values",
                             choiceNames = c(rCodes[,'code']),
                             choiceValues = c(rCodes[,'ID']))
        }
      }
    }
  })
  #####
  #---------------------------------------------------------------#
  
  #===============================================================#
  #####Make a new good range slider####
  range_count <- 0
  #when the button is clicked observe event
  observeEvent(input$add_range,{
    if(check.null(input$scale_choice)){
      range_count <<- range_count + 1
      #id_r is the index that we use to refer to the number of ranges added 
      #and the values of the sliders
      id_r <- paste0("range", range_count)
      #insertUI is the action taken when we observe an event
      #done at #add_range from the UI
      insertUI(selector = '#add_range',
               where = 'beforeBegin',
               #the only thing we are doing is adding one slider then updating the 
               #reclass matrix
               ui = uiOutput(paste0('new_range', id_r))
      )
      #here is the slider that we are adding, the output will be indexed as val_g,id_r
      output[[paste0('new_range',id_r)]] <- renderUI({
        quant <- plotting_scale()
        sliderTextInput(paste0("val_g",id_r),paste('Good Range',input$data_units),
                        choices=quant,
                        grid = T, selected = c(quant[1], quant[length(quant)]))
      })
      #now when we hit the classify button we want both the initial val_g values and the 
      #new values from the new ranges to be input into the reclass matrix
      observeEvent(input[['classify']],{
        rast_min <- min(values(myRaster()), na.rm = T)-abs(min(values(myRaster()), na.rm = T)*0.005)
        rast_max <- max(values(myRaster()), na.rm = T)+abs(max(values(myRaster()), na.rm = T)*0.005)
        #reclassify the good values based on slider inputs
        #the reclass$gf and reclass$gt vectors must also include themselves so that for example if 
        #3 slider ranges are used, the second set of slider values is stored
        reclass$gf <- c(reclass$gf, input[["val_g"]][1],input[[paste0("val_g",id_r)]][1])
        reclass$gt <- c(reclass$gt, input[["val_g"]][2],input[[paste0("val_g",id_r)]][2])
        #make the bad values everything else
        reclass$bf <- rast_min
        reclass$bt <- rast_max
        #insert the slider for weights in the third panel
        output[['weights']] <- renderUI({
          sliderInput('w', label = input$data_name, value = 1/(input$addBtn+1), min = 0, max = 1)
        })
      })
    }else{
      #Do nothing if you try to add another range with a categorical variable
    }
  })
  
  #####
  #===============================================================#
  
  #---------------------------------------------------------------#
  #####make the raw map of the data####
  output$plot1 <- renderPlot({
    quant <- plotting_scale()
    #get all the raster values
    z <- getValues(myRaster())
    #collect the non NA values
    z <- z[is.finite(z)]
    #round them to two digits
    z <- round(z, digits=2)
    
    #if the scale choice dropdown hasn't been activated plot the default
    #which is decile plotting
    if(is.null(input$scale_choice)){
      my.at=c(quant)
      my.brks=seq(0,length(quant)-1)
      myColorkey <- list(at=my.brks, labels=list(at=my.brks, labels=my.at, cex = 1.0), height = 0.60, space="right")
      cols <- brewer.pal(8, 'Spectral')
      mapTheme <- rasterTheme(region=cols, axis.line = list(col = 'transparent'))  
      p = levelplot(myRaster(), par.settings=mapTheme, at=my.at, colorkey=myColorkey, margin=F, scales = list(draw = F),
                    main = '')
      print(p)
      #if Categorical has been selected
    }else if(input$scale_choice == 'Categorical'){
      #check if the raster codes have been uploaded
      if(is.null(input$file_rast_codes)){
        #if they haven't then still plot but just use the 
        #plotting scale as the codes for now
        raster.f <- as.factor(myRaster())
        levels(raster.f)[[1]][,'code'] <- quant
      }else{
        #if they have been uploaded then use the codes as plotting
        #symbols, in both cases (this and above) changing the raster to
        #a factor seemed to work the best
        raster.f <- as.factor(myRaster())
        x <- myRastercodes()
        levels(raster.f)[[1]][,'code'] <- x[,2]
      }
      
      #either way the raster plotting is similar
      myColorkey <- list(height = 0.60, space="right", labels=list(cex=1))
      mapTheme <- rasterTheme(region=cols, axis.line = list(col = 'transparent'))  
      p = levelplot(raster.f, par.settings=mapTheme, colorkey=myColorkey,margin=F, scales = list(draw = F))
      print(p)
    }else{
      #if the scale choice dropdown has been chosen and its not Categorical this the 
      #the plotting chunk that will be used
      my.at=c(quant)
      my.brks=seq(0,length(quant)-1)
      myColorkey <- list(at=my.brks, labels=list(at=my.brks, labels=my.at, cex = 1.0), height = 0.60, space="right")
      cols <- brewer.pal(8, 'Spectral')
      mapTheme <- rasterTheme(region=cols, axis.line = list(col = 'transparent'))  
      p = levelplot(myRaster(), par.settings=mapTheme, at=my.at, colorkey=myColorkey, margin=F, scales = list(draw = F),
                    main = '')
      print(p)
    }
    
    #for all instances this is the title and legend title used
    trellis.focus("legend", side="right", clipp.off=TRUE, highlight=FALSE)
    grid.text(input$data_units, 0, 0.95, hjust=0, vjust=1, gp=gpar(fontsize = 14))
    trellis.unfocus()
    grid.text(paste(input$data_name,'Map', sep = ' '), 0.5, 1, hjust=0, vjust=1, gp=gpar(fontsize = 20))
    
  })
  #####
  #---------------------------------------------------------------#
  
  #---------------------------------------------------------------#
  #####Classify Button Matrix Population#####
  #If no new range is created (i.e. only 1 range)
  #when reclassify is clicked, this matrix is repopulated
  observeEvent(input$classify,{
    
    #pad the min and max by 0.5% to make sure it gets everything
    rast_min <- min(values(myRaster()), na.rm = T)-abs(min(values(myRaster()), na.rm = T)*0.005)
    rast_max <- max(values(myRaster()), na.rm = T)+abs(max(values(myRaster()), na.rm = T)*0.005)
    
    #make the bad values everything else
    reclass$bf <- rast_min
    reclass$bt <- rast_max
    
    #check if scale choice is null or categorical
    if(check.null(input$scale_choice)){
      
      #if the good range starts at the beginning of the scale or ends at the end
      #pad it with 5% to make sure it encompasses the whole range of values
      quant <- plotting_scale()
      
      if(input$val_g[1] == quant[1]){
        reclass$gf <- min(values(myRaster()), na.rm = T)-abs(min(values(myRaster()), na.rm = T)*0.005)
      }else{
        #reclassify the good values based on slider inputs
        reclass$gf <- input$val_g[1]
      }
      
      if(input$val_g[2] == quant[length(quant)]){
        reclass$gt <- max(values(myRaster()), na.rm = T)+abs(max(values(myRaster()), na.rm = T)*0.005)
      }else{
        #reclassify the good values based on slider inputs
        reclass$gt <- input$val_g[2]
      }
    }else{
      #if the scale choice is categorical
      #make the good to and good from values a vector of those chosen 
      #check boxes
      reclass$gf <- as.numeric(input$val_g)
      reclass$gt <- as.numeric(input$val_g)
    }
    
    
    #insert the slider for weights in the third panel
    output[['weights']] <- renderUI({
      sliderInput('w', label = input$data_name, value = 1/(input$addBtn+1), min = 0, max = 1)
    })
  })
  #####
  #---------------------------------------------------------------#
  
  #---------------------------------------------------------------#
  #####Plot the Ranked Properties Based on Slider Inputs#####
  output$plot2 <- renderPlot({
    #if the reclassify button has not yet been pushed plot a map of all green
    if(is.na(reclass$bf)){
      #all green map
      suit = levelplot(myRaster(), margin = F, col.regions=c('palegreen') ,
                       colorkey=list(at = c(0,0.5,1),
                                     labels=list(at=c(0.25,0.75),labels=c("Poor", "Good"), cex = 1.7), 
                                     col = c('indianred1','palegreen'), height = 0.25),
                       main = '', par = rasterTheme(axis.line = list(col = 'transparent')), scales = list(draw = F))
      print(suit)
      grid.text(paste(input$data_name,'Suitability', sep = ' '), 0.5, 1.0, hjust=0, vjust=1, gp=gpar(fontsize = 20))
    }else{
      rcl.mat <- matrix(c(reclass$gf,reclass$bf,
                          reclass$gt,reclass$bt,
                          rep(1,length(reclass$gt)),0), ncol = 3)
      #use the matrix to reclassify the raster
      raster_reclass <- reclassify(myRaster(), rcl = rcl.mat, right = NA)
      raster.list[['raster1']] <<- raster_reclass
      #give the levels codes instead of just numbers
      levels(raster_reclass) <- data.frame(ID = c(0,1), code = c('Poor','Good'))
      #make a plot with the different classes
      suit = levelplot(raster_reclass,col.regions=c('indianred1','palegreen'), 
                       main = '', 
                       colorkey = list(height = 0.25, labels = list(cex = 1.7)), par = rasterTheme(axis.line = list(col = 'transparent')), scales = list(draw = F))
      print(suit)
      grid.text(paste(input$data_name,'Suitability', sep = ' '), 0.5, 1.0, hjust=0, vjust=1, gp=gpar(fontsize = 20))
    }
  })
  #####
  #---------------------------------------------------------------#
  
  
  #---------------------------------------------------------------#
  #####This is here as a check to print outputs if need####
  
  output$reclass_matrix <- renderText({ 
    # if(is.null(input$file_rast_codes)){
    #   print(plotting_scale())
    # }else{
    #   b <- myRastercodes()
    #   print(b[,1])
    # }
    
  })
  #####
  #---------------------------------------------------------------#
  #####
  #===============================================================#
  
  
  #===============================================================#
  #####ADDING ADDITIONAL ATTRIBUTE LAYERS#####
  ## keep track of elements inserted and not yet removed
  #plot_count is the number of plots added with the dynamic UI buttons
  plot_count <- 0
  reclass_nl <- reactiveValues(gf = NA,   gt = NA,   bf = NA,   bt = NA)
  
  #---------------------------------------------------------------#
  #####This builds a whole new shiny app when the add button is clicked####
  observeEvent(input$addBtn, {
    #the <<- indicates that plot_count will be recognized outside of observe event (I think this is true)
    #each time you click the Add Layer button, plot_count increases by 1
    plot_count <<- plot_count + 1
    composite$switch <- 1
    #id is the index that we use to refer to the plots and the input values from the fileInput calls
    id <- paste0("item", plot_count)
    
    #---------------------------------------------------------------#
    #####Build a new user interface which should look the same as for####
    #the previoius attribute
    insertUI(
      selector = '#placeholder',
      where = 'beforeBegin',
      ## wrap element in a div with id for ease of removal
      ui = tags$div(
        #fileInput(id, "Upload file", multiple = FALSE),
        h4('Additional Factor'),
        fluidRow(column(4,
                        fileInput(id, "Upload GeoTiff File",accept = c('.tiff')),
                        fileInput(paste0("file_rast_codes",id),"Upload Raster Codes", accept = c('.csv')),
                        textInput(paste0("data_name",id), "Name", " "),
                        textInput(paste0("data_units",id), "Units", " "),
                        uiOutput(paste0('good_range',id)),
                        actionButton(paste0('add_range_nl',id), 'Add New Range'),
                        actionButton(paste0('classify',id),'Classify'),
                        uiOutput(paste0('change_scale',id))
        ),
        column(4,
               plotOutput(paste0('rawmap',id), height = '600px')),
        column(4,
               plotOutput(paste0('classified',id),height = '600px')))
      ))
    #####
    #---------------------------------------------------------------#
    
    #reclass is a matrix used to reclassify the maps
    reclass_nl <-   reactiveValues(gf = NA,   gt = NA,   bf = NA,   bt = NA)
    
    #---------------------------------------------------------------#
    #####Raster input message and upload box####
    #if an input raster hasn't been read in yet this will prompt you, then it will 
    #turn the input raster into a reactive object that can be worked with within the observe event space
    myRaster_nl <- reactive({
      validate(
        need(input[[id]], "Select a raster file to load")
      )
      inF <- input[[id]]
      raster(inF$datapath)
    })
    #####
    #---------------------------------------------------------------#
    
    #---------------------------------------------------------------#
    ####Raster code input message and upload box####
    myRastercodes_nl <- reactive({
      validate(
        need(input[[paste0("file_rast_codes",id)]], "Select a Raster File to Load")
      )
      inF <- input[[paste0("file_rast_codes",id)]]
      read.csv(inF$datapath, header = T, stringsAsFactors = F) 
    })
    #####
    #---------------------------------------------------------------#    
    
    #---------------------------------------------------------------#
    ####Choose the plotting scale####
    output[[paste0('change_scale',id)]] <- renderUI({
      z <- getValues(myRaster_nl())
      z <- z[is.finite(z)]
      z <- round(z, digits=1)
      
      quant <- round(unique(quantile(z, seq(0,1, length.out=10), na.rm = T)), digits = 2)
      selectInput(paste0('scale_choice',id), "Choose Plotting Scale",choices = c('Decile','Linear','Log10','Ln','Categorical'), selected = 'Decile')
    })
    
    #####
    #---------------------------------------------------------------#
    
    #---------------------------------------------------------------#
    ####Makes a dynamic object called plotting_scale_nl that ####
    #is used to make a scale bar to choose the good range
    plotting_scale_nl <- reactive({
      
      
      z <- getValues(myRaster_nl()) %>%
        subset(.,!is.na(.)) %>%
        round(., digits=2)
      if(is.null(input[[paste0('scale_choice',id)]])){
        
      }else if(input[[paste0('scale_choice',id)]] == 'Decile'){
        #keep it at deciles
        round(unique(quantile(z, seq(0,1, length.out=10), na.rm = T)), digits = 2)
        #pad quant by 0.5% to make sure it encompasses all values to account for rounding
        
      }else if(input[[paste0('scale_choice',id)]] == 'Linear'){
        #make it linear
        #if the linear sequence is less than 2 make the length.out = 10
        if(length(seq(min(z), max(z))<3)){
          seq(min(z), max(z), length.out = 10) %>%
            round(digits = 2)
          
        }else{
          seq(min(z), max(z)) %>%
            round(digits = 2)
          
        }
      }else if(input[[paste0('scale_choice',id)]] == 'Log10'){
        #make it log
        #if the log sequence is less than three then make the length.out = 10
        if(length(10^(seq(log10(min(z[z != 0])), log10(max(z)))))<3){
          if(min(z) == 0){
            10^(seq(log10(min(z[z != 0])), log10(max(z)), length.out = 10))%>%
              round(digits = 2) %>%
              c(0,.)
          }else{
            10^(seq(log10(min(z)), log10(max(z)), length.out = 10))%>%
              round(digits = 2) 
            
          }
          
        }else{
          if(min(z) == 0){
            10^(seq(log10(min(z[z != 0])), log10(max(z))))%>%
              round(digits = 2) %>%
              c(0,.)
          }else{
            10^(seq(log10(min(z)), log10(max(z)), length.out = 10))%>%
              round(digits = 2) 
            
          }
        }
      }else if(input[[paste0('scale_choice',id)]] == 'Ln'){
        #make it natural log
        #if the ln sequence is less than three then make the length.out = 10
        if(length(exp(seq(log(min(z[z != 0])), log(max(z)))))<3){
          #if the lowest value is 0
          if(min(z) == 0){
            #get the second lowest value make a scale
            exp(seq(log(min(z[z != 0])), log(max(z)), length.out = 10))%>%
              round(digits = 2) %>%
              #and append a zero at the beginning
              c(0,.)
          }else{
            #if zero isn't the lowest value then just make a simple scale
            exp(seq(log(min(z)), log(max(z)), length.out = 10))%>%
              round(digits = 2) 
            
          }
          
        }else{
          if(min(z) == 0){
            exp(seq(log(min(z[z != 0])), log(max(z))))%>%
              round(digits = 2) %>%
              c(0,.)
          }else{
            exp(seq(log(min(z)), log(max(z)), length.out = 10))%>%
              round(digits = 2) 
            
          }
        }
        #if categorical is selected
      }else if(input[[paste0('scale_choice',id)]] == 'Categorical'){
        #make it a categorical scale
        sort(unique(z))
      }
    })
    #####
    #---------------------------------------------------------------#
    
    #---------------------------------------------------------------#
    ######make a dyanamic slider bar for the newly added layer####
    #make the slider for the good range#
    #this produces output val_g that are then used to make the raster
    #reclassification matrix 
    output[[paste0('good_range',id)]] <- renderUI({
      quant <- plotting_scale_nl()
      
      if(check.null(input[[paste0('scale_choice',id)]])){
        sliderTextInput(paste0("val_g",id),label = paste('Good Range',input[[paste0('data_units',id)]], sep = ' '),
                        choices=quant,
                        grid = T, selected = c(quant[1], quant[length(quant)]))
      }else{
        boxes <- quant
        if(is.null(input[[paste0("file_rast_codes",id)]])){
          checkboxGroupInput(paste0("val_g",id), "Select the Good Values",
                             choices = c(quant))
        }else{
          rCodes <- myRastercodes_nl()
          checkboxGroupInput(paste0("val_g",id), "Select the Good Values",
                             choiceNames = c(rCodes[,'code']),
                             choiceValues = c(rCodes[,'ID']))
        }
      }
    })
    #####
    #---------------------------------------------------------------#
    
    #===============================================================#
    #####Make a new good range slider####
    range_count_nl <- 0
    observeEvent(input[[paste0('add_range_nl',id)]],{
      if(check.null(input[[paste0('scale_choice',id)]])){
        range_count_nl <<- range_count_nl + 1
        #id is the index that we use to refer to the plots and the input values from the fileInput calls
        id_nl <- paste0("range", range_count_nl, id)
        
        insertUI(selector = paste0('#add_range_nl',id),
                 where = 'beforeBegin',
                 ## wrap element in a div with id for ease of removal
                 ui = uiOutput(paste0('new_range_nl', id_nl))
        )
        output[[paste0('new_range_nl',id_nl)]] <- renderUI({
          quant <- plotting_scale_nl()
          sliderTextInput(paste0("val_g",id_nl), label = paste('Good Range',input[[paste0('data_units',id)]], sep = ' '),choices=quant,
                          grid = T, selected = c(quant[1], quant[length(quant)]))
        })
        
        
        observeEvent(input[[paste0('classify',id)]],{
          rast_min <- min(values(myRaster_nl()), na.rm = T)-abs(min(values(myRaster_nl()), na.rm = T)*0.005)
          rast_max <- max(values(myRaster_nl()), na.rm = T)+abs(max(values(myRaster_nl()), na.rm = T)*0.005)
          
          #pad the good to and from with 5% on either side
          
          reclass_nl$gf <- c(reclass_nl$gf,input[[paste0("val_g",id)]][1],input[[paste0("val_g",id_nl)]][1])
          reclass_nl$gt <- c(reclass_nl$gt,input[[paste0("val_g",id)]][2],input[[paste0("val_g",id_nl)]][2])
          
          
          #make the bad values everything else
          reclass_nl$bf <- rast_min
          reclass_nl$bt <- rast_max
        })
      }else{
        #Don't add another scale if categorical has been checked
      }
      insertUI(
        selector = '#weights_nl',
        where = 'afterEnd',
        ui = uiOutput(paste0('weights',id))
      )
      output[[paste0('weights',id)]] <- renderUI({
        sliderInput(inputId = paste0('w',id), label = input[[paste0('data_name',id)]], min = 0, max = 1,value = 1/(input$addBtn+1))
      })
      
    })
    ##$$$$$$$$$$$$$#####input[[paste0('w','item',i)]]
    #####
    #===============================================================#
    
    #---------------------------------------------------------------#
    ######make raw map of data#####
    output[[paste0('rawmap',id)]] <- renderPlot({
      quant <- plotting_scale_nl()
      #get all the raster values
      z <- getValues(myRaster_nl())
      #get all the non-NA raster values    
      z <- z[is.finite(z)]
      #round them to 2 digits
      z <- round(z, digits=2)
      
      #if the scale choice dropdown hasn't been activated plot the default
      #which is decile plotting
      if(is.null(input[[paste0('scale_choice',id)]])){
        my.at=c(quant)
        my.brks=seq(0, length(quant)-1)
        cols <- brewer.pal(8, 'Spectral')
        mapTheme <- rasterTheme(region=cols, axis.line = list(col = 'transparent'))  
        
        myColorkey <- list(at=my.brks, labels=list(at=my.brks, labels=my.at, cex = 1.0), height = 0.60, space="right")
        p = levelplot(myRaster_nl(), par.settings=mapTheme, at=my.at, colorkey=myColorkey, margin=F, scales = list(draw = F),
                      main = '')       
        print(p)
        #if categorical has been selected
      }else if(input[[paste0('scale_choice',id)]] == 'Categorical'){
        #check if the raster codes have been uploaded
        if(is.null(input[[paste0("file_rast_codes",id)]])){
          #if they haven't then still plot but just use the 
          #plotting scale as the codes for now
          raster.f <- as.factor(myRaster_nl())
          levels(raster.f)[[1]][,'code'] <- quant
        }else{
          #if they have been uploaded then use the codes as plotting
          #symbols, in both cases (this and above) changing the raster to
          #a factor seemed to work the best
          raster.f <- as.factor(myRaster_nl())
          x <- myRastercodes_nl()
          levels(raster.f)[[1]][,'code'] <- x[,2]
        }
        #either way the raster plotting is similar
        myColorkey <- list(height = 0.60, space="right", labels=list(cex=1))
        mapTheme <- rasterTheme(region=cols, axis.line = list(col = 'transparent'))  
        p = levelplot(raster.f, par.settings=mapTheme, colorkey=myColorkey,margin=F, scales = list(draw = F))
        print(p)
      }else{
        #if the scale choice dropdown has been chosen and its not Categorical this the 
        #the plotting chunk that will be used
        my.at=c(quant)
        my.brks=seq(0,length(quant)-1)
        myColorkey <- list(at=my.brks, labels=list(at=my.brks, labels=my.at, cex = 1.0), height = 0.60, space="right")
        cols <- brewer.pal(8, 'Spectral')
        mapTheme <- rasterTheme(region=cols, axis.line = list(col = 'transparent'))  
        p = levelplot(myRaster_nl(), par.settings=mapTheme, at=my.at, colorkey=myColorkey, margin=F, scales = list(draw = F),
                      main = '')
        print(p)
      }
      
      #for all instances this is the title and legend title used
      trellis.focus("legend", side="right", clipp.off=TRUE, highlight=FALSE)
      grid.text(input[[paste0('data_units',id)]], 0, 0.95, hjust=0, vjust=1, gp=gpar(fontsize = 18))
      trellis.unfocus()
      grid.text(paste(input[[paste0('data_name',id)]],'Map', sep = ' '), 0.5, 1.0, hjust=0, vjust=1, gp=gpar(fontsize = 20))
    })
    #####
    #---------------------------------------------------------------#
    
    #---------------------------------------------------------------#
    #####Classify Button Matrix Population#####
    #If no new range is created (i.e. only 1 range)
    #when classify is clicked, this matrix is repopulated
    observeEvent(input[[paste0('classify',id)]],{
      
      rast_min <- min(values(myRaster_nl()), na.rm = T)-abs(min(values(myRaster_nl()), na.rm = T)*0.005)
      rast_max <- max(values(myRaster_nl()), na.rm = T)+abs(max(values(myRaster_nl()), na.rm = T)*0.005)
      #make the bad values everything else
      reclass_nl$bf <- rast_min
      reclass_nl$bt <- rast_max
      
      #check if the scale is null or categorical
      if(check.null(input[[paste0('scale_choice',id)]])){
        quant <- plotting_scale_nl()
        
        #pad the good values by 0.5% to make sure they encompass the right values
        if(input[[paste0("val_g",id)]][1] == quant[1]){
          reclass_nl$gf <- min(values(myRaster_nl()), na.rm = T)-abs(min(values(myRaster_nl()), na.rm = T)*0.005)
        }else{
          reclass_nl$gf <- input[[paste0("val_g",id)]][1]
        }
        if(input[[paste0("val_g",id)]][2] == quant[length(quant)]){
          reclass_nl$gt <- max(values(myRaster_nl()), na.rm = T)+abs(max(values(myRaster_nl()), na.rm = T)*0.005)
        }else{
          reclass_nl$gt <- input[[paste0("val_g",id)]][2]
        }
      }else{
        #if it is categorical make the good to and good from
        #values a vector of the checked check boxes
        reclass_nl$gf <- as.numeric(input[[paste0('val_g',id)]])
        reclass_nl$gt <- as.numeric(input[[paste0('val_g',id)]])
      }
      
      #each time the classify button is clicked, a weights slider is added to the last tab
      insertUI(
        selector = '#weights_nl',
        where = 'beforeEnd',
        ui = tags$div(uiOutput(paste0('weights',id)))
      )
      output[[paste0('weights',id)]] <- renderUI({
        sliderInput(inputId = paste0('w',id), value = 1/(input$addBtn+1), label = input[[paste0('data_name',id)]], min = 0, max = 1)
      })
    })
    #####
    #---------------------------------------------------------------#
    
    #---------------------------------------------------------------#
    #####Plot the Ranked Properties Based on Slider Inputs#####
    output[[paste0('classified',id)]] <- renderPlot({
      #if the reclassify button has not yet been pushed plot a map of all green
      if(is.na(reclass_nl$bf)){
        #all green map
        p = levelplot(myRaster_nl(), margin = F, col.regions=c('palegreen') ,
                      colorkey=list(at = c(0,0.5,1),
                                    labels=list(at=c(0.25,0.75),labels=c("Poor", "Good"), cex = 1.7),
                                    col = c('indianred1','palegreen'), height = 0.25),
                      main = '', par = rasterTheme(axis.line = list(col = 'transparent')), scales = list(draw = F))
        print(p)
        grid.text(paste(input[[paste0('data_name',id)]],'Suitability', sep = ' '), 0.5, 1.0, hjust=0, vjust=1, gp=gpar(fontsize = 20))
        
      }else{
        rcl.mat_nl <- matrix(c(reclass_nl$gf,reclass_nl$bf,
                               reclass_nl$gt,reclass_nl$bt,
                               rep(1,length(reclass_nl$gt)),0), ncol = 3)
        
        # #use the matrix to reclassify the raster
        raster_reclass_nl <- reclassify(myRaster_nl(), rcl = rcl.mat_nl, right = NA)
        raster.list[[paste0('raster',id)]] <<- raster_reclass_nl
        #give the levels codes instead of just numbers
        levels(raster_reclass_nl) <- data.frame(ID = c(0,1), code = c('Poor','Good'))
        #make a plot with the different classes
        p = levelplot(raster_reclass_nl,col.regions=c('indianred1','palegreen'),
                      main = '',
                      colorkey = list(height = 0.25, labels = list(cex = 1.7)), par = rasterTheme(axis.line = list(col = 'transparent')), scales = list(draw = F))
        print(p)
        grid.text(paste(input[[paste0('data_name',id)]],'Suitability', sep = ' '), 0.5, 1.0, hjust=0, vjust=1, gp=gpar(fontsize = 20))
      }
    })
    #####
    #---------------------------------------------------------------#
    
  })
  #####
  #===============================================================#
  
  
  #===============================================================#
  #===============================================================#
  #WEIGHTING AND COMPOSITE MAPPING TAB
  
  #---------------------------------------------------------------#
  #####Even the weights with a button click####
  #THIS IS AN UNUSED FUNCTION AS OF 1/16/2019#
  observeEvent(input$even_weights,{
    output[['weights']] <- renderUI({
      sliderInput('w', label = input$data_name, value = 1/(input$addBtn+1), min = 0, max = 1)
    })
    for(i in 1:input$addBtn){
      insertUI(
        selector = '#weights_nl',
        where = 'beforeEnd',
        ui = tags$div(uiOutput(paste0('weights','item',i)))
      )
      output[[paste0('weights','item',i)]] <- renderUI({
        sliderInput(inputId = paste0('w','item',i), value = 1/(input$addBtn+1), label = input[[paste0('data_name','item',i)]], min = 0, max = 1)
      })
      #output[[paste0('weights','item',2)]] <- renderUI({
      #  sliderInput(inputId = paste0('w','item',2), value = 1/(input$addBtn+1), label = input[[paste0('data_name','item',2)]], min = 0, max = 1)
      #})
    }
  })
  ######
  #---------------------------------------------------------------#
  
  #---------------------------------------------------------------#
  #####Use Ranks and Weights to Make a Composite Suitability Map#####
  comp_rast <- reactive({
    #if addbtn hasn't been clicked
    if(is.na(composite$switch)){
      #the composite raster is just the reclassified raster from the first layer
      raster.list[['raster1']]
    }else{
      if(is.null(input[['w']])){
        #if it has been clicked then composite raster is the first layer multiplied by its weight
        composite.raster <- raster.list[['raster1']]
      }else{
        composite.raster <- raster.list[['raster1']]*input[['w']]
        for(i in 1:input$addBtn){
          #plus all the other layers multiplied by their weights
          composite.raster <- composite.raster + (raster.list[[paste0('raster','item',i)]]*input[[paste0('w','item',i)]])   
        }  
      }
      composite.raster
    }
  })
  ######
  #---------------------------------------------------------------#
  
  #---------------------------------------------------------------#
  #####Plot the Composite Suitability Map#####
  output$test_1 <- renderPlot({
    #   #if the classify button hasn't been clicked
    if(is.na(reclass$gf[1])){
    }else{
      if(is.null(input$sites)){
        myColorkey <- list(labels=list(at=c(0,1), labels=c('Low','High'), cex = 1.4), space="right")
        suit.pallet <- colorRampPalette(c('indianred1','palegreen'), space = 'rgb')
        levelplot(comp_rast(), margin = F, colorkey = myColorkey, col.regions = suit.pallet(120), at = seq(0,1, length.out = 120),par = rasterTheme(axis.line = list(col = 'transparent')), 
                  scales = list(draw = F), main = 'Composite Suitability', cex.main = 1.7)
      }else{
        suit.pallet <- colorRampPalette(c('indianred1','palegreen'), space = 'rgb')
        colfunc <- colorRampPalette(c("indianred1", "palegreen"))
        col.grad <- colfunc(10)
        arg <- list(c(0.1,0.9), labels = c('Low','High'), main = 'Suitability', cex.axis = 1.4)
        plot(comp_rast(), xlab = '',ylab = '', col = col.grad, axes = F, box = F, axis.arg = arg, main = 'Composite Suitability', cex.main = 1.7)
        points(inputSites(), col = 'black', pch = 21, cex = 1.7, bg = 'white')
        pointLabel(coordinates(inputSites()),labels=inputSites()$id, cex = 2)
        
      }
    }
  })
  #####
  #---------------------------------------------------------------#
  
  #---------------------------------------------------------------#
  #####Print the sum of weights####
  output$sum_weights <- renderText({
    if(is.null(input[['w']])){
      
    }else{
      sw <- input[['w']]
      if(is.null(input[[paste0('w','item',1)]])){
        'Sum of weights = 1' 
      }else{
        for(i in 1:input$addBtn){
          sw <- sw + input[[paste0('w','item',i)]]
        }
        if(sw == 1){
          'Sum of weights = 1'
        }else{
          paste('Sum of weights = ', sw, '\n The weights should add up to 1', sep = '')
        }
      }
    }
  })
  #####
  #---------------------------------------------------------------#
  
  #---------------------------------------------------------------#
  #####Make Raster File Reactive Upon Import####
  inputSites <- reactive({
    validate(
      need(input$sites, "No File Loaded")
    )
    shpdf <- input$sites
    tempdirname <- dirname(shpdf$datapath[1])
    for(i in 1:nrow(shpdf)){
      file.rename(shpdf$datapath[i], paste0(tempdirname, "/", shpdf$name[i]))
    }
    map <- readOGR(paste(tempdirname, shpdf$name[grep(pattern = "*.shp$", shpdf$name)], sep = "/"))
    map
  })
  #####
  #---------------------------------------------------------------#
  
  #---------------------------------------------------------------#
  ######Plot the suitability of the shapefile points########
  output$comp_bplot <- renderPlot({
    if(is.na(reclass$gf)[1]){
    }else{
      suit <- raster::extract(comp_rast(), inputSites())
      
      colfunc <- colorRampPalette(c("palegreen", "indianred"))
      col.grad <- colfunc(dim(table(suit)))
      
      barplot(table(suit), names.arg = '', 
              las = 1, ylab = 'Sites', col = rev(col.grad), main = 'Site Composite Suitability', cex.lab = 1.4, cex.names = 1.4, cex.axis = 1.4, cex.main = 1.7)
      mtext('Least \n Suitable', side = 1, adj = 0, cex = 1.7, line = 2)
      mtext('Most \n Suitable', side = 1, adj = 1, cex = 1.7, line = 2)
    }
  })
  #####
  #---------------------------------------------------------------#
  
  #---------------------------------------------------------------#
  #####Plot the Composite Suitability Distribution#####
  output$dist_comp_suit <- renderPlot({
    if(is.na(reclass$gf[1])){
    }else{
      mx <- sum(table(comp_rast()@data@values))
      colfunc <- colorRampPalette(c("palegreen", "indianred"))
      col.grad <- colfunc(dim(table(comp_rast()@data@values)))
      barplot(table(comp_rast()@data@values), main = 'Area Composite Suitability', col = rev(col.grad), las = 1, axes = F, names.arg = '', cex.lab = 1.4, cex.main = 1.7)
      axis(2, labels = seq(0,1,0.1), at = c(0,mx*.1, mx*.2, mx*.3, mx*.4, mx*.5, mx*.6, mx*.7, mx*.8, mx*.9, mx), las = 1, cex.axis = 1.4)
      mtext('Fraction of Area',side = 2, line = 3, cex = 1.4)
      mtext('Least \n Suitable', side = 1, adj = 0, cex = 1.7, line = 2)
      mtext('Most \n Suitable', side = 1, adj = 1, cex = 1.7, line = 2)
    }
  })
  ######
  #---------------------------------------------------------------#
  #---------------------------------------------------------------#
  #####This is here as a check to print outputs if need####
  
  # output$weight_check <- renderText({ 
  #   sw <- input[['w']]
  #   if(is.null(input[[paste0('w','item',1)]])){
  #     'Sum of weights = 1' 
  #   }else{
  #     for(i in 1:input$addBtn){
  #       sw <- sw + input[[paste0('w','item',i)]]
  #     }
  #     if(sw == 1){
  #       'Sum of weights = 1'
  #     }else{
  #       paste('Sum of weights = ', sw, '\n The weights should add up to 1', sep = '')
  #     }
  #   }
  #   print(table(comp_rast()@data@values))
  #   print(sw)
  #   print(input[['w']])
  #   for(i in 1:input$addBtn){
  #     print(input[[paste0('w','item',i)]])
  #   }
  #   print(paste0('add button',input$addBtn))
  # })
  #####
  #---------------------------------------------------------------#
  
  #---------------------------------------------------------------#
  #####Download composite map as geotif####
  output$download_comp <- downloadHandler(
    filename = function() {
      paste('composite_suitability', ".tif", sep = "")
    },
    content = function(file) {
      writeRaster(comp_rast(), file)
    }
  )
  #####
  #---------------------------------------------------------------#
  
  #===============================================================#
  #===============================================================#
  
}
#####
#===================================================================================#
