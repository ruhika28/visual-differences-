##############################################################################################################
#Title        : LifeStory Health, Inc Project
# Script      : R code for Finding Visual difference on RShiny
# Description : This script will create an interactive Rshiny dashboard to find the visual differences  
#               between two images(graphs) with two options and provide user an ability to mark differences
#               by drawing rectangles around the difference.Both the images can be made blurred with their 
#               respective sliders.
#               1. Compare with existing: Compare a new graph with sample graphs provided by LSH
#               2. Compare New Images : Compare two new graphs
#############################################################################################################
#Load required packages and library for building Rshiny dashboard
#install.packages("shiny")
#install.packages("shinydashboard")
#install.packages("semantic.dashboard")
#install.packages("magick")  #This package is heart of this code, used for overlapping images to compare
#install.packages("shinyjs") # used to add javascript to shiny 
#install.packages("shinythemes")#Package  is used inorder to set the theme of the dashboard 
library(shinyjs)
library(magick)
library(shiny)
library(shinydashboard)
library(semantic.dashboard)
library(shinythemes)

################################################################################################################
#Set the working directory, change it as per location of the images, here /www is directory where 
# all sample images are downloaded in PNG format and renamed with corresponding Item name like 1201.png etc.

################################################################################################################

#ui script to create the layout
ui <-     
          fluidPage(
            theme = shinytheme("simplex"),
            tags$head(
              tags$style(HTML("
      @import url('//fonts.googleapis.com/css?family=Lobster|Cabin:400,700');
      
      h1 {
        font-family: 'Arial', cursive;
        font-weight: 500;
        line-height: 1.1;
        color: maroon;
      }

    "))
            ),
            
           shinyjs::useShinyjs(),
           headerPanel("LifeStory Health:Find visual differences between Graphs"),
           
            # Sidebar Layout for inputs ----
            sidebarLayout(
              # Sidebar Panel for inputs ----
              #Select : input a disease
             sidebarPanel (selectInput("dis", 
                        label = "Choose a Disease",
                        choices = c("Alzheimer", 
                                    "Malaria",
                                   "Cancer", 
                                    "Thyroid"),
                        selected = "Malaria"),
                        #conditional panel : choose compare type
                        conditionalPanel(condition = "input.dis =='Alzheimer'||input.dis =='Malaria'||input.dis =='Cancer'||input.dis =='Thyroid'",
                             selectInput("diff",
                                         label ="Choose Difference type",
                                         choices = c("Compare One  Image Against Two Others"),
                                         selected = "Compare One  Image Against Two Others")),
                        #conditional panel :file input for two files when compare with upload 1
                        conditionalPanel(condition = "input.dis =='Alzheimer'||input.dis =='Malaria'||input.dis =='Cancer'||input.dis =='Thyroid'",
                             #main image upload
                              fileInput("upload2", "Choose a Main  Item Image", accept = c('image/png', 'image/jpeg')),
                             fileInput("upload3", "Choose a new Item Name image 2", accept = c('image/png', 'image/jpeg')),
                              fileInput("upload4", "Choose a new Item Name image 3", accept = c('image/png', 'image/jpeg'))),
                       
             
                         textInput("size", "Size", value = "500x500!"),
                         sliderInput("blur", "Blur Original Image", 0, 20, 0),
                         sliderInput("blur_new", "Blur New Image", 0, 20, 0),
      
            
                        
                         radioButtons("radio", 
               label = HTML('<FONT color="maroon"><FONT size="5pt">Welcome</FONT></FONT><br> <b> choose your comparison</b>'),
               choices = list("compare1st & 2 nd img" = 1, "compare 1st and 3rd img" = 2,"compare 3rd and 2 nd img"=3),
               selected ="",
               inline = T,
               width = "100%")  , 

               actionButton("reset_input", "clear difference")
                              ), #end of sidebar panel
          #main panel for output
          mainPanel(
          conditionalPanel(condition = "input.dis =='Alzheimer'||input.dis =='Malaria'||input.dis =='Cancer'||input.dis =='Thyroid'",
                           plotOutput("img",click = "plot_click",
                                      dblclick = "plot_dblclick",
                                      hover = "plot_hover",
                                      brush = "plot_brush"))
          
         # conditionalPanel( condition ="input.effects != 'compare1'&& input.effects != 'compare2", imageOutput(""))
          
             )#end of main panel
            )#end of sidebar layout
          )#end of fluid page
#server script for comparision
server <- function(input, output, session) {
#initialzing variables for rectangle to be created by user on images for comparision
  print(set.seed(123))
  prev_vals <- NULL

  structures <- reactiveValues(data = data.frame(box_id = numeric(), xmin = numeric(), ymin = numeric(), xmax = numeric(), xmax = numeric()))
  observeEvent(input$reset_input, 
               {
                 structures$data <- data.frame()
                 vals<-NULL
                 prev_vals<-NULL

    
    
     })
  
  #upload Main File
  observeEvent(input$upload2,
               {if (length(input$upload2$datapath))
                image_y <<- image_convert(image_read(input$upload2$datapath), "jpeg")
               info <- image_info(image_y)
               updateRadioButtons(session, "effects", selected = "")
               updateTextInput(session, "size", value = "500x300")
               output$img <- renderImage({
                 tmpfile <- image_y %>%
                   image_resize(input$size) %>%
                   image_blur(input$blur, input$blur) %>%
                   image_write(tempfile(fileext='jpg'), format = 'jpg')
                 # Return a list
                 list(src = tmpfile, contentType = "image/jpeg")})
               })
  
  #Upload  1st File To Be Compared Against Main 
  observeEvent(input$upload3,
               {if (length(input$upload3$datapath))
                 image_z <<- image_convert(image_read(input$upload3$datapath), "jpeg")
               
               image_z<<-image_z %>% image_convolve('Sobel') %>% image_negate()
               image_z<<-image_charcoal(image_z)
               image_z<<-image_transparent(image_z, "white")
               image_z<<-image_colorize(image_z, 50,"olivedrab4")
               info <- image_info(image_z)
               updateRadioButtons(session, "radio", selected = "" )
               updateTextInput(session, "size", value = "500x300")
               output$img_new <- renderImage({
                 tmpfile <- image_z %>%
                   image_resize(input$size) %>%
                   image_blur(input$blur_new, input$blur_new) %>%
                   image_write(tempfile(fileext='jpg'), format = 'jpg')
                 # Return a list
                 list(src = tmpfile, contentType = "image/jpeg")})
               })
  
  
  #Upload  2nd File To Be Compared Against Main 
  observeEvent(input$upload4,
               {if (length(input$upload4$datapath))
                 image_t <<- image_convert(image_read(input$upload4$datapath), "jpeg")
               
               image_t<<-image_t %>% image_convolve('Sobel') %>% image_negate()
               image_t<<-image_charcoal(image_t)
               image_t<<-image_transparent(image_t, "white")
               image_t<<-image_colorize(image_t, 50,"olivedrab4")
               info <- image_info(image_t)
                updateRadioButtons(session, "radio", selected = "")
               updateTextInput(session, "size", value = "500x300")
               output$img_new <- renderImage({
                 tmpfile2<- image_t %>%
                   image_resize(input$size) %>%
                   image_blur(input$blur_new, input$blur_new) %>%
                   image_write(tempfile(fileext='jpg'), format = 'jpg')
                 # Return a list
                list(src = tmpfile2, contentType = "image/jpeg")})
              
               })  
  
  
  
#######################################################################################################################################
#When new file for "Compare New Images" selected, the two new images uploaded are compared by overlapping them
########################################################################################################################################    
  observeEvent(input$upload3,
               {if(!is.null(input$upload2) && !is.null(input$upload3) )
                 observeEvent(input$radio,
                              {if (input$radio == 1 && !is.null(input$radio))
                                
                                
                                output$img <- renderPlot({
                              #  shinyjs::disable(selector = "#effects input[value='compare2']")
                                  tmpfile <- image_flatten(c(image_blur(image_y,input$blur,input$blur),image_blur(image_z,input$blur_new,input$blur_new)),"Add")
                               
                                                            #create a blank plot where overlapped image will be rastered
                                  blank<-plot(1:500,type='n',axes = FALSE, xlab = "", ylab = "")
                                  rasterImage(tmpfile,1,1,500,500)
                                  #user can click and create red rectangles to mark on the image
                                  if (nrow(structures$data) > 0) {
                                    r <- structures$data
                                    rect(r$xmin, r$ymin, r$xmax, r$ymax, border = "green",lwd="3")
                                  }
                                  tmpfile <- tmpfile %>%
                                    image_blur(input$blur, input$blur) %>%
                                    image_write(tempfile(fileext = 'jpg'), format = 'jpg')
                                  #   # Return a list
                                  list(src = tmpfile, contentType = "image/jpeg")
                                  
                                }, height = 1000, width = 1000)
                              #display the rectangles clicked by user
                              observeEvent(input$radio,{
                                if(input$radio=="1")
                               { e <- input$plot_brush
                                if (!is.null(e)) {
                                  vals <- data.frame(xmin = round(e$xmin, 1), ymin = round(e$ymin, 1), xmax = round(e$xmax, 1), ymax = round(e$ymax, 1))
                                  if (identical(vals,prev_vals)) return() #We dont want to change anything if the values havent changed.
                                  
                                  structures$data <- rbind(structures$data,cbind(data.frame(box_id = nrow(structures$data)+1),vals))
                                  
                                  prev_vals <<- vals}}
                                
                                
                                
                              })  
                              }
                              )#})
                 
               }#end of first if - observEvent - upload3
   )#end of observEvent - upload3
  
  
  
  
  observeEvent(input$upload4,
               {if(!is.null(input$upload2) && !is.null(input$upload4) )
                 observeEvent(input$radio,
                              {if (input$radio == "2" && !is.null(input$radio))
                                output$img <- renderPlot({
                                  #shinyjs::disable(selector = "#effects input[value='compare1']")
                                  tmpfile2<- image_flatten(c(image_blur(image_y,input$blur,input$blur),image_blur(image_t,input$blur_new,input$blur_new)),"Add")
                                  
                                  #create a blank plot where overlapped image will be rastered
                                  plot(1:500,type='n',axes = FALSE, xlab = "", ylab = "")
                                  rasterImage(tmpfile2,1,1,500,500)
                                  #user can click and create red rectangles to mark on the image
                                  if (nrow(structures$data) > 0) {
                                    r <- structures$data
                                    rect(r$xmin, r$ymin, r$xmax, r$ymax, border = "green",lwd="3")
                                  }
                                  tmpfile2 <- tmpfile2 %>%
                                    image_blur(input$blur, input$blur) %>%
                                    image_write(tempfile(fileext = 'jpg'), format = 'jpg')
                                  #   # Return a list
                                  list(src = tmpfile2, contentType = "image/jpeg")
                                  
                                }, height = 1000, width = 1000)
                              #display the rectangles clicked by user
                              observe({
                            
                                
                                e <- input$plot_brush
                                if (!is.null(e)) {
                                  vals <- data.frame(xmin = round(e$xmin, 1), ymin = round(e$ymin, 1), xmax = round(e$xmax, 1), ymax = round(e$ymax, 1))
                                  if (identical(vals,prev_vals)) return() #We dont want to change anything if the values havent changed.
                                  
                                  structures$data <- rbind(structures$data,cbind(data.frame(box_id = nrow(structures$data)+1),vals))
                                  
                                  
                                  
                                 prev_vals <<- vals
                                
                              }
                              })  
                              })#})
                 
  
               }#end of second if - observEvent - upload4
  )#end of observEvent - upload4
  
  
  observeEvent(input$radio,
               {if(!is.null(input$upload3) && !is.null(input$upload4) )
                 observeEvent(input$radio,
                              {if (input$radio == "3" && !is.null(input$radio))
                                
    
                                
                                
                                output$img <- renderPlot({
                                
                                  tmpfile2<- image_flatten(c(image_blur(image_z,input$blur,input$blur),image_blur(image_t,input$blur_new,input$blur_new)),"Add")
                                  
                                  #create a blank plot where overlapped image will be rastered
                                  plot(1:500,type='n',axes = FALSE, xlab = "", ylab = "")
                                  rasterImage(tmpfile2,1,1,500,500)
                                  #user can click and create red rectangles to mark on the image
                                  if (nrow(structures$data) > 0) {
                                    r <- structures$data
                                    rect(r$xmin, r$ymin, r$xmax, r$ymax, border = "green",lwd="3")
                                  }
                                  tmpfile2 <- tmpfile2 %>%
                                    image_blur(input$blur, input$blur) %>%
                                    image_write(tempfile(fileext = 'jpg'), format = 'jpg')
                                  #   # Return a list
                                  list(src = tmpfile2, contentType = "image/jpeg")
                                  
                                }, height = 1000, width = 1000)
                              #display the rectangles clicked by user
                              observe({
                                e <- input$plot_brush
                                if (!is.null(e)) {
                                  vals <- data.frame(xmin = round(e$xmin, 1), ymin = round(e$ymin, 1), xmax = round(e$xmax, 1), ymax = round(e$ymax, 1))
                                  if (identical(vals,prev_vals)) return() #We dont want to change anything if the values havent changed.
                                  
              
                                  structures$data <- rbind(structures$data,cbind(data.frame(box_id = nrow(structures$data)+1),vals))
                                  prev_vals <<- vals
                                  
                                }
                              })  
                              })#})
                 
                 
               }#end of second if - observEvent - upload4
  )#end of observEvent - upload4
  
  
}#end of server

#run App
 shinyApp(ui, server)    
