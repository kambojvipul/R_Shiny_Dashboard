library(shiny)
library(shinydashboard)
library(shinydashboardPlus)

library(ggplot2)
library(DT)
library(corrplot)
library(PerformanceAnalytics)
library(wesanderson)
library(hrbrthemes)
library(viridis)
library(reshape2)

theme_update(text = element_text(size=14))
#Department Data Load
dept_sum = read.csv("/Users/vipulkamboj/Downloads/r_shiny/dept_summary.csv")
#dept_sum.molten <- melt(dept_sum, id="Department",value.name="Score", variable.name="Features", na.rm=TRUE) #Transverse Table

#Doctor Data Load
doc_sum = read.csv("/Users/vipulkamboj/Downloads/r_shiny/doc_summary.csv")
#doc_sum.molten <- melt(doc_sum, id=c("Doctor_ID","Department"),value.name="Score", variable.name="Features", na.rm=TRUE) #Transverse Table


doc_evaluation = read.csv('/Users/vipulkamboj/Downloads/r_shiny/Dataset/doc_feedback.csv')

rat_type = read.csv('/Users/vipulkamboj/Downloads/r_shiny/Dataset/rating_details.csv')


ui <- dashboardPage(skin="purple",
  dashboardHeader(title= "Performance Indicator"),#dashboardHeader
  dashboardSidebar(
    sidebarMenu(
                menuItem("360 Feedback Rating", tabName = "rating",badgeLabel = icon("rating"),badgeColor = "maroon"),
                menuItem("Doctor Performance", tabName = "doc_per", badgeLabel = icon("dept_per_1"),badgeColor = "purple"),
                menuItem("Department Performance", tabName = "dept_per", badgeLabel = icon("dept_per_2"),badgeColor = "yellow"),
                selectInput(inputId = "doc_sel",
                            label = "Choose Doctor",
                            list('All','doc-01', 'doc-02', 'doc-03', 'doc-04', 'doc-05', 'doc-06',
                                  'doc-07', 'doc-08', 'doc-09', 'doc-10', 'doc-11', 'doc-12',
                                  'doc-13', 'doc-14', 'doc-15', 'doc-16', 'doc-17', 'doc-18',
                                  'doc-19', 'doc-20', 'doc-21', 'doc-22', 'doc-23', 'doc-24',
                                  'doc-25'),),
                selectInput(inputId = "dept_sel",
                            label = "Choose Department",
                            list('All','anesthesia', 'emergency', 'internal medicine', 'medicine',
                                  'neurology', 'pulmonary medicine'),)
                )
    ),#dashboardSidebar
  dashboardBody(
    tabItems(
      
      tabItem("rating",
              fluidPage(
                navbarPage(title = span( "360 Feedback Ratings", style = "background-color: #00; color: black"),
                           tabPanel(title = span( "Details of Ratings", style = "background-color: #00; color: black"), 
                                    h1("Ratings Description: "),
                                    h4("This table describes all ratings (Q1 to Q23) given to all doctors. Each rating has unique purpose, which is detailed below in Table."),
                                    (dataTableOutput("rat_type_table"))),
                           tabPanel(title = span( "Ratings Data Table", style = "background-color: #00; color: black"), 
                                    h1("Ratings of Doctor's on 23 Parameters by 360_Feedback:"),
                                    h4("This table presents the score of each doctor on all 23 nos. ratings."),
                                    dataTableOutput("rat_table")),
                           tabPanel(title = span( "Visualization of Ratings", style = "background-color: #00; color: black"), 
                                    h1("Visualization of Ratings:"),
                                    h4("The 360 Feedback Rating of all Doctors:"),
                                    (plotOutput("doc_rating_1")))
                            )
                
                        ),
          
              ),
      
      tabItem("doc_per", 
              fluidPage(
                navbarPage(title = span( "Doctor Performance Indicator", style = "background-color: #00; color: black"),
                           tabPanel(title = span( "Score Table", style = "background-color: #00; color: black"), 
                                    h1("Doctors Performance Table:"),
                                    h4("This table presents the performnace score of each doctor on seven key features. The department and doctor can be chosen by drop down menu in left side."),
                                    dataTableOutput("doc_data_table")),
                           tabPanel(title = span( "Stacked Plot of Performance", style = "background-color: #00; color: black"), 
                                    h1("Doctor's Combined Performance on Seven Key Features:"),
                                    h4("The	plot presents	 the	stacked	scores	of	all	physicians	on	seven	 features.	Thus,
the	 combined	 score	 of	 each	 physician	 on	 these	 parameters	 presents	 the	comprehensive	 performance	 score	 of	 the	 physician.	The	 physician	 can	 also be	 assessed inter	 or	 intra	
department. The department and doctor can be chosen by drop down menu in left side."),
                                    (plotOutput("doc_plot_1"))),
                           tabPanel(title = span( "Unstacked Plot of Performance", style = "background-color: #00; color: black"), 
                                    h1("Doctor's Individual Performance on Seven Key Features: "),
                                    h4("This plot presents the score of performance of each doctor on seven individual features. The department and doctor can be chosen by drop down menu in left side."),
                                    (plotOutput("doc_plot_2")))
                ),
                
                
              )
      ),#tabItem
      
      tabItem("dept_per", 
              fluidPage(
                navbarPage(title = span( "Department Performance Indicator", style = "background-color: #00; color: black"),
                           tabPanel(title = span( "Score Table", style = "background-color: #00; color: black"), 
                                    h1("Department Performnace Table:"),
                                    h4("This table presents the performnace score of each department on seven key features. The department can be chosen by drop down menu in left side."),
                                    dataTableOutput("dept_data_table")),
                           tabPanel(title = span( "Stacked Plot of Performance", style = "background-color: #00; color: black"), 
                                    h1("Department Combinded Performance Indicator on Seven Key Features: "),
                                    h4("The	plot presents	 the	stacked	scores	of	all departments	on	seven	 key features.	Thus,
the	 combined	 score	 of	 each	 department	 on	 these	 parameters	 presents	 the	comprehensive	 performance	 score	 of	 that department. 	The	 physician	 can	 also be	 assessed inter	 or	 intra	
department. The department can be chosen by drop down menu in left side."),
                                    (plotOutput("dept_plot_1"))),
                           tabPanel(title = span( "Unstacked Plot of Performance", style = "background-color: #00; color: black"), 
                                    h1("Department Individual Performance Indicator on Seven Key Features: "),
                                    h4("This plot presents the score of performance of each doctor on seven individual features. The department can be chosen by drop down menu in left side."),
                                    (plotOutput("dept_plot_2")),
                                    h4("Analyzed by Vipul Kamboj, July 14, 2022"))
                ),
                
                 
                        )
              )#tabItem
    )
    
  )#dashboardBody
) #ui

server <-function(input, output){
  
  doc_evaluation.subset = reactive({if(input$doc_sel=="All"){d=doc_evaluation}else{d=subset(doc_evaluation, DocID==input$doc_sel)}
                          d = data.frame(d)
                          return(d)})
  
  output$rat_table= renderDataTable(doc_evaluation.subset())
  
  output$rat_type_table= renderDataTable(rat_type)
  
  output$doc_rating_1 <- renderPlot({
    ggplot(melt(doc_evaluation.subset(), value.name="Rating", variable.name="Feature", na.rm=TRUE), aes(x = DocID, y = Feature, fill = Rating)) + #theme(text = element_text(size = 17))+
      geom_tile() + 
      scale_fill_viridis(discrete=FALSE) 
  })
  
  #doc_sum = doc_sum[doc_sum$Doctor_ID==input$doc_sel]
  
  #doc_sum.molten <- melt(doc_sum, id=c("Doctor_ID","Department"),value.name="Score", variable.name="Features", na.rm=TRUE) #Transverse Table
  
  doc_sum.subset = reactive({
    
    if(input$doc_sel=="All"){a=doc_sum}else{a=subset(doc_sum, Doctor_ID==input$doc_sel)}
    if(input$dept_sel=="All"){c=a}else{c=subset(a, Department==input$dept_sel)}
    c= data.frame(c)
    #a = melt(a, id=c("Doctor_ID","Department"),value.name="Score", variable.name="Features", na.rm=TRUE)
    #if(input$doc_sel=="All"){a=doc_sum.molten}else{a=subset(doc_sum.molten, Doctor_ID==input$doc_sel)}
    return(c)
  })
  
  dept_sum.subset = reactive({
    
    if(input$dept_sel=="All"){b=dept_sum}else{b=subset(dept_sum, Department==input$dept_sel)}
    b= data.frame(b)
    #b = melt(b, id="Department",value.name="Score", variable.name="Features", na.rm=TRUE)
    #if(input$doc_sel=="All"){a=doc_sum.molten}else{a=subset(doc_sum.molten, Doctor_ID==input$doc_sel)}
    return(b)
  })
  
  
  output$doc_data_table= renderDataTable(doc_sum.subset())
  
  output$dept_data_table= renderDataTable(dept_sum.subset())
  
  output$doc_plot_1 <- renderPlot({
    
    a = melt(doc_sum.subset(), id=c("Doctor_ID","Department"),value.name="Score", variable.name="Features", na.rm=TRUE)
    
    ggplot(data = a, aes(fill=Features, y=Score, x=Doctor_ID)) + #theme(axis.title = element_text(size = 20))+ 
      geom_bar(position="stack", stat="identity", colour="grey")+scale_fill_manual(values = c("#ffccd5","#f7aef8","#b388eb","#8093f1","#72ddf7","#a5ffd6","#98c1d9"))
  })
  
  output$doc_plot_2 <- renderPlot({
    
    a = melt(doc_sum.subset(), id=c("Doctor_ID","Department"),value.name="Score", variable.name="Features", na.rm=TRUE)
    
    ggplot(data = a, aes(fill=Features, y=Score, x=Doctor_ID)) + #theme(axis.title = element_text(size = 20))+
      geom_bar(position="dodge", stat="identity", colour="grey")+scale_fill_manual(values = c("#ffccd5","#f7aef8","#b388eb","#8093f1","#72ddf7","#a5ffd6","#98c1d9"))
  })
  
  output$dept_plot_1 <- renderPlot({
    
    b = melt(dept_sum.subset(), id="Department",value.name="Score", variable.name="Features", na.rm=TRUE)
    
    ggplot(data = b, aes(fill=Features, y=Score, x=Department)) + theme(text = element_text(size = 20))+
      geom_bar(position="stack", stat="identity", colour="grey")+scale_fill_manual(values = c("#ffccd5","#f7aef8","#b388eb","#8093f1","#72ddf7","#a5ffd6","#98c1d9"))
  })
  
  output$dept_plot_2 <- renderPlot({
    
    b = melt(dept_sum.subset(), id="Department",value.name="Score", variable.name="Features", na.rm=TRUE)
    
    ggplot(data = b, aes(fill=Features, y=Score, x=Department, colour="white", name="Darjeeling")) + theme(text = element_text(size = 20))+ 
      geom_bar(position="dodge", stat="identity", colour="grey", name="Darjeeling")+scale_fill_manual(values = c("#ffccd5","#f7aef8","#b388eb","#8093f1","#72ddf7","#a5ffd6","#98c1d9"))
    
  })
}#server

shinyApp(ui, server)

