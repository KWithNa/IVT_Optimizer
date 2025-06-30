options(repos = c(CRAN = "https://cran.rstudio.com"))
{ install.packages("shiny")
  install.packages("promises")
  install.packages("DT")
  install.packages("purrr")
  install.packages("htmltools")
  install.packages("tibble")
  install.packages("colorspace")
  install.packages("shinyjs")
  install.packages("shinyBS")
  install.packages("openxlsx")
  install.packages("dplyr")
}
{ library(shiny)
  library(DT)
  library(purrr)
  library(htmltools)
  library(tibble)
  library(colorspace)
  library(shinyjs)
  library(shinyBS)
  library(openxlsx)
  library(dplyr)
}

{ load("data.RData")
  
  lab.int <- lis.iaf$ALL
  
  palettes <- tibble(palette = c("Overall","Safety","Efficacy","Convenience","Affordability"),
                     colours = list(colorRampPalette(RColorBrewer::brewer.pal(9,"Greys"))(101),
                                    colorRampPalette(RColorBrewer::brewer.pal(9,"Blues"))(101),
                                    colorRampPalette(RColorBrewer::brewer.pal(9,"Reds"))(101),
                                    colorRampPalette(RColorBrewer::brewer.pal(9,"YlOrBr"))(101),
                                    colorRampPalette(RColorBrewer::brewer.pal(9,"Greens"))(101))
  ) %>% deframe() #颜色
  func.scl <- function(y,lower = min(y,na.rm = TRUE),upper = max(y,na.rm = TRUE)) {
    if (lower == upper)
      lower <- upper-0.1
    y[y<lower] <- lower
    y[y>upper] <- upper
    (y-lower)/(upper-lower)
  } #标准化函数
  
  func.col <- function (x,palette) {
    palette[ceiling(x*(length(palette)-1))+1]
  } #颜色提取
  
  func.rdr <- function (palette) {
    function(x) {
      normalised <- func.scl(x,lower = 0)
      color <- func.col(normalised,palette)
      paste0('<div style="display: flex; align-items: center;">
                <div style="height: 15px; width:',normalised*60,'%; background-color:',color,'; border-radius: 5px;"></div>
                <span style="color: black; margin-left: 5px; text-align: right;">',format(round(x,2),nsmall = 2),'</span>
              </div>')
    }
  } #获取分数渲染器函数
}

# Shiny 应用 UI
ui <- fluidPage(
  useShinyjs(),
  tags$style(HTML("
    .fixed-title { position: fixed; top: 0; left: 0; width: 100%; height: 160px; background-color: #f8f9fa;
      z-index: 1000;padding: 10px 15px;box-shadow: 0 2px 5px rgba(0, 0, 0, 0.1); }
    .fixed-title img { height: 120px;  margin-left: 10px;  margin-top: 15px;  margin-bottom: 25px; max-height: 100%; }
    .sidebar-panel { height: calc(100vh - 160px); overflow-y: auto; position: fixed; top: 160px; left: 0; width: 600px; background-color: #f8f9fa; padding: 10px; }
    .main-panel { height: calc(100vh - 160px); overflow-y: auto; width: calc(100vw - 600px); margin-left: 600px; margin-top: 160px; padding: 10px; }
    .container-fluid { padding: 0; }
    table.dataTable { width: 150% !important; white-space: nowrap; }
    .dt-header-overall { background-color: #D1D1D1; color: black; }
    .dt-header-safety { background-color: #BAD6EB; color: black; }
    .dt-header-efficacy { background-color: #FCAF93; color: black; }
    .dt-header-convenience { background-color: #FEDA7E; color: black; }
    .dt-header-affordability { background-color: #BCE4B5; color: black; }
    .sidebar-panel .panel-heading { background-color: #DAE8F5; }
  ")),
  div(class = "fixed-title", tags$img(src = "icon.png")),
  tags$div(
    class = "sidebar-panel",
    bsCollapse(
      bsCollapsePanel(title = "Weight Adjustment",div(
        tags$p("Adjust the weight parameters for each dimension below to optimize the scoring."),
        tags$p("We recommend that the weights for Safety and Efficacy should not be less than 1, while the weights for Convenience and Affordability should not exceed 1.",style = "color: red;"),
        sliderInput("wei.saf","Weight for Safety:",min = 0,max = 10,value = 5,step = 0.1,width = "500px"),
        sliderInput("wei.eff","Weight for Efficacy:",min = 0,max = 10,value = 5,step = 0.1,width = "500px"),
        sliderInput("wei.con","Weight for Convenience:",min = 0,max = 1,value = 0.5,step = 0.01,width = "500px"),
        sliderInput("wei.aff","Weight for Affordability:",min = 0,max = 1,value = 0.5,step = 0.01,width = "500px")
      ))
    ),
    br(),
    bsCollapse(bsCollapsePanel(title = "Therapeutic Window",div(
      tags$p("Whether therapeutic window (4.5 hours) should be considered?"),
      radioButtons("ttw.cho",choices = list("ALL" = "ALL","IN" = "IN","OUT" = "OUT"),selected = "ALL",label = NULL)))),
    br(),
    bsCollapse(bsCollapsePanel(title = "Storage Adjustment",div(
      tags$p("Refine the grading of the storage conditions for intravenous thrombolytic drugs below to optimize the scoring."),
      tags$p("We recommend using a 1, 3, 5 grading scale for evaluation, but other grading methods are also acceptable.",style = "color: red;"),
      tags$p(HTML("Temperature Requirements:<br>
                  &nbsp;&nbsp;&nbsp;&nbsp;1 = Requires strict refrigeration<br>
                  &nbsp;&nbsp;&nbsp;&nbsp;3 = Requires room temperature<br>
                  &nbsp;&nbsp;&nbsp;&nbsp;5 = Allows for a wide temperature range"),
             style = "color: blue;"),
      tags$p(HTML("Light Sensitivity:<br>
                  &nbsp;&nbsp;&nbsp;&nbsp;1 = Needs to be protected strictly from light<br>
                  &nbsp;&nbsp;&nbsp;&nbsp;3 = Requires avoiding prolonged exposure to light<br>
                  &nbsp;&nbsp;&nbsp;&nbsp;5 = Not sensitive to light"),
             style = "color: blue;"),
      tags$p(HTML("Shelf Life:<br>
                  &nbsp;&nbsp;&nbsp;&nbsp;1 = Must be used immediately after reconstitution<br>
                  &nbsp;&nbsp;&nbsp;&nbsp;3 = Must be used within 8 hours after reconstitution<br>
                  &nbsp;&nbsp;&nbsp;&nbsp;5 = Has an extremely long shelf life"),
             style = "color: blue;"),
      id = "stg.are",uiOutput("stg.trt"),actionButton("stg.edt","Toggle Edit Mode"),actionButton("stg.rst","Reset Values")))),
    br(),
    bsCollapse(bsCollapsePanel(title = "Cost Adjustment",div(
      tags$p("Please fill in more accurate costs for intravenous thrombolytic therapy below to optimize the scoring."),
      id = "cos.are",uiOutput("cos.trt"),actionButton("cos.edt","Toggle Edit Mode"),actionButton("cos.rst","Reset Values")))),
    br()
  ),
  tags$div(class = "main-panel",mainPanel(DTOutput("lab.scr")))
)

# Shiny 应用服务器逻辑
server <- function(input,output,session) {
  
  # 更新Storage和Cost面板输入框
  dyn.scr <- function(lab.inf) {
    lapply(seq_along(lab.inf$`Temperature Requirements`),function(i) {
      updateNumericInput(session,paste0("temp_",i),value = lab.inf$`Temperature Requirements`[i])
      updateNumericInput(session,paste0("light_",i),value = lab.inf$`Light Sensitivity`[i])
      updateNumericInput(session,paste0("shelf_",i),value = lab.inf$`Shelf Life`[i])
    })
    lapply(seq_along(lab.inf$`Treatment Cost`), function(i) {
      updateNumericInput(session,paste0("afford_",i),value = lab.inf$`Treatment Cost`[i])
    })
  }
  
  # 根据治疗窗选择更新 dyn.int 的值
  dyn.int <- reactiveVal()
  observe({
    dyn.int(lis.iaf[[input$ttw.cho]])
    dyn.scr(lis.iaf[[input$ttw.cho]])
  })
  
  # 封装数据读取
  lab.dat <- reactive({
    dyn.dat <- dyn.int()
    list(temp = dyn.dat$`Temperature Requirements`,
         light = dyn.dat$`Light Sensitivity`,
         shelf = dyn.dat$`Shelf Life`,
         cost = dyn.dat$`Treatment Cost`)
    })
  
  # Storage和Cost数据读取
  output$stg.trt <- renderUI({
    lab.inf <- lab.dat()  # 获取当前的存储数据
    tagList(
      fluidRow(
        column(5,""),
        column(3,tags$div(style = "font-weight: bold; height: 40px; text-align: center;","Temperature\nRequirements")),
        column(2,tags$div(style = "font-weight: bold; height: 40px; text-align: center;","Light\nSensitivity")),
        column(2,tags$div(style = "font-weight: bold; height: 40px; text-align: center;","Shelf\nLife"))
      ),
      lapply(seq_along(lab.inf$temp),function(i) {
        fluidRow(
          column(5,strong(rownames(dyn.int())[i])),
          column(3,numericInput(inputId = paste0("temp_",i),label = NULL,value = lab.inf$temp[i],min = 0,max = 6,step = 1,width = "125px")),
          column(2,numericInput(inputId = paste0("light_",i),label = NULL,value = lab.inf$light[i],min = 0,max = 6,step = 1,width = "80px")),
          column(2,numericInput(inputId = paste0("shelf_",i),label = NULL,value = lab.inf$shelf[i],min = 0,max = 6,step = 1,width = "80px"))
        )
      })
    )
  })
  output$cos.trt <- renderUI({
    lab.inf <- lab.dat()  # 获取当前的 cost 数据
    tagList(
      fluidRow(
        column(6,""),
        column(6,tags$div(style = "font-weight: bold; height: 40px; text-align: center;","Cost of Treatment"))
      ),
      lapply(seq_along(lab.inf$cost),function(i) {
        fluidRow(
          column(6,strong(rownames(dyn.int())[i])), # 显示干预措施名称
          column(6,numericInput(inputId = paste0("afford_",i),label = NULL,value = lab.inf$cost[i],width = "250px"))
        )
      })
    )
  })
  
  # 切换输入框的可编辑状态
  observeEvent(input$cos.edt,{
    cos.tog <- isolate(input$cos.edt %% 2 == 0)
    lapply(seq_along(lab.dat()$cost),function(i) {
      shinyjs::toggleState(paste0("afford_",i),condition = cos.tog)
    })
  })
  observeEvent(input$stg.edt,{
    stg.tog <- isolate(input$stg.edt %% 2 == 0)
    lapply(seq_along(lab.dat()$cost),function(i) {
      shinyjs::toggleState(paste0("temp_",i),condition = stg.tog)
      shinyjs::toggleState(paste0("light_",i),condition = stg.tog)
      shinyjs::toggleState(paste0("shelf_",i),condition = stg.tog)
    })
  })
  
  # 重置Storage和Cost输入框的值为初始值
  observeEvent(input$cos.rst,{
    dyn.int(lis.iaf[[input$ttw.cho]])
    dyn.scr(lis.iaf[[input$ttw.cho]])
  })
  observeEvent(input$stg.rst,{
    dyn.int(lis.iaf[[input$ttw.cho]])
    dyn.scr(lis.iaf[[input$ttw.cho]])
  })
  
  # 更新数据中的Treatment Cost和Storage值
  observe({
    cos.new <- sapply(seq_along(lab.dat()$cost),function(i) {
      aff.val <- input[[paste0("afford_",i)]]
      if (!is.null(aff.val) && !is.na(as.numeric(aff.val))) {
        as.numeric(aff.val)
      } else {
        lab.dat()$cost[i]
      }
    })
    # 更新dyn.int的`Treatment Cost`
    edt.int <- dyn.int()
    edt.int$`Treatment Cost` <- cos.new
    dyn.int(edt.int)
  })
  observe({
    temp.new <- sapply(seq_along(lab.dat()$temp),function(i) {
      temp.val <- input[[paste0("temp_",i)]]
      if (!is.null(temp.val) && !is.na(as.numeric(temp.val))) {
        as.numeric(temp.val)
      } else {
        lab.dat()$temp[i]
      }
    })
    
    light.new <- sapply(seq_along(lab.dat()$light),function(i) {
      light.val <- input[[paste0("light_",i)]]
      if (!is.null(light.val) && !is.na(as.numeric(light.val))) {
        as.numeric(light.val)
      } else {
        lab.dat()$light[i]
      }
    })
    
    shelf.new <- sapply(seq_along(lab.dat()$shelf),function(i) {
      shelf.val <- input[[paste0("shelf_",i)]]
      if (!is.null(shelf.val) && !is.na(as.numeric(shelf.val))) {
        as.numeric(shelf.val)
      } else {
        lab.dat()$shelf[i]
      }
    })
    # 更新 dyn.int 的 Storage 值
    edt.int <- dyn.int()
    edt.int$`Temperature Requirements` <- temp.new
    edt.int$`Light Sensitivity` <- light.new
    edt.int$`Shelf Life` <- shelf.new
    dyn.int(edt.int)
  })
  
  # 渲染表格
  output$lab.scr <- renderDT({
    lab.prt <- dyn.int() %>%
      mutate(Convenience = 70-5*`Intravenous Bolus`-`Intravenous Infusion`+2*(lab.dat()$temp+lab.dat()$light +lab.dat()$shelf)) %>%
      mutate(`Treatment Cost` = round(lab.dat()$cost,2)) %>%
      mutate(`Treatment Cost` = ifelse(is.na(`Treatment Cost`),mean(`Treatment Cost`,na.rm = TRUE),`Treatment Cost`)) %>%
      mutate(Affordability = 100-`Treatment Cost`/max(`Treatment Cost`)*100) %>%
      mutate(Overall = (Safety*input$wei.saf+Efficacy*input$wei.eff+Convenience*input$wei.con+Affordability*input$wei.aff)/
               (input$wei.saf+input$wei.eff+input$wei.con+input$wei.aff)) %>%
      relocate(Count,Overall,Safety,Efficacy,Convenience,Affordability,.after = 1) %>%
      arrange(desc(Overall)) %>%
      mutate(Overall = func.rdr(palettes$Overall)(Overall),
             Safety = func.rdr(palettes$Safety)(Safety),
             Efficacy = func.rdr(palettes$Efficacy)(Efficacy),
             Convenience = func.rdr(palettes$Convenience)(Convenience),
             Affordability = func.rdr(palettes$Affordability)(Affordability)) %>%
      .[2:7]
    
    datatable(lab.prt, escape = FALSE,
              options = list(
                paging = FALSE,pageLength = -1,
                columnDefs = list(list(className = 'dt-center',targets = "_all"),
                                  list(width = "100px",targets = 2:6),
                                  list(width = "100px",targets = 0),
                                  list(width = "50px",targets = 1)
                                  ),
                headerCallback = JS(
                  "function(thead, data, start, end, display) {",
                  "  $(thead).find('th').eq(2).addClass('dt-header-overall');",
                  "  $(thead).find('th').eq(3).addClass('dt-header-safety');",
                  "  $(thead).find('th').eq(4).addClass('dt-header-efficacy');",
                  "  $(thead).find('th').eq(5).addClass('dt-header-convenience');",
                  "  $(thead).find('th').eq(6).addClass('dt-header-affordability');",
                  "}"
                )
              )
    )
  })
}

# 运行 Shiny 应用
shinyApp(ui = ui,server = server)

