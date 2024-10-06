library(shiny)
library(writexl)

# 创建 Shiny 应用程序
ui <- fluidPage(
  titlePanel("小鼠行为记录工具 - 毫秒精度"),
  
  sidebarLayout(
    sidebarPanel(
      radioButtons("num_mice", "记录小鼠数量", choices = c("1 只" = 1, "2 只" = 2), selected = 1, inline = TRUE),
      textInput("group_name", "实验组名称", value = "Group 1"),
      textInput("mouse1_id", "小鼠 1 编号", value = "Mouse 1"),
      textInput("behaviorA_name", "行为 A 名称", value = "行为 A"),
      textInput("behaviorB_name", "行为 B 名称", value = "行为 B"),
      textInput("behaviorC_name", "行为 C 名称", value = "行为 C"),
      textInput("behaviorD_name", "行为 D 名称", value = "行为 D"),
      h4("小鼠 1 的行为按钮"),
      actionButton("mouse1_behaviorA", "记录行为 A"),
      actionButton("mouse1_behaviorB", "记录行为 B"),
      actionButton("mouse1_behaviorC", "记录行为 C"),
      actionButton("mouse1_behaviorD", "记录行为 D"),
      actionButton("mouse1_other", "记录其他行为"),
      
      conditionalPanel(
        condition = "input.num_mice == 2",
        br(), br(),
        textInput("mouse2_id", "小鼠 2 编号", value = "Mouse 2"),
        textInput("mouse2_behaviorA_name", "行为 A 名称", value = "行为 A"),
        textInput("mouse2_behaviorB_name", "行为 B 名称", value = "行为 B"),
        textInput("mouse2_behaviorC_name", "行为 C 名称", value = "行为 C"),
        textInput("mouse2_behaviorD_name", "行为 D 名称", value = "行为 D"),
        h4("小鼠 2 的行为按钮"),
        actionButton("mouse2_behaviorA", "记录行为 A"),
        actionButton("mouse2_behaviorB", "记录行为 B"),
        actionButton("mouse2_behaviorC", "记录行为 C"),
        actionButton("mouse2_behaviorD", "记录行为 D"),
        actionButton("mouse2_other", "记录其他行为")
      ),
      
      br(), br(),
      actionButton("reset", "重置所有时长"),
      actionButton("save", "保存当前数据"),
      br(), br(),
      actionButton("stop", "导出数据并结束记录")
    ),
    
    mainPanel(
      h4("小鼠行为总时长统计（毫秒）："),
      tableOutput("behavior_table"),
      h4("实时显示的行为时长："),
      verbatimTextOutput("mouse1_time_display"),
      conditionalPanel(
        condition = "input.num_mice == 2",
        verbatimTextOutput("mouse2_time_display")
      )
    )
  )
)

# 服务器逻辑
server <- function(input, output, session) {
  
  all_data <- reactiveValues(data = data.frame(
    Group = character(),
    Mouse = character(),
    Behavior_A = numeric(),
    Behavior_B = numeric(),
    Behavior_C = numeric(),
    Behavior_D = numeric(),
    Other_Behavior = numeric()
  ))
  
  mouse1 <- reactiveValues(
    behavior = NA,
    start_time = NA,
    duration_A = 0,
    duration_B = 0,
    duration_C = 0,
    duration_D = 0,
    duration_other = 0,
    current_duration = 0
  )
  
  mouse2 <- reactiveValues(
    behavior = NA,
    start_time = NA,
    duration_A = 0,
    duration_B = 0,
    duration_C = 0,
    duration_D = 0,
    duration_other = 0,
    current_duration = 0
  )
  
  toggle_behavior <- function(mouse, behavior) {
    if (is.null(mouse$behavior) || is.na(mouse$behavior)) {
      # 开始新的行为计时
      mouse$behavior <- behavior
      mouse$start_time <- Sys.time()
      mouse$current_duration <- 0
    } else {
      # 结束当前行为计时，并累加时长
      duration <- as.numeric(difftime(Sys.time(), mouse$start_time, units = "secs")) * 1000
      if (mouse$behavior == "A") mouse$duration_A <- mouse$duration_A + duration
      if (mouse$behavior == "B") mouse$duration_B <- mouse$duration_B + duration
      if (mouse$behavior == "C") mouse$duration_C <- mouse$duration_C + duration
      if (mouse$behavior == "D") mouse$duration_D <- mouse$duration_D + duration
      if (mouse$behavior == "Other") mouse$duration_other <- mouse$duration_other + duration
      
      # 更新当前行为为新的行为
      mouse$behavior <- ifelse(mouse$behavior == behavior, NA, behavior)
      mouse$start_time <- ifelse(is.na(mouse$behavior), NA, Sys.time())
      mouse$current_duration <- 0
    }
  }
  
  # 为鼠标1行为按钮添加监听事件
  observeEvent(input$mouse1_behaviorA, { toggle_behavior(mouse1, "A") })
  observeEvent(input$mouse1_behaviorB, { toggle_behavior(mouse1, "B") })
  observeEvent(input$mouse1_behaviorC, { toggle_behavior(mouse1, "C") })
  observeEvent(input$mouse1_behaviorD, { toggle_behavior(mouse1, "D") })
  observeEvent(input$mouse1_other, { toggle_behavior(mouse1, "Other") })
  
  # 为鼠标2行为按钮添加监听事件
  observeEvent(input$mouse2_behaviorA, { toggle_behavior(mouse2, "A") })
  observeEvent(input$mouse2_behaviorB, { toggle_behavior(mouse2, "B") })
  observeEvent(input$mouse2_behaviorC, { toggle_behavior(mouse2, "C") })
  observeEvent(input$mouse2_behaviorD, { toggle_behavior(mouse2, "D") })
  observeEvent(input$mouse2_other, { toggle_behavior(mouse2, "Other") })
  
  # 实时更新并显示行为的持续时间
  observe({
    invalidateLater(100, session)
    # 更新鼠标1的实时时长
    if (!is.null(mouse1$start_time) && !is.na(mouse1$start_time) && !is.na(mouse1$behavior)) {
      mouse1$current_duration <- as.numeric(difftime(Sys.time(), mouse1$start_time, units = "secs")) * 1000
    }
    
    # 更新鼠标2的实时时长
    if (input$num_mice == 2 && !is.null(mouse2$start_time) && !is.na(mouse2$start_time) && !is.na(mouse2$behavior)) {
      mouse2$current_duration <- as.numeric(difftime(Sys.time(), mouse2$start_time, units = "secs")) * 1000
    }
    
    # 显示鼠标1的实时时长
    output$mouse1_time_display <- renderText({
      sprintf("小鼠 1 当前行为: %s, 当前行为时长 (毫秒): %.2f, 行为总时长 (毫秒): %s=%.2f, %s=%.2f, %s=%.2f, %s=%.2f, 其他=%.2f", 
              ifelse(is.na(mouse1$behavior), "无", mouse1$behavior), 
              mouse1$current_duration,
              input$behaviorA_name, mouse1$duration_A, 
              input$behaviorB_name, mouse1$duration_B, 
              input$behaviorC_name, mouse1$duration_C, 
              input$behaviorD_name, mouse1$duration_D, 
              mouse1$duration_other)
    })
    
    # 显示鼠标2的实时时长（如果存在）
    if (input$num_mice == 2) {
      output$mouse2_time_display <- renderText({
        sprintf("小鼠 2 当前行为: %s, 当前行为时长 (毫秒): %.2f, 行为总时长 (毫秒): %s=%.2f, %s=%.2f, %s=%.2f, %s=%.2f, 其他=%.2f", 
                ifelse(is.na(mouse2$behavior), "无", mouse2$behavior), 
                mouse2$current_duration,
                input$mouse2_behaviorA_name, mouse2$duration_A, 
                input$mouse2_behaviorB_name, mouse2$duration_B, 
                input$mouse2_behaviorC_name, mouse2$duration_C, 
                input$mouse2_behaviorD_name, mouse2$duration_D, 
                mouse2$duration_other)
      })
    }
  })
  
  # 监听实验组和小鼠编号的变化，重置计时
  observeEvent(input$group_name, {
    mouse1$duration_A <- 0
    mouse1$duration_B <- 0
    mouse1$duration_C <- 0
    mouse1$duration_D <- 0
    mouse1$duration_other <- 0
    mouse1$behavior <- NA
    mouse1$start_time <- NA
    mouse1$current_duration <- 0
  })
  
  observeEvent(input$mouse2_id, {
    if (input$num_mice == 2) {
      mouse2$duration_A <- 0
      mouse2$duration_B <- 0
      mouse2$duration_C <- 0
      mouse2$duration_D <- 0
      mouse2$duration_other <- 0
      mouse2$behavior <- NA
      mouse2$start_time <- NA
      mouse2$current_duration <- 0
    }
  })
  
  # 重置按钮
  observeEvent(input$reset, {
    mouse1$duration_A <- 0
    mouse1$duration_B <- 0
    mouse1$duration_C <- 0
    mouse1$duration_D <- 0
    mouse1$duration_other <- 0
    mouse1$behavior <- NA
    mouse1$start_time <- NA
    mouse1$current_duration <- 0
    
    if (input$num_mice == 2) {
      mouse2$duration_A <- 0
      mouse2$duration_B <- 0
      mouse2$duration_C <- 0
      mouse2$duration_D <- 0
      mouse2$duration_other <- 0
      mouse2$behavior <- NA
      mouse2$start_time <- NA
      mouse2$current_duration <- 0
    }
  })
  
  # 保存数据
  observeEvent(input$save, {
    new_row <- data.frame(
      Group = input$group_name,
      Mouse = input$mouse1_id,
      Behavior_A = mouse1$duration_A,
      Behavior_B = mouse1$duration_B,
      Behavior_C = mouse1$duration_C,
      Behavior_D = mouse1$duration_D,
      Other_Behavior = mouse1$duration_other,
      stringsAsFactors = FALSE
    )
    
    all_data$data <- rbind(all_data$data, new_row)
    
    if (input$num_mice == 2) {
      new_row2 <- data.frame(
        Group = input$group_name,
        Mouse = input$mouse2_id,
        Behavior_A = mouse2$duration_A,
        Behavior_B = mouse2$duration_B,
        Behavior_C = mouse2$duration_C,
        Behavior_D = mouse2$duration_D,
        Other_Behavior = mouse2$duration_other,
        stringsAsFactors = FALSE
      )
      
      all_data$data <- rbind(all_data$data, new_row2)
    }
    
    shiny::showNotification("数据已保存！", type = "message")
  })
  
  # 导出数据
  observeEvent(input$stop, {
    colnames(all_data$data) <- c("实验组", "小鼠编号", input$behaviorA_name, input$behaviorB_name, input$behaviorC_name, input$behaviorD_name, "其他行为")
    write_xlsx(all_data$data, "mouse_behavior_data.xlsx")
    shiny::stopApp()
  })
  
  # 生成行为表格
  output$behavior_table <- renderTable({
    all_data$data
  })
}

# 运行应用程序
shinyApp(ui = ui, server = server)
