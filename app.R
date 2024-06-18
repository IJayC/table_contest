library(shiny)
library(DT)
library(shinyalert)

source('datatable_contest_source.R')
###-----------------UI----------------- > 
ui <- fluidPage(

  textInputRow("rows_dim", 
            label = "Rows", 
            value = "5"),
  textInputRow("column_dim", 
            label = "Columns", 
            value = "5"),
  actionButton('play', label = "Go"),
  # actionButton("getQueue", "Get list of queued files"),
  # verbatimTextOutput("devel"),
  # dataTableOutput('play_history_table'),
  DT::dataTableOutput("game_board")
)

## ----------------Server------------------>
shinyServer <- function(input, output) {
  
  
  tbl <- eventReactive(input$play, {
    rows <- as.numeric(input$rows_dim)
    columns <- as.numeric(input$column_dim)
    if(is.na(rows))
      shinyalert::shinyalert(title = "Check your input", text = "Please enter numeric for rows")
    if(is.na(columns))
      shinyalert::shinyalert(title = "Check your input", text = "Please enter numeric for columns")
    initialize_matrix(rows, columns, NA)
  })

  # Reactive values
  rvs <- reactiveValues(data = NA) #dynamic data object 
  rvs_tmp <- reactiveVal(NA)
  rvs_clicks_count <- reactiveVal()
  rvs_player_slots <- reactiveVal()
  
  observe({ rvs$data <- tbl() })
  observe({ rvs$data <- tbl() })
  observe({ rvs_tmp(tbl()) })
  observe({ rvs_clicks_count(0) })
  observe({ rvs_player_slots(tbl()) })
  
  output$game_board <- DT::renderDataTable({
    DT::datatable(isolate(rvs$data), 
                  selection = list(target = "cell"), 
                  editable= TRUE, 
                  # editable= "cell", 
                  class = 'cell-border stripe', 
                  rownames= FALSE, 
                  options = list(searching = FALSE, 
                                 pageLength = nrow(tbl()),
                                 info = FALSE,
                                 dom='t',
                                 ordering=F,
                                 rowCallback = JS("function(r,d) {$(r).attr('height', '100px')}")), escape = FALSE) #columnDefs = list(list(width = '20px', targets = 1)) ## Ref https://yihui.shinyapps.io/DT-edit/1
  })
  # 
  observeEvent(input$game_board_cells_selected, {
    req(input$game_board_cells_selected)
    board <- rvs$data
    n_clicks <- rvs_clicks_count() # To track total_clicks
    temp_reactive_df <- rvs_tmp() # The tracker for number of clicks to save in the back end
    slots_table <- rvs_player_slots()

    # Cell value from the board
    x <- as.data.frame(input$game_board_cells_selected)
    if(nrow(x) > 1)
      x <- x[nrow(x),]
    selected_row_col <- c(x$V1, (x$V2+1))
    current_value <- unlist(temp_reactive_df[x$V1, (x$V2+1)])[1] 

    # Update number of clicks
    n_clicks <- n_clicks + 1
    rvs_clicks_count(n_clicks)
    print(rvs_clicks_count())
    is_odd <- ifelse(n_clicks %% 2 == 0, F, T)
   
    rw <- selected_row_col[1]
    clm <- selected_row_col[2]
  
    # Current slot (player slot allocation)
    player_slot <- unlist(slots_table[rw, clm])[1]
    slot_free <- ifelse(is.na(player_slot), TRUE, FALSE)
    
    if(slot_free) {
      if(is_odd) {
        slots_table[rw, clm] <- 'P1'
        rvs_player_slots(slots_table)
      } else {
        slots_table[rw, clm] <- 'P2'
        rvs_player_slots(slots_table)
      }
    } 
    # shinyalert::shinyalert(title = "Check your input", text = "Please enter numeric for columns")
    
    # print(rvs_player_slots())
    
    if(is.na(current_value)) {
      temp_reactive_df[rw, clm] <- 1
    }
    
    type <- getNeighborType(temp_reactive_df, rw, clm)
    print(paste("you Clicked on ",type , " type of cell"))
    
    # shp <- giveMeShapesForThisValue(is_odd = is_odd, current_value = current_value)
    if(type == 'corner') {
      if (is.na(current_value)) {
        temp_reactive_df[x$V1, (x$V2+1)] <- 1
        rvs_tmp(temp_reactive_df)
        shp <- giveMeShapesForThisValue(is_odd = is_odd, current_value = 1)
        rvs$data[rw, clm] <- shp
      } else if (current_value >= 1) {
          if(is_odd) {
            if(unlist(slots_table[rw, clm]) == 'P1') {
              print('Do the reaction')
              ## -------- Reaction (Odd corner Begin)--------------->
              slots_table <- rvs_player_slots()
              cell <- list(c(x$V1, (x$V2+1)))
              res <- reactoR(data_frame = temp_reactive_df, cells = cell)
              # affected_cells_shp <- append(res$affected_cells, cell)
              # affected_cells_player <- res$affected_cells
              # browser()
              lst <- extract_elements(res)
              
              affected_cells_shp <- lst$affected_cells
              affected_cells_player <- lst$affected_cells
              
              temp_reactive_df <- lst$data_frame
              rvs_tmp(temp_reactive_df)
              print(rvs_tmp())
              
              #Update shapes 
              for(i in 1:length(affected_cells_shp)) {
                this_cell <- affected_cells_shp[[i]]
                value <- unlist(temp_reactive_df[this_cell[1], this_cell[2]])
                shp <- giveMeShapesForThisValue(is_odd = is_odd, current_value = value)
                rvs$data[this_cell[1], this_cell[2]] <- shp
              }            
              
              #Update slots 
              slots_table <- rvs_player_slots()

              for(i in 1:length(affected_cells_player)) {
                #Update slots table
                slots_table <- rvs_player_slots()
                this_cell <- affected_cells_player[[i]]
                current_player <- unlist(slots_table[rw, clm])
                
                slots_table[this_cell[1], this_cell[2]] <- current_player
                rvs_player_slots(slots_table) 
              }
              
              current_value <- unlist(temp_reactive_df[rw, clm])
              if(is.na(current_value))
                slots_table[rw, clm] <- NA
              
              rvs_player_slots(slots_table)
              # check player won?

              x <- rvs_player_slots()
              print(x)
              check_winning_moment(x)
              
              ## update the reactive df
                rvs_tmp(temp_reactive_df)
              ## -------- Reaction (Odd corner end)--------------->
            } else {
              n_clicks <- n_clicks - 1
              rvs_clicks_count(n_clicks)
              shinyalert::shinyalert(title = "Reserved for Player 1", 
                                     text = "Reserved for player 1")
            }
          } else {
            if(unlist(slots_table[rw, clm]) == 'P1') {
              n_clicks <- n_clicks - 1
              rvs_clicks_count(n_clicks)
              shinyalert::shinyalert(title = "Reserved for Player 2", 
                                     text = "Reserved for player 2")
            } else {
              print('Do the reaction')
              ## -------- Reaction (Even corner corner Begin)--------------->
              
              slots_table <- rvs_player_slots()
              cell <- list(c(x$V1, (x$V2+1)))
              res <- reactoR(data_frame = temp_reactive_df, cells = cell)
              # affected_cells_shp <- append(res$affected_cells, cell)
              # affected_cells_player <- res$affected_cells
              # browser()
              lst <- extract_elements(res)
              
              affected_cells_shp <- lst$affected_cells
              affected_cells_player <- lst$affected_cells
              
              temp_reactive_df <- lst$data_frame
              rvs_tmp(temp_reactive_df)
              print(rvs_tmp())
              
              #Update shapes 
              for(i in 1:length(affected_cells_shp)) {
                this_cell <- affected_cells_shp[[i]]
                value <- unlist(temp_reactive_df[this_cell[1], this_cell[2]])
                shp <- giveMeShapesForThisValue(is_odd = is_odd, current_value = value)
                rvs$data[this_cell[1], this_cell[2]] <- shp
              }            
              
              #Update slots 

              rvs_player_slots(slots_table)
              for(i in 1:length(affected_cells_player)) {
                #Update slots table
                slots_table <- rvs_player_slots()
                this_cell <- affected_cells_player[[i]]
                current_player <- unlist(slots_table[rw, clm])
                slots_table[this_cell[1], this_cell[2]] <- current_player
                rvs_player_slots(slots_table) 
              }
              current_value <- unlist(temp_reactive_df[rw, clm])
              if(is.na(current_value))
                slots_table[rw, clm] <- NA
              
              rvs_player_slots(slots_table)
              # check player won?
              
              x <- rvs_player_slots()
              print(x)
              check_winning_moment(x)
              
              ## update the reactive df
              rvs_tmp(temp_reactive_df)
              ## -------- Reaction (Even player corner end)--------------->
              
            }
          }
      }
    } else if(type == 'edge') {
      if (is.na(current_value)) {
        temp_reactive_df[x$V1, (x$V2+1)] <- 1
        rvs_tmp(temp_reactive_df)
        shp <- giveMeShapesForThisValue(is_odd = is_odd, current_value = 1)
        rvs$data[rw, clm] <- shp
      } else if(current_value == 1) {
        if(is_odd) {
          if(unlist(slots_table[rw, clm]) == 'P1') {
            temp_reactive_df[x$V1, (x$V2+1)] <- 2
            rvs_tmp(temp_reactive_df)
            shp <- giveMeShapesForThisValue(is_odd = is_odd, current_value = 2)
            rvs$data[x$V1, (x$V2+1)] <- shp
          } else {
            n_clicks <- n_clicks - 1
            rvs_clicks_count(n_clicks)
            shinyalert::shinyalert(title = "Reserved for Player 1", 
                                   text = "Reserved for player 1")
          }
        } else {
          if(unlist(slots_table[rw, clm]) == 'P1') {
            n_clicks <- n_clicks - 1
            rvs_clicks_count(n_clicks)
            shinyalert::shinyalert(title = "Reserved for Player 2", 
                                   text = "Reserved for player 2")
          } else {
            temp_reactive_df[x$V1, (x$V2+1)] <- 2
            
            rvs_tmp(temp_reactive_df)
            shp <- giveMeShapesForThisValue(is_odd = is_odd, current_value = 2)
            rvs$data[x$V1, (x$V2+1)] <- shp
          }
        }
      } else if (current_value == 2) {
        if(is_odd) {
          if(unlist(slots_table[rw, clm]) == 'P1') {
            print('Do the reaction')
            
            ## -------- Reaction (Odd edge Begin)--------------->
            
            slots_table <- rvs_player_slots()
            cell <- list(c(x$V1, (x$V2+1)))
            res <- reactoR(data_frame = temp_reactive_df, cells = cell)
            # affected_cells_shp <- append(res$affected_cells, cell)
            # affected_cells_player <- res$affected_cells
            # browser()
            lst <- extract_elements(res)
            
            affected_cells_shp <- lst$affected_cells
            affected_cells_player <- lst$affected_cells
            
            temp_reactive_df <- lst$data_frame
            rvs_tmp(temp_reactive_df)
            print(rvs_tmp())
            
            #Update shapes 
            for(i in 1:length(affected_cells_shp)) {
              this_cell <- affected_cells_shp[[i]]
              value <- unlist(temp_reactive_df[this_cell[1], this_cell[2]])
              shp <- giveMeShapesForThisValue(is_odd = is_odd, current_value = value)
              rvs$data[this_cell[1], this_cell[2]] <- shp
            }            
            
            #Update slots 
            slots_table <- rvs_player_slots()
            for(i in 1:length(affected_cells_player)) {
              #Update slots table
              slots_table <- rvs_player_slots()
              this_cell <- affected_cells_player[[i]]
              current_player <- unlist(slots_table[rw, clm])
              slots_table[this_cell[1], this_cell[2]] <- current_player
              rvs_player_slots(slots_table) 
            }
            current_value <- unlist(temp_reactive_df[rw, clm])
            if(is.na(current_value))
              slots_table[rw, clm] <- NA
            
            rvs_player_slots(slots_table)
            # check player won?
            
            x <- rvs_player_slots()
            print(x)
            check_winning_moment(x)
            
            ## update the reactive df
            rvs_tmp(temp_reactive_df)
            ## -------- Reaction (Odd edge end)--------------->
            
          } else {
            n_clicks <- n_clicks - 1
            rvs_clicks_count(n_clicks)
            shinyalert::shinyalert(title = "Reserved for Player 1", 
                                   text = "Reserved for player 1")
          }
        } else {
          if(unlist(slots_table[rw, clm]) == 'P1') {
            n_clicks <- n_clicks - 1
            rvs_clicks_count(n_clicks)
            shinyalert::shinyalert(title = "Reserved for Player 2", 
                                   text = "Reserved for player 2")
          } else {
            print("Do the reaction")
            
            
            ## -------- Reaction (even edge Begin)--------------->
            
            slots_table <- rvs_player_slots()
            cell <- list(c(x$V1, (x$V2+1)))
            res <- reactoR(data_frame = temp_reactive_df, cells = cell)
            # affected_cells_shp <- append(res$affected_cells, cell)
            # affected_cells_player <- res$affected_cells
            # browser()
            lst <- extract_elements(res)
            
            affected_cells_shp <- lst$affected_cells
            affected_cells_player <- lst$affected_cells
            
            temp_reactive_df <- lst$data_frame
            rvs_tmp(temp_reactive_df)
            print(rvs_tmp())
            
            #Update shapes 
            for(i in 1:length(affected_cells_shp)) {
              this_cell <- affected_cells_shp[[i]]
              value <- unlist(temp_reactive_df[this_cell[1], this_cell[2]])
              shp <- giveMeShapesForThisValue(is_odd = is_odd, current_value = value)
              rvs$data[this_cell[1], this_cell[2]] <- shp
            }            
            
            #Update slots 
            slots_table <- rvs_player_slots()
            for(i in 1:length(affected_cells_player)) {
              #Update slots table
              slots_table <- rvs_player_slots()
              this_cell <- affected_cells_player[[i]]
              current_player <- unlist(slots_table[rw, clm])
              slots_table[this_cell[1], this_cell[2]] <- current_player
              rvs_player_slots(slots_table) 
            }
            # check player won?
            
            current_value <- unlist(temp_reactive_df[rw, clm])
            if(is.na(current_value))
              slots_table[rw, clm] <- NA
            
            rvs_player_slots(slots_table)
            # check player won?
            
            x <- rvs_player_slots()
            print(x)
            check_winning_moment(x)
            
            ## update the reactive df
            rvs_tmp(temp_reactive_df)
            
            ## -------- Reaction (Even edge end)--------------->
            
          }
        }
        
      }
    } else if(type == 'other') {
      if (is.na(current_value)) {
        temp_reactive_df[x$V1, (x$V2+1)] <- 1
        rvs_tmp(temp_reactive_df)
        shp <- giveMeShapesForThisValue(is_odd = is_odd, current_value = 1)
        rvs$data[rw, clm] <- shp
      } else if(current_value == 1) {
        if(is_odd) {
          if(unlist(slots_table[rw, clm]) == 'P1') {
            temp_reactive_df[x$V1, (x$V2+1)] <- 2
            rvs_tmp(temp_reactive_df)
            shp <- giveMeShapesForThisValue(is_odd = is_odd, current_value = 2)
            rvs$data[x$V1, (x$V2+1)] <- shp
          } else {
            n_clicks <- n_clicks - 1
            rvs_clicks_count(n_clicks)
            shinyalert::shinyalert(title = "Reserved for Player 1", 
                                   text = "Reserved for player 1")
          }
        } else {
          if(unlist(slots_table[rw, clm]) == 'P1') {
            n_clicks <- n_clicks - 1
            rvs_clicks_count(n_clicks)
            shinyalert::shinyalert(title = "Reserved for Player 2", 
                                   text = "Reserved for player 2")
          } else {
            temp_reactive_df[x$V1, (x$V2+1)] <- 2
            rvs_tmp(temp_reactive_df)
            shp <- giveMeShapesForThisValue(is_odd = is_odd, current_value = 2)
            rvs$data[x$V1, (x$V2+1)] <- shp
          }
        }
      } else if (current_value == 2) {
        if(is_odd) {
          if(unlist(slots_table[rw, clm]) == 'P1') {
            temp_reactive_df[x$V1, (x$V2+1)] <- 3
            rvs_tmp(temp_reactive_df)
            shp <- giveMeShapesForThisValue(is_odd = is_odd, current_value = 3)
            rvs$data[x$V1, (x$V2+1)] <- shp
          } else {
            n_clicks <- n_clicks - 1
            rvs_clicks_count(n_clicks)
            shinyalert::shinyalert(title = "Reserved for Player 1", 
                                   text = "Reserved for player 1")
          }
        } else {
          if(unlist(slots_table[rw, clm]) == 'P1') {
            n_clicks <- n_clicks - 1
            rvs_clicks_count(n_clicks)
            shinyalert::shinyalert(title = "Reserved for Player 2", 
                                   text = "Reserved for player 2")
          } else {
            temp_reactive_df[x$V1, (x$V2+1)] <- 3
            rvs_tmp(temp_reactive_df)
            shp <- giveMeShapesForThisValue(is_odd = is_odd, current_value = 3)
            rvs$data[x$V1, (x$V2+1)] <- shp
          }
        }
      } else if(current_value == 3) {
        if(is_odd) {
          if(unlist(slots_table[rw, clm]) == 'P1') {
            print('Do the reaction')
            
            ## -------- Reaction (Odd other Begin)--------------->
            
            slots_table <- rvs_player_slots()
            cell <- list(c(x$V1, (x$V2+1)))
            res <- reactoR(data_frame = temp_reactive_df, cells = cell)
            # affected_cells_shp <- append(res$affected_cells, cell)
            # affected_cells_player <- res$affected_cells
            # browser()
            lst <- extract_elements(res)
            
            affected_cells_shp <- lst$affected_cells
            affected_cells_player <- lst$affected_cells
            
            temp_reactive_df <- lst$data_frame
            rvs_tmp(temp_reactive_df)
            print(rvs_tmp())
            
            #Update shapes 
            for(i in 1:length(affected_cells_shp)) {
              this_cell <- affected_cells_shp[[i]]
              value <- unlist(temp_reactive_df[this_cell[1], this_cell[2]])
              shp <- giveMeShapesForThisValue(is_odd = is_odd, current_value = value)
              rvs$data[this_cell[1], this_cell[2]] <- shp
            }            
            
            #Update slots 
            slots_table <- rvs_player_slots()
            for(i in 1:length(affected_cells_player)) {
              #Update slots table
              slots_table <- rvs_player_slots()
              this_cell <- affected_cells_player[[i]]
              current_player <- unlist(slots_table[rw, clm])
              # reactive_tmp <- rvs_tmp()
              # current_value <- reactive_tmp[this_cell[1], this_cell[2]]
              # if(!is.na(current_value))
              #   slots_table[this_cell[1], this_cell[2]] <- current_player
              slots_table[this_cell[1], this_cell[2]] <- current_player
              rvs_player_slots(slots_table) 
            }
            # check player won?
            current_value <- unlist(temp_reactive_df[rw, clm])
            if(is.na(current_value))
              slots_table[rw, clm] <- NA
            
            rvs_player_slots(slots_table)
            # check player won?
            
            x <- rvs_player_slots()
            print(x)
            check_winning_moment(x)
            
            ## update the reactive df
            rvs_tmp(temp_reactive_df)
            ## -------- Reaction (Odd other end)--------------->
            
          } else {
            n_clicks <- n_clicks - 1
            rvs_clicks_count(n_clicks)
            shinyalert::shinyalert(title = "Reserved for Player 1", 
                                   text = "Reserved for player 1")
          }
        } else {
          if(unlist(slots_table[rw, clm]) == 'P1') {
            n_clicks <- n_clicks - 1
            rvs_clicks_count(n_clicks)
            shinyalert::shinyalert(title = "Reserved for Player 2", 
                                   text = "Reserved for player 2")
          } else {
            print("Do the reaction")
            
            ## -------- Reaction (even other Begin)--------------->
            
            slots_table <- rvs_player_slots()
            cell <- list(c(x$V1, (x$V2+1)))
            res <- reactoR(data_frame = temp_reactive_df, cells = cell)
            # affected_cells_shp <- append(res$affected_cells, cell)
            # affected_cells_player <- res$affected_cells
            # browser()
            lst <- extract_elements(res)
            
            affected_cells_shp <- lst$affected_cells
            affected_cells_player <- lst$affected_cells
            
            temp_reactive_df <- lst$data_frame
            rvs_tmp(temp_reactive_df)
            print(rvs_tmp())
            
            # browser()
            #Update shapes 
            for(i in 1:length(affected_cells_shp)) {
              this_cell <- affected_cells_shp[[i]]
              value <- unlist(temp_reactive_df[this_cell[1], this_cell[2]])
              shp <- giveMeShapesForThisValue(is_odd = is_odd, current_value = value)
              rvs$data[this_cell[1], this_cell[2]] <- shp
            }            
            
            #Update slots 
            slots_table <- rvs_player_slots()
            for(i in 1:length(affected_cells_player)) {
              #Update slots table
              slots_table <- rvs_player_slots()
              this_cell <- affected_cells_player[[i]]
              current_player <- unlist(slots_table[rw, clm])
              slots_table[this_cell[1], this_cell[2]] <- current_player
              rvs_player_slots(slots_table) 
            }
            # check player won?
            current_value <- unlist(temp_reactive_df[rw, clm])
            if(is.na(current_value))
              slots_table[rw, clm] <- NA
            
            rvs_player_slots(slots_table)
            # check player won?
            
            x <- rvs_player_slots()
            print(x)
            check_winning_moment(x)
            
            ## update the reactive df
            rvs_tmp(temp_reactive_df)
            
            ## -------- Reaction (even other end)--------------->
            
          }
        }
      }
    }

  })
 
  proxy = dataTableProxy('game_board')
  observe({
    DT::replaceData(proxy, rvs$data, rownames = FALSE, resetPaging = FALSE)
  })

}

shinyApp(ui = ui, server = shinyServer)
