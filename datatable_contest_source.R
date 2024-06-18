## ---------------------Utils--------->
initialize_matrix <- function(nrow, ncol, value="") {
  res <- as.data.frame(matrix(data = value, nrow = nrow, ncol = ncol))
  row.names(res) <- NULL
  names(res) <- NULL
  return(res)
}

checkInputValidity <- function(char) {
  flag <- "good"
  if(!is.na(as.numeric(char))) {
    shinyalert::shinyalert("Numeric inpputs are not allowed", "error")
    flag <- "bad"
  }
  n_letters <- strsplit(char, split = ".") %>% unlist() %>% length()
  if(n_letters != 1) {
    shinyalert::shinyalert("Please enter one character at a time", "error")
    flag <- "bad"
  }
  return(flag)
}

textInputRow<-function (inputId, label, value = "") {
  div(style="display:inline-block",
      tags$label(label, `for` = inputId), 
      tags$input(id = inputId, type = "text", value = value,class="input-small"))
}

# Definition
which_corner <- function(data_frame, row, column) {
  if(row == 1 && column == 1) {
    return('top_left_corner')
  } else if(row == 1 && column == ncol(data_frame)) {
    return('top_right_corner')
  } else if(row == nrow(data_frame) && column == 1) {
    return('bottom_left_corner')
  } else if(row == nrow(data_frame) && column == ncol(data_frame)) {
    return('bottom_right_corner')
  } else {
    return('not_a_corner')
  }
}

# Call
test_df <- as.data.frame(matrix(NA, 10,10))
# which_corner(test_df, 10, 10)
# which_corner(test_df, 4, 1)
# which_corner(test_df, 1, 4)
# which_corner(test_df, 1, 1)

which_edge <- function(data_frame, row, column) {
  corner <- which_corner(data_frame, row, column)
  if(corner == 'not_a_corner' && sum(c(column,row) %in% c(dim(data_frame), 1)) >= 1) {
    if(column %in% dim(data_frame)){
      return("right_most")
    } else if(column == 1) {
      return("left_most")
    } else if (row %in% dim(data_frame)) {
      return("bottom_most")
    } else if(row == 1) {
      return("top_most")
    }
  } else {
    return("not_an_edge")
  }
}
# Call
# which_edge(test_df, 2, 2)
# which_edge(test_df, 1,3)
# which_edge(test_df, 4,3)
# which_edge(test_df, 3,4)
# which_edge(test_df, 3,1)

getNeighborsForThisCorner <- function(data_frame, row_index, column_index) {
  corner <- which_corner(data_frame, row_index, column_index)
  # browser()
  if(corner %in% c('top_left_corner', "top_right_corner", "bottom_left_corner", "bottom_right_corner")) {
    if(corner == "top_left_corner"){
      neightbours <- list(c(row_index + 1, column_index), c(row_index, column_index + 1))
    }
    if(corner == "top_right_corner") {
      neightbours <- list(c(row_index + 1, column_index), c(row_index, column_index - 1))
    }
    if(corner == "bottom_left_corner"){
      neightbours <- list(c(row_index -1, column_index), c(row_index, column_index + 1))
    }
    if(corner == 'bottom_right_corner') {
      neightbours <- list(c(row_index -1, column_index), c(row_index, column_index + 1))
    }
  } else {
    neightbours <- "not_a_corner, Perhaps! it's an edge of DF"
  }
  return(neightbours)
}

# Call
# getNeighborsForThisCorner(test_df, 1,1)
# getNeighborsForThisCorner(test_df, 1,10)
# getNeighborsForThisCorner(test_df, 10, 1)
# getNeighborsForThisCorner(test_df, 10, 10)

exclude_items <- function(my_list, excluded_values) {
  filtered_list <- lapply(my_list, function(sub_list) {
    if (any(sub_list %in% excluded_values)) {
      NULL  # Exclude the entire sub-list
    } else {
      sub_list
    }
  })
  filtered_list <- filtered_list[!sapply(filtered_list, is.null)]  # Remove NULL sub-lists
  return(filtered_list)
}

# Example usage with a list of vectors:
original_list <- list(
  a = c(1, 2, 0, 5, 10, -1, 7),
  b = c(3, 8),
  c = c(-1, 2, 4, 6)
)
excluded_values <- c(0, -1, 10)
filtered_result <- exclude_items(original_list, excluded_values)
# print(filtered_result)


# This works for any cell including corner, edge or other cells
getNeighboursForThisCell <- function(data_frame, row_index, column_index) {
  edge <- which_edge(data_frame, row_index, column_index)
  res <- list(
    c(row_index, column_index - 1),
    c(row_index, column_index + 1),
    c(row_index + 1, column_index),
    c(row_index - 1, column_index)
  )
  
  excluded_values <- c(0, -1, nrow(data_frame + 1), ncol(data_frame) + 1)
  valid_neighbours <- exclude_items(res, excluded_values)
  return(valid_neighbours)
}

#Call
# df <- as.data.frame(matrix(NA, 5, 5))
# getNeighboursForThisCell(df, 1, 2)

# Distribution
# distributeToNeighbours <- function(data_frame, row_index, column_index) {
#   neighbours <- getNeighboursForThisCell(data_frame, row_index, column_index)
#   
#   for( j in neighbours) {
#     current_value <- data_frame[j[1], j[2]]
#     if(is.na(current_value)) {
#       data_frame[j[1], j[2]] <- 1
#     } else {
#       data_frame[j[1], j[2]] <- data_frame[j[1], j[2]] + 1
#     }
#   }
#   data_frame[row_index, column_index] <- NA
#   return(data_frame)
# }

distributeToNeighbours <- function(data_frame, row_index, column_index) {
  neighbours <- getNeighboursForThisCell(data_frame, row_index, column_index)
  for( j in neighbours) {
    current_value <- data_frame[j[1], j[2]]
    
    if(is.data.frame(current_value))
      current_value <- unlist(current_value)
    
    if(is.na(current_value)) {
      data_frame[j[1], j[2]] <- 1
    } else {
      data_frame[j[1], j[2]] <- current_value + 1
    }
  }
  data_frame[row_index, column_index] <- NA
  return(data_frame)
}

# Call
# test_df[1,1] <- 2
# distributeToNeighbours(test_df, 1, 1)
# test_df[1,2] <- 3
# distributeToNeighbours(test_df, 1, 2)
# test_df[2,2] <- 4
# distributeToNeighbours(test_df, 2, 2)

getNeighborType <- function(data_frame, row_index, column_index) {
  corner <- which_corner(data_frame = data_frame, 
                         row = row_index, 
                         column = column_index)
  edge <- which_edge(data_frame = data_frame, 
                     row = row_index, 
                     column = column_index)
  is_corner <- corner %in% c('top_left_corner', 
                             'top_right_corner', 
                             'bottom_left_corner', 
                             'bottom_right_corner')
  is_edge <- edge %in% c('top_most', 
                         'bottom_most', 
                         'left_most', 
                         'right_most')
  if(is_corner){
    return("corner")
  } else if(is_edge) {
    return('edge')
  } else {
    return('other')
  }
}

#Call
df <- as.data.frame(matrix(NA, 5, 5))
getNeighborType(df, 5,5)

check_winning_moment <- function(df) {
  res <- unique(na.omit(as.vector(as.matrix(df))))
  if(length(res) == 1) {
    if(res == 'P1') {
      color <- "#CC6CE7"
      t <- "Player 1"
      m <- "Winner Winner Chicken Dinner!"
      # t <- paste0("<p style='font-size: 50px; font-family: Arial, sans-serif; color:", color, ";'>" , "Player 1", "</p>")
      # m <- paste0("<p style='font-size: 50px; font-family: Arial, sans-serif; color:", color, ";'>" , "Winner Winner Chicken Dinner!!", "</p>")
    } else {
      color <- "#7DDA58"
      t <- "Player 2"
      m <- "Winner Winner Chicken Dinner!"
      # t <- paste0("<p style='font-size: 50px; font-family: Arial, sans-serif; color:", color, ";'>" , "Player 2", "</p>")
      # m <- paste0("<p style='font-size: 50px; font-family: Arial, sans-serif; color:", color, ";'>" , "Winner Winner Chicken Dinner!!", "</p>")
    }
      
    shinyalert(title = t, 
               text = m)
  } else {
    return(res)
  }
}

cells <- list(c(3, 3), c(3,2))
cells
reactoR <- function(data_frame, cells) {
  # get the valid neighbors
  print(length(cells))
  affected_cells <- list()
  if(length(cells) == 0) { # call reactor recursively
    print("We are in if section")
    cells <- list()
    res <- list(data_frame = data_frame, cells = list())
    return(res)
  } else {
    print("we are in else section")
    j <- lapply(cells, FUN = function(cell) {
      row_index <- cell[1]
      column_index <- cell[2]
      the_neighbours <- getNeighboursForThisCell(data_frame, row_index, column_index)
      #Share the value to neighbor
      data_frame <- distributeToNeighbours(data_frame, row_index, column_index)
      #Get neighbors_type
      neighbors_type <- lapply(the_neighbours, FUN = function(x) {
        getNeighborType(data_frame = data_frame, row_index = x[1], column_index = x[2])
      }) %>% unlist()

      # Get Neighbor_scores
      neighbor_scores_after_distribution <- lapply(the_neighbours, FUN = function(x) {
        return(data_frame[x[1], x[2]])
      }) %>% unlist()

      is_neighbor_burstable <- lapply(1:length(neighbor_scores_after_distribution),
                                      FUN = function(x) {
                                        burstable <- (neighbors_type[x] == 'corner' && neighbor_scores_after_distribution[x] >= 2) ||
                                          (neighbors_type[x] == 'edge' && neighbor_scores_after_distribution[x] >= 3) ||
                                          (neighbors_type[x] == 'other' && neighbor_scores_after_distribution[x] >= 4)
                                        if(burstable){
                                          return(list(neighbor = the_neighbours[[x]], burstable = TRUE))
                                        } else {
                                          return(list(neighbor = the_neighbours[[x]], burstable = FALSE))
                                        }
                                      })

      burstable_cells <- lapply(is_neighbor_burstable, function(x) {
        if(T %in% x)
          x$neighbor
        else
          NA
      })

      burstable_cells <- burstable_cells[!is.na(burstable_cells)]
      list(data_frame = data_frame, burstable_cells = burstable_cells, the_neighbours = the_neighbours)
    })
    temp_cells <- lapply(1:length(j), FUN = function(i) {
      item <- j[[i]]
      item$burstable_cells
    }) %>% do.call('c', .)
    the_neighbours <- lapply(1:length(j), FUN = function(i) {
      item <- j[[i]]
      item$the_neighbours
    }) %>% do.call('c', .) 
    affected_cells <- append(affected_cells, the_neighbours) 
    data_frame <- j[[length(j)]]$data_frame
    res <- list(data_frame = data_frame, cells = temp_cells, affected_cells = affected_cells)
  }
  data_frame <- res$data_frame
  cells <- unique(res$cells)
  affected_cells <- res$affected_cells
  # print(paste("affected_cells", affected_cells))
  return(list(affected_cells = affected_cells, result = reactoR(data_frame, cells)))
}


three_dots <- function(color = NULL) {
  colored <- paste('<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <style>
        .circle-container {
            position: relative;
            width: 100px;
            height: 100px;
        }
        .circle {
            position: absolute;
            width: 50px;
            height: 50px;
            background-color:', color, ';
            border-radius: 50%;
        }
        .circle1 {
            top: 0;
            left: 0;
        }
        .circle2 {
            top: 2px;
            left: 48px;
        }
        .circle3 {
        	top:42px;
            left: 20px;
        }
    </style>
</head>
<body>
    <div class="circle-container">
        <div class="circle circle1"></div>
        <div class="circle circle2"></div>
        <div class="circle circle3"></div>
    </div>
</body>
</html>')
  
  x <- '<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <style>
        .circle-container {
            position: relative;
            width: 100px;
            height: 100px;
        }
        .circle {
            position: absolute;
            width: 50px;
            height: 50px;
            background-color: #3498db;
            border-radius: 50%;
        }
        .circle1 {
            top: 0;
            left: 0;
        }
        .circle2 {
            top: 2px;
            left: 48px;
        }
        .circle3 {
        	top:42px;
            left: 20px;
        }
    </style>
</head>
<body>
    <div class="circle-container">
        <div class="circle circle1"></div>
        <div class="circle circle2"></div>
        <div class="circle circle3"></div>
    </div>
</body>
</html>'
  if (is.null(color)) 
    return(x)
  else 
    return(colored)
}



extract_elements <- function(recursive_list) {
  stop <- 0
  affected_cells <- list()
  data_frame <- list()
  x <- recursive_list
  while(stop == 0) {
    
    if("data_frame" %in% names(x)) {
      data_frame <- x$data_frame
    }
    
    if('result' %in% names(x)) {
      tmp <- x$affected_cells
      x <- x$result
      affected_cells <- append(tmp, affected_cells)
      stop <- 0
    } else {
      
      stop <- 1
    }
  }
  return(list(data_frame = data_frame, affected_cells = unique(affected_cells)))
}

# Call
# x <- readRDS('chainReaction/list_test.rds')
y <- extract_elements(x)
y

two_dots <- function(color = NULL) {
  colored <- paste('<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <style>
        .circle-container {
            position: relative;
            width: 100px;
            height: 100px;
        }
        .circle {
            position: absolute;
            width: 50px;
            height: 50px;
            background-color: ', color, ';
            border-radius: 50%;
        }
        .circle1 {
            top: 0;
            left: 0;
        }
        .circle2 {
            top: 2px;
            left: 48px;
        }
        }
    </style>
</head>
<body>
    <div class="circle-container">
        <div class="circle circle1"></div>
        <div class="circle circle2"></div>
    </div>
</body>
</html>')
  x <- '<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <style>
        .circle-container {
            position: relative;
            width: 100px;
            height: 100px;
        }
        .circle {
            position: absolute;
            width: 50px;
            height: 50px;
            background-color: #3498db;
            border-radius: 50%;
        }
        .circle1 {
            top: 0;
            left: 0;
        }
        .circle2 {
            top: 2px;
            left: 48px;
        }
        }
    </style>
</head>
<body>
    <div class="circle-container">
        <div class="circle circle1"></div>
        <div class="circle circle2"></div>
    </div>
</body>
</html>'
  if(is.null(color))
    return(x)
  else return(colored)
}

a_dot <- function(color = NULL) {
  colored <- paste('<!DOCTYPE html>
<html>
<head>
<meta name="viewport" content="width=device-width, initial-scale=1">
<style>
.dot {
  height: 25px;
  width: 25px;
  background-color:', color, ';
  border-radius: 50%;
  display: inline-block;
  color: #fef;
}
</style>
</head>
<body>

<div style="text-align:center">
  <span class="dot"></span>
</div>

</body>
</html>')
  x <- '<!DOCTYPE html>
<html>
<head>
<meta name="viewport" content="width=device-width, initial-scale=1">
<style>
.dot {
  height: 25px;
  width: 25px;
  background-color: #7DDA58;
  border-radius: 50%;
  display: inline-block;
  color: #fef;
}
</style>
</head>
<body>

<div style="text-align:center">
  <span class="dot"></span>
</div>

</body>
</html>'
if(is.null(color)) {
  return(x)
} else {
  return(colored)
}
}

giveMeShapesForThisValue <- function(is_odd, current_value) {
  print("Generating the Shapes..") 
  if(is.na(current_value)) {
    val <- NA
  } else {
    
    # Determine the color
    if(is_odd) {
      color <- "#CC6CE7"
    } else {
      color <- "#7DDA58"
    }
    
    if(current_value == 1) {
      # val <- a_dot(color)
      val <- a_number(color, 1)
    } else if (current_value == 2) {
      # val <- two_dots(color)
      val <- a_number(color, 2)
    } else if(current_value == 3) {
      # val <- three_dots(color)
      val <- a_number(color, 3)
    } else if (current_value > 3) {
      val <- a_number(color, 3)
    } else if (is.na(current_value)) {
      val <- NA
    } else {
      print("Sorry! giveMeShapesForThisValue Can't give more than 3 shapes")
    }
    
  }
  return(val)
}

a_number <- function(color, number){
  paste0("<p style='font-size: 50px; font-family: Arial, sans-serif; color:", color, ";'>" , number, "</p>")
}
a_number('Tomato', 3)
#Backup
# observeEvent(input$game_board_cells_selected, {
#   req(input$game_board_cells_selected)
#   board <- rvs$data
# 
#   # The tracker for number of clicks to save in the back end
#   temp_reactive_df <- rvs_tmp()
#   # Cell value from the board
#   x <- as.data.frame(input$game_board_cells_selected)
#   # print(x) # R, C
#   selected_row_col <- c(x$V1, (x$V2+1))
#   current_value <- unlist(temp_reactive_df[x$V1, (x$V2+1)])[1]
#   # current_value <- unlist(current_value)[1]
#   # keep track of number of clicks
#   n_clicks <- rvs_clicks_count()
#   n_clicks <- n_clicks + 1
#   rvs_clicks_count(n_clicks)
#   is_odd <- ifelse(n_clicks %% 2 == 0, F, T)  # To find out even | odd, determine player color
#   # print(paste("n_clicks===", n_clicks))
#   if (is.na(current_value)) {
#     temp_reactive_df[x$V1, (x$V2+1)] <- 1
#     rvs_tmp(temp_reactive_df)
#   } else if(current_value == 1) {
#     temp_reactive_df[x$V1, (x$V2+1)] <- 2
#     rvs_tmp(temp_reactive_df)
#   } else if (current_value == 2) {
#     temp_reactive_df[x$V1, (x$V2+1)] <- 3
#     rvs_tmp(temp_reactive_df)
#   } else if(current_value == 3) {
#     # reactoR(data_frame = temp_reactive_df, row_index = x$V1, column_index = (x$V2+1))
#     print("Do the chain reaction")
#   }
# 
#   updated_n_click_count <- temp_reactive_df[x$V1, (x$V2+1)]
#   updated_n_click_count <- unlist(updated_n_click_count)
#   # print("board_data======")
#   # print(rvs$data)
#   # print("Backend data===")
#   # print(rvs_tmp())
#   if(is.na(updated_n_click_count)) {
# 
#     rvs$data[x$V1, (x$V2+1)] <- NA
# 
#   } else if(updated_n_click_count == 1) {
# 
#     if(is_odd) {
#       rvs$data[x$V1, (x$V2+1)] <- a_dot("#CC6CE7")
#     } else {
#       rvs$data[x$V1, (x$V2+1)] <- a_dot("#7DDA58")
#     }
# 
#   } else if (updated_n_click_count == 2) {
# 
#     if(is_odd)  {
#       rvs$data[x$V1, (x$V2+1)] <- two_dots("#CC6CE7")
#     } else {
#       rvs$data[x$V1, (x$V2+1)] <- two_dots("#7DDA58")
#     }
# 
#   } else if(updated_n_click_count == 3) {
#     if(is_odd)  {
#       rvs$data[x$V1, (x$V2+1)] <- three_dots("#CC6CE7")
#     } else {
#       rvs$data[x$V1, (x$V2+1)] <- three_dots("#7DDA58")
#     }
#   } else {
#     "Do the distribution"
#   }
# 
# })

# lst <- list(data_frame, row_index, column_index)
# reactoR <- function(lst) {
#   # get the valid neigubors
#   data_frame <- lst$data_frame
#   row_index <- lst$row_index
#   column_index <- lst$column_index
#   
#   the_neighbours <- getNeighboursForThisCell(data_frame, row_index, column_index)
#   #Share the value to neighbor
#   data_frame <- distributeToNeighbours(data_frame, row_index, column_index)
# 
# if(flag) { # call reactor recursively
#     
#     #Get neighbors_type
#     neighbors_type <- lapply(the_neighbours, FUN = function(x) {
#       getNeighborType(data_frame = data_frame, row_index = x[1], column_index = x[2])
#     }) %>% unlist()
#     
#     # Get Neighbor_scores 
#     neighbor_scores_after_distribution <- lapply(the_neighbours, FUN = function(x) {
#       return(data_frame[x[1], x[2]])
#     }) %>% unlist()
#     # if(row_index == 3 & column_index == 2) {
#     #   browser()
#     # }
#     is_neighbor_burstable <- lapply(1:length(neighbor_scores_after_distribution), 
#                                     FUN = function(x) {
#                                       burstable <- (neighbors_type[x] == 'corner' && neighbor_scores_after_distribution[x] >= 2) ||
#                                         (neighbors_type[x] == 'edge' && neighbor_scores_after_distribution[x] >= 3) || 
#                                         (neighbors_type[x] == 'other' && neighbor_scores_after_distribution[x] >= 4)
#                                       if(burstable){
#                                         return(list(neighbor = the_neighbours[[x]], burstable = TRUE))
#                                       } else {
#                                         return(list(neighbor = the_neighbours[[x]], burstable = FALSE))
#                                       }
#                                     })
#     
#     flag <- (lapply(is_neighbor_burstable, FUN = function(j) {j[[2]]}) %>% unlist() %>% sum) >= 1
#     
#     burstable_cells <- lapply(is_neighbor_burstable, function(x) {
#       if(T %in% x)
#         x 
#       else 
#         NA
#     })
#     burstable_cells <- burstable_cells[!is.na(burstable_cells)]
#     # print(burstable_cells)
#     # if(length(burstable_cells)   > 1) {
#     #   browser()
#     # }
#     
#     res <- lapply(burstable_cells, FUN = function(sub_neighbor) {
#       # print(sub_neighbor)
#       # print(sub_neighbor$neighbor)
#       data_frame <- reactoR(data_frame = data_frame,
#                             row_index = sub_neighbor$neighbor[1],
#                             column_index = sub_neighbor$neighbor[2])
#     })
#     data_frame <- res[[length(res)]]
#   } else {
#     flag = 0
#   }
#   return(data_frame)
# }

# reactoR <- function(data_frame, row_index, column_index) {
#   # get the valid neigubors
#   
#   the_neighbours <- getNeighboursForThisCell(data_frame, row_index, column_index)
#   #Share the value to neighbor
#   data_frame <- distributeToNeighbours(data_frame, row_index, column_index)
#   #Get neighbors_type
#   neighbors_type <- lapply(the_neighbours, FUN = function(x) {
#     getNeighborType(data_frame = data_frame, row_index = x[1], column_index = x[2])
#   }) %>% unlist()
#   
#   # Get Neighbor_scores
#   neighbor_scores_after_distribution <- lapply(the_neighbours, FUN = function(x) {
#     return(data_frame[x[1], x[2]])
#   }) %>% unlist()
#   # if(row_index == 3 & column_index == 2) {
#   #   browser()
#   # }
#   is_neighbor_burstable <- lapply(1:length(neighbor_scores_after_distribution),
#                                   FUN = function(x) {
#                                     burstable <- (neighbors_type[x] == 'corner' && neighbor_scores_after_distribution[x] >= 2) ||
#                                       (neighbors_type[x] == 'edge' && neighbor_scores_after_distribution[x] >= 3) ||
#                                       (neighbors_type[x] == 'other' && neighbor_scores_after_distribution[x] >= 4)
#                                     if(burstable){
#                                       return(list(neighbor = the_neighbours[[x]], burstable = TRUE))
#                                     } else {
#                                       return(list(neighbor = the_neighbours[[x]], burstable = FALSE))
#                                     }
#                                   })
#   
#   flag <- (lapply(is_neighbor_burstable, FUN = function(j) {j[[2]]}) %>% unlist() %>% sum) >= 1
#   
#   burstable_cells <- lapply(is_neighbor_burstable, function(x) {
#     if(T %in% x)
#       x
#     else
#       NA
#   })
#   burstable_cells <- burstable_cells[!is.na(burstable_cells)]
#   # print(burstable_cells)
#   # if(length(burstable_cells)   > 1) {
#   #   browser()
#   # }
#   if(flag) { # call reactor recursively
#     res <- lapply(burstable_cells, FUN = function(sub_neighbor) {
#       # print(sub_neighbor)
#       # print(sub_neighbor$neighbor)
#       data_frame <- reactoR(data_frame = data_frame,
#                             row_index = sub_neighbor$neighbor[1],
#                             column_index = sub_neighbor$neighbor[2])
#     })
#     data_frame <- res[[length(res)]]
#   } else {
#     flag = 0
#   }
#   return(data_frame)
# }

# test_df <- as.data.frame(matrix(NA, 5,5))
# test_df[1, 4] <- 2
# test_df[2, 5] <- 2
# test_df[2, 4] <- 2
# cells <- list(c(5,5))
# test_df
# res <- reactoR(test_df, cells)
# 
# test_df[1, 2] <- 2
# test_df[2, 1] <- 2
# test_df[2, 2] <- 1
# cells <- list(c(1, 1))
# # 
# test_df
# res <- reactoR(test_df, cells)

# res <- reactoR(test_df, cells)
# res$result$result$result$data_frame
# res$result$result$data_frame
#

# test_df <- as.data.frame(matrix(NA, 5,5))
# test_df[3,3] <- 3 
# test_df[3,2] <- 3
# test_df
# test_df <- reactoR(test_df, cells)
# test_df

# reactoR <- function(data_frame, cells) {
# 
#   while(length(cells) != 0) {
#     j <- lapply(cells, FUN = function(cell) {
#       the_neighbours <- getNeighboursForThisCell(data_frame, row_index, column_index)
#       #Share the value to neighbor
#       data_frame <- distributeToNeighbours(data_frame, row_index, column_index)
#       data <- append(data, data_frame)
#       #Get neighbors_type
#       neighbors_type <- lapply(the_neighbours, FUN = function(x) {
#         getNeighborType(data_frame = data_frame, row_index = x[1], column_index = x[2])
#       }) %>% unlist()
# 
#       # Get Neighbor_scores
#       neighbor_scores_after_distribution <- lapply(the_neighbours, FUN = function(x) {
#         return(data_frame[x[1], x[2]])
#       }) %>% unlist()
#       is_neighbor_burstable <- lapply(1:length(neighbor_scores_after_distribution),
#                                       FUN = function(x) {
#                                         burstable <- (neighbors_type[x] == 'corner' && neighbor_scores_after_distribution[x] >= 2) ||
#                                           (neighbors_type[x] == 'edge' && neighbor_scores_after_distribution[x] >= 3) ||
#                                           (neighbors_type[x] == 'other' && neighbor_scores_after_distribution[x] >= 4)
#                                         if(burstable){
#                                           return(list(neighbor = the_neighbours[[x]], burstable = TRUE))
#                                         } else {
#                                           return(list(neighbor = the_neighbours[[x]], burstable = FALSE))
#                                         }
#                                       })
# 
#       burstable_cells <- lapply(is_neighbor_burstable, function(x) {
#         if(T %in% x)
#           x$neighbor
#         else
#           NA
#       })
#       burstable_cells <- burstable_cells[!is.na(burstable_cells)]
#       return(list(burstable_cells, data_frame))
#     }) #j lapply end
#   } #while end
#   
# }
# 


# for (i in 1:50) {
#   # system('sleep 1')
# test_df <- reactoR(test_df, cell)
#   print(test_df)
# }
