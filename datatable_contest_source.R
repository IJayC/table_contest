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
getNeighboursForThisCell(test_df, 1,1)


distributeToNeighbours <- function(data_frame, row_index, column_index) {
  neighbours <- getNeighboursForThisCell(data_frame, row_index, column_index)
  
  for( j in neighbours) {
    current_value <- data_frame[j[1], j[2]]
    if(is.na(current_value)) {
      data_frame[j[1], j[2]] <- 1
    } else {
      data_frame[j[1], j[2]] <- data_frame[j[1], j[2]] + 1
    }
  }
  data_frame[row_index, column_index] <- NA
  return(data_frame)
}

# Call
test_df[1,1] <- 2
distributeToNeighbours(test_df, 1, 1)
test_df[1,2] <- 3
distributeToNeighbours(test_df, 1, 2)
test_df[2,2] <- 4
distributeToNeighbours(test_df, 2, 2)

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
                             'top_right_corner')
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



# for (i in 1:50) {
#   # system('sleep 1')
# test_df <- reactoR(test_df, cell)
#   print(test_df)
# }



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
