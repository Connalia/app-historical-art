library(shiny)
library(DT)

mymtcars <- mtcars
mymtcars[["Select"]] <- paste0('<input type="checkbox" name="row_selected" value=',1:nrow(mymtcars),' checked>')
mymtcars[["_id"]] <- paste0("row_", seq(nrow(mymtcars)))

callback <- c(
  sprintf("table.on('click', 'td:nth-child(%d)', function(){", 
          which(names(mymtcars) == "Select")),
  "  var checkbox = $(this).children()[0];",
  "  var $row = $(this).closest('tr');",
  "  if(checkbox.checked){",
  "    $row.removeClass('excluded');",
  "  }else{",
  "    $row.addClass('excluded');",
  "  }",
  "  var excludedRows = [];",
  "  table.$('tr').each(function(i, row){",
  "    if($(this).hasClass('excluded')){",
  "      excludedRows.push(parseInt($(row).attr('id').split('_')[1]));",
  "    }",
  "  });",
  "  Shiny.setInputValue('excludedRows', excludedRows);",
  "});"
)

ui = fluidPage(
  verbatimTextOutput("excludedRows"),
  DTOutput('myDT')
)

server = function(input, output) {
  
  output$myDT <- renderDT({
    
    datatable(
      mymtcars, selection = "multiple",
      options = list(pageLength = 5,
                     lengthChange = FALSE,
                     rowId = JS(sprintf("function(data){return data[%d];}", 
                                        ncol(mymtcars)-1)),
                     columnDefs = list( # hide the '_id' column
                       list(visible = FALSE, targets = ncol(mymtcars)-1)
                     )
      ),
      rownames = FALSE,
      escape = FALSE,
      callback = JS(callback)
    )
  }, server = FALSE)
  
  output$excludedRows <- renderPrint({
    input[["excludedRows"]]
  })
}

shinyApp(ui,server, options = list(launch.browser = TRUE))