gta_hs_sql_save <- function(path) {

  # Construct the update query by looping over the data fields
  query <- sprintf(
    "INSERT INTO %s (%s) VALUES ('%s')",
    table, 
    paste(names(data), collapse = ", "),
    paste(data, collapse = "', '")
  )
  
  
  sql <- "SELECT * FROM City WHERE ID = ?id1 OR ID = ?id2 OR ID = ?id3;"
  query <- sqlInterpolate(conn, sql, id1 = input$ID1,
                          id2 = input$ID2, id3 = input$ID3)
  # Submit the update query and disconnect
  dbGetQuery(ricardo.connection, query)
  
}

