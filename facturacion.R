





facturacion <- function(path){

  library(googledrive)
  library(dplyr)
  library(lubridate)
  library(filesstrings)

  list_files <- drive_ls(as_id(path))

  info <-
    tibble(
      `Rfc emisor` = c(),
      `Nombre emisor` = c(),
      `Rfc receptor` = c(),
      `Timbre fiscal digital` = c(),
      `Fecha timbrado` = c(),
      Cuenta = c(),
      total = c(),
      subtotal = c()
    )



  for (i in 1:nrow(list_files)){

    x <- drive_ls(as_id(list_files$id[i]))

    for (j in 1:nrow(x)){

      y <- drive_ls(as_id(x$id[j]))


      for (k in 1:nrow(y)){

        if (endsWith(y$name[k], ".xml") == T){
          drive_download(y$id[k], path = paste0("facturas/",y$name[k]), overwrite = T)

          val <- XML::xmlToList(
            XML::xmlParse(
              paste0(
                "facturas/",dir("facturas")[1]
              )))

          extraccion <-
            tibble(
              `Rfc emisor` = val$Emisor[1],
              `Nombre emisor` = val$Emisor[2],
              `Rfc receptor` = val$Receptor[1],
              `Timbre fiscal digital` = val[["Complemento"]][["TimbreFiscalDigital"]]@.Data[3],
              `Fecha timbrado` = val[["Complemento"]][["TimbreFiscalDigital"]]@.Data[4],
              total = val$.attrs[["Total"]],
              subtotal = val$.attrs[["SubTotal"]]
            ) |>
            mutate(Cuenta = list_files$name[i])
          info<-
            info |>
            bind_rows(
              extraccion
            )

          file.move(paste0("facturas/",list.files("facturas")),
                    destinations =paste0("facturas procesadas/",list.files("facturas")) )


        }





      }
    }

  }




  info <-
    info %>%
    mutate(
      timbre = if_else(
        nchar(`Timbre fiscal digital`) >19,`Timbre fiscal digital`,
        `Fecha timbrado`
      ),
      fecha = if_else(
        nchar(`Timbre fiscal digital`) == 19,`Timbre fiscal digital`,
        `Fecha timbrado`
      )
    ) %>%
    select(-one_of(c("Timbre fiscal digital", "Fecha timbrado")))

  writexl::write_xlsx(info, path = "excel/facturas.xlsx")



}









