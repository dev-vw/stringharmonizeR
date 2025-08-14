#' Run validateR
#'
#' @param vec string vector to modify and check
#' @param compvec comparison string vector
#'
#' @importFrom dplyr left_join
#'
#' @return a list with outvec, vec_tbl, and compvec_tbl
#' @export
#' @examples
run_harmonizeR <- function(vec, compvec) {
  set.seed(1234)
  fenv <- new.env()
  start_i <- 1

  compvec_tbl <- data.frame(polyname = compvec,
                            matchname = NA)

  outvec <- rep(NA, length(vec))

  cat(paste("This is a interative tool to help you match geographical names",
            "against a user-defined standard.",
            "\n", sep="\n"))

  cat(paste("`vec` is the vector you seek to modify. `compvec` contains the",
            "strings that serve as the standard to which `vec` is compared",
            "\n", sep="\n"))

  cat(paste("This function will loop through `vec` and ask you if each item",
            "should be modified against a similarity vector generated from",
            "`comp vec`. This similarity vector is created using fuzzy string",
            "matching.",
            "\n", sep="\n"))

  cat(paste("You can choose to MODIFY the string shown from vec, replacing it",
            "with a string in the similarity vector. You can also QUIT this",
            "function at any time, or SAVE the current state of `vec` as an R",
            "object. You will be able to save your state as an .rda file.",
            "\n", sep="\n"))

  can_start <- readline("Would you like to begin (yes, no): ")

  while (!(tolower(can_start) %in% c("no", "yes"))) {
    can_start <- readline("Input not recognized. Please input a valid response (yes, no): ")
  }

  if (tolower(can_start) == "no") {
    return("Quitting interactive string matching tool...")
  }

  load_save <- readline("Would you like to load a saved state (yes, no): ")

  while (!(tolower(load_save) %in% c("no", "yes"))) {
    load_save <- readline("Input not recognized. Please input a valid response (yes, no): ")
  }

  if (tolower(load_save == "yes")) {
    path <- file.choose()

    while (!(tolower(tools::file_ext(path)) %in% c("rda", "rda", "rdata"))) {
      print("File is not the correct extension. Please upload your .rda (.RData) file now: ")
      path <- file.choose()
    }

    load(file = path, envir = fenv)
    start_i <- fenv$i
    vec <- fenv$vec
  }

  for (i in start_i:length(vec)) {

    if (!tolower(vec[i]) %in% tolower(compvec)) {

      print(paste0("Index ", i, " of ", length(vec), ": ", vec[i]))

      choice <- readline("What do you want to do? (modify, save, quit): ")

      while (!(tolower(choice) %in% c("modify", "save", "quit"))) {
        choice <- readline("Input not recognized. What do you want to do? (modify, save, quit): ")
      }

      if (tolower(choice) == "quit") {
        return("Quitting interactive string matching tool...")
      } else if (tolower(choice) == "modify") {

        # get a df of top three matches from compvec
        dist <- 1 - stringdist::stringdist(compvec, vec[i], method = "lv") / nchar(compvec)
        dist_df <- data.frame(geoname = compvec, dist = dist)

        top_dist <- dist_df |> dplyr::arrange(desc(dist)) |> head(10)

        # display df to user
        print(top_dist)

        mod_choice <- readline("Select string to replace current name (1 to 10). Select 0 to input your own string. Select 99 to assign an NA value to this string. ")

        while (!(tolower(mod_choice) %in% c(0:10, 99))) {
          mod_choice <- readline("Input not recognized. Select a valid numerical response: ")
        }

        # force numeric
        mod_choice <- as.numeric(mod_choice)

        if (mod_choice %in% c(1:10)) {
          outvec[i] <- tolower(top_dist$geoname[mod_choice])

          compindex <- which(tolower(outvec[i]) == tolower(compvec))
          compvec_tbl$matchname[compindex] <- tolower(outvec[i])
        } else if (mod_choice == 0) {
          new_string <- readline("Please enter your custom string: ")
          outvec[i] <- tolower(new_string)

          compindex <- which(tolower(outvec[i]) == tolower(compvec))
          compvec_tbl$matchname[compindex] <- tolower(outvec[i])
        } else if (mod_choice == 99) {
          outvec[i] <- NA
        }

        print("Current output vector state:")
        print(outvec)
        cat("\n")

      } else if (tolower(choice) == "save") {
        # save current state of vectors: the index, and vec
        assign("index", i)
        assign("saved_vec", outvec)

        saved_state <- list(index = i, saved_vec = outvec)

        cat("Saving state as saved_state.rda")
        save(i, outvec, file = paste0(rstudioapi::selectDirectory(), "/saved_state.rda"))
      }
    } else {
      compindex <- which(tolower(vec[i]) == tolower(compvec))

      compvec_tbl$matchname[compindex] <- tolower(vec[i])
      outvec[i] <- tolower(vec[i])

      print(compvec_tbl)

      print("Current output vector state:")
      print(outvec)
      cat("\n")
    }
  }

  crosswalk_id <- gen_crosswalkid_vector(length(vec))

  match_tbl <- data.frame(matchname = outvec,
                          crosswalk_id = crosswalk_id)

  vec_tbl <- data.frame(polyname = vec,
                        matchname = outvec,
                        crosswalk_id = crosswalk_id)

  compvec_tbl <- dplyr::left_join(compvec_tbl, match_tbl, by = "matchname")

  return(list(outvec = outvec,
              vec_tbl = vec_tbl,
              compvec_tbl = compvec_tbl))
}
