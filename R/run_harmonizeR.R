#' Run harmonizeR
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
  set.seed(1234) # for reproducibly

  fenv <- new.env()
  start_i <- 1

  compvec_tbl <- data.frame(polyname = compvec,
                            matchname = NA)

  outvec <- rep(NA, length(vec))
  orig_vec <- NULL

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

  can_start <- readline("Would you like to begin (yes [y], no [n]): ")

  can_start <- bin_resp_not_recog(can_start)

  if (tolower(can_start) == "n") {
    return("Quitting interactive string matching tool...")
  }

  # add or remove suffix ----------------------------------------------------
  alter_suffix_prefix <- readline("Would you like to remove a prefix or suffix to vec (yes [y], no [n]): ")

  alter_suffix_prefix <- bin_resp_not_recog(alter_suffix_prefix)

  # if (tolower(alter_suffix_prefix) == "n") {
  #   return("Quitting interactive string matching tool...")
  # }
  orig_vec <- vec

  while (tolower(alter_suffix_prefix == "y")) {

    regex_string <- readline("What is your replacement string? It can be literal or a regex (use '_' to mark spaces at the beginning or end of your input): ")
    regex_string <- sub('_', " ", regex_string)

    cat("\nThis is vec before the replacement: ")
    print(vec)

    vec <- gsub(regex_string, "", vec)

    cat("\nThis is vec after the replacement: ")
    print(vec)

    alter_suffix_prefix <- readline("Would you like to remove another prefix or suffix (yes [y], no [n])? ")

    alter_suffix_prefix <- bin_resp_not_recog(alter_suffix_prefix)
  }

  # load save ---------------------------------------------------------------
  if (is.null(orig_vec)) {
    load_save <- readline("Would you like to load a saved state (yes [y], no [n]): ")

    load_save <- bin_resp_not_recog(load_save)

    if (tolower(load_save == "y")) {
      path <- file.choose()

      while (!(tolower(tools::file_ext(path)) %in% c("rda", "rdata"))) {
        print("File is not the correct extension. Please upload your .rda (.RData) file now: ")
        path <- file.choose()
      }

      load(file = path, envir = fenv)

      start_i <- fenv$saved_state$index
      outvec <- fenv$saved_state$outvec
      compvec_tbl <- fenv$saved_state$compvec_tbl
    }
  }

  # start loop through vec --------------------------------------------------

  for (i in start_i:length(vec)) {

    if (!tolower(vec[i]) %in% tolower(compvec)) {

      print(paste0("Index ", i, " of ", length(vec), ": ", vec[i]))

      choice <- readline("What do you want to do? (modify [m], save [s], quit [q]): ")

      while (!(tolower(choice) %in% c("m", "s", "q"))) {
        choice <- readline("Input not recognized. What do you want to do? (modify [m], save [s], quit [q]): ")
      }

      if (tolower(choice) == "q") {
        return("Quitting interactive string matching tool...")
      } else if (tolower(choice) == "m") {

        # get a df of top three matches from compvec
        dist <- 1 - stringdist::stringdist(compvec, vec[i], method = "lv") / nchar(compvec)
        dist_df <- data.frame(geoname = compvec, dist = dist)

        top_dist <- dist_df |> dplyr::arrange(desc(dist)) |> head(10)

        # display df to user
        print(top_dist)

        mod_choice <- readline("Select string to match the current name (1 to 10). Select 0 to input your own string. Select 99 to assign an NA value to this string. ")

        while (!(tolower(mod_choice) %in% c(0:10, 99))) {
          mod_choice <- readline("Input not recognized. Select a valid numerical response: ")
        }

        # force numeric
        mod_choice <- as.numeric(mod_choice)

        if (mod_choice %in% c(1:10)) {
          match_name <- tolower(top_dist$geoname[mod_choice])

          to_swap <- readline(paste0("Would you like to keep the current spelling (select 0: ",
                                     tolower(vec[i]),
                                     ") or the comparison spelling (select 1: ",
                                     match_name,
                                     ")? "))

          while (!(tolower(to_swap) %in% c(0, 1))) {
            to_swap <- readline("Input not recognized. Select 0 to keep or 1 to swap to the matched namme: ")
          }

          # force numeric
          to_swap <- as.numeric(to_swap)

          if (to_swap == 1) {
            outvec[i] <- match_name

            compindex <- which(match_name == tolower(compvec))
            compvec_tbl$matchname[compindex] <- tolower(outvec[i])
          } else {
            outvec[i] <- tolower(vec[i])

            compindex <- which(match_name == tolower(compvec))
            compvec_tbl$matchname[compindex] <- tolower(outvec[i])
          }
        } else if (mod_choice == 0) {
          new_string <- tolower(readline("Please enter your custom string: "))
          outvec[i] <- new_string

          if (any(new_string == tolower(compvec))) {
            compindex <- which(new_string == tolower(compvec))
            compvec_tbl$matchname[compindex] <- new_string
          } else {
            compvec_tbl <- rbind(compvec_tbl,
                                 data.frame(polyname = NA, matchname = new_string))
          }
        } else if (mod_choice == 99) {
          outvec[i] <- NA
        }

        print("Current output vector state:")
        print(outvec)
        cat("\n")

      } else if (tolower(choice) == "s") {
        # save current state of vectors: the index, outvec, and compvec_tbl
        # NOTE that vec_tbl is generated at the end of the loop
        saved_state <- list(index = i,
                            outvec = outvec,
                            compvec_tbl = compvec_tbl)

        cat("Saving state as saved_state.rda")
        save(saved_state, file = paste0(rstudioapi::selectDirectory(), "/saved_state.rda"))
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

  crosswalk_id <- gen_crosswalkid_vector(outvec)

  match_tbl <- data.frame(matchname = outvec,
                          crosswalk_id = crosswalk_id)

  polyname <- if (!is.null(orig_vec)) orig_vec else vec # was a prefix/suffix removed?
  vec_tbl <- data.frame(polyname = polyname,
                        matchname = outvec,
                        crosswalk_id = crosswalk_id)

  compvec_tbl <- dplyr::left_join(compvec_tbl, match_tbl, by = "matchname")

  return(list(outvec = outvec,
              vec_tbl = vec_tbl,
              compvec_tbl = compvec_tbl))
}
