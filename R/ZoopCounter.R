#' Open the Zooplankton Counter
#'
#' Opens up the Zooplankton counting tool
#' @param data A vector of names
#' @param shortcuts A vector of characters representing the shortcut keys used to add to a species' count. For example, c("Z", "X", "C"...). Has a default setting if none is specified.
#' @return A Zooplankton Counter instance
#' @export
ZoopCounter <- function(data, shortcuts = NULL){

  # Pre-checks
  if (isFALSE(interactive())){
    stop("No human user was detected")
  }
  if (length(data) > 30){
    stop("Maximum length of data is 30")
  }
  if (typeof(data) != "character"){
    stop("Please specify a column or vector of species names when defining the data argument")
  }

  if (!requireNamespace("gWidgets2tcltk", quietly = TRUE)) {
    stop(
      "Package \"gWidgets2tcltk\" must be installed",
      call. = FALSE
    )
  }

  if (is.null(shortcuts)){
    keys <- c("Q","W", "E", "R", "T", "Y", "U", "I", "O", "P", "A", "S", "D", "F", "G", "H", "J", "K", "L", "Z", "X", "C", "V", "B", "N", "M", "1", "2", "3", "4", "5")
  } else if (typeof(shortcuts) == "character" & length(shortcuts) >= length(data)){
    keys <- shortcuts
  } else {
    stop("shortcuts must be a vector of character strings, greater in length than data")
  }

  # Creating Master Window and container
  w <- gWidgets2::gwindow("Zooplankton Counter", visible = FALSE, width = 1500, height = 700)
  gmaster <- gWidgets2::ggroup(cont=w, horizontal=FALSE, use.scrollwindow = TRUE)

  # Keyboard focus widget
  gKeyFocus <- gWidgets2::gbutton("Click to enable shortcuts", container = gmaster, handler = function(h,...){
    gWidgets2::svalue(gKeyFocus)<- "Shortcuts are enabled"
    gWidgets2::font(gKeyFocus) <- c(weight = "light", color = "green")
  })
  gKeyFocusHandle <- gWidgets2::addHandlerBlur(gKeyFocus, handler = function(h,...){
    gWidgets2::focus(gKeyFocus) <- TRUE # The Keyboard handler requires focus to accept inputs from the keyboard. This line ensures the shortcut enable button remains in focus even after other buttons are pressed.
  })
  gWidgets2::font(gKeyFocus) <- c(weight = "bold", color = "red")


  # Creating species containers

  ##### Note: The need for species specific containers is a result of gWidgets2 not playing nice with loops that create objects dynamically for each species. Only count for the final species could be tracked and edited. Therefore, this seemingly redundant code is required.


  ## Row 1 ##
  row1 <- gWidgets2::ggroup(container = gmaster, horizontal = TRUE)

  ### Species 1 ###
  if (isFALSE(is.na(data[1]))){
    gSpec1 <- gWidgets2::ggroup(cont=row1, horizontal=FALSE)
    Spec1Name <- gWidgets2::glabel(data[1], container=gSpec1, editable = FALSE)

    # Counter
    gCount1 <- gWidgets2::ggroup(cont=gSpec1, horizontal=TRUE)

    count1 <- 0
    Spec1sub <- gWidgets2::gbutton("-", cont=gCount1, handler = function(h,...){
      count1 <-  as.numeric(gWidgets2::svalue(lCount1)) - 1
      gWidgets2::svalue(lCount1) <- count1
    })
    lCount1 <- gWidgets2::glabel(count1, container=gCount1, editable = TRUE)
    Spec1add <- gWidgets2::gbutton("+", cont=gCount1, handler = function(h,...){
      count1 <-  as.numeric(gWidgets2::svalue(lCount1)) + 1
      gWidgets2::svalue(lCount1) <- count1
    })

    # Keyboard Shortcut
    gKeyShrt1 <- gWidgets2::ggroup(cont = gSpec1, horizontal = TRUE)
    lKeyShrt1 <- gWidgets2::glabel("Shortcut Key:", cont = gKeyShrt1)
    lKeyPress1 <- gWidgets2::glabel(keys[1], container = gKeyShrt1, editable = TRUE)
    gWidgets2::addHandlerKeystroke(gKeyFocus, handler = function(h,...){
      if (h$key == tolower(gWidgets2::svalue(lKeyPress1))){
        count1 <-  as.numeric(gWidgets2::svalue(lCount1)) + 1
        gWidgets2::svalue(lCount1) <- count1
      }
    })
    gWidgets2::gseparator(cont=row1, horizontal = FALSE)
  }
  ### End Species 1 ###

  ### Species 2 ###
  if (isFALSE(is.na(data[2]))){
    gSpec2 <- gWidgets2::ggroup(cont=row1, horizontal=FALSE)
    Spec2Name <- gWidgets2::glabel(data[2], container=gSpec2, editable = FALSE)

    # Counter
    gCount2 <- gWidgets2::ggroup(cont=gSpec2, horizontal=TRUE)

    count2 <- 0
    Spec2sub <- gWidgets2::gbutton("-", cont=gCount2, handler = function(h,...){
      count2 <-  as.numeric(gWidgets2::svalue(lCount2)) - 1
      gWidgets2::svalue(lCount2) <- count2
    })
    lCount2 <- gWidgets2::glabel(count2, container=gCount2, editable = TRUE)
    Spec2add <- gWidgets2::gbutton("+", cont=gCount2, handler = function(h,...){
      count2 <-  as.numeric(gWidgets2::svalue(lCount2)) + 1
      gWidgets2::svalue(lCount2) <- count2
    })

    # Keyboard Shortcut
    gKeyShrt2 <- gWidgets2::ggroup(cont = gSpec2, horizontal = TRUE)
    lKeyShrt2 <- gWidgets2::glabel("Shortcut Key:", cont = gKeyShrt2)
    lKeyPress2 <- gWidgets2::glabel(keys[2], container = gKeyShrt2, editable = TRUE)
    gWidgets2::addHandlerKeystroke(gKeyFocus, handler = function(h,...){
      if (h$key == tolower(gWidgets2::svalue(lKeyPress2))){
        count2 <-  as.numeric(gWidgets2::svalue(lCount2)) + 1
        gWidgets2::svalue(lCount2) <- count2
      }
    })
    gWidgets2::gseparator(cont=row1, horizontal = FALSE)
  }
  ### End Species 2 ###

  ### Species 3 ###
  if (isFALSE(is.na(data[3]))){
    gSpec3 <- gWidgets2::ggroup(cont=row1, horizontal=FALSE)
    Spec3Name <- gWidgets2::glabel(data[3], container=gSpec3, editable = FALSE)

    # Counter
    gCount3 <- gWidgets2::ggroup(cont=gSpec3, horizontal=TRUE)

    count3 <- 0
    Spec3sub <- gWidgets2::gbutton("-", cont=gCount3, handler = function(h,...){
      count3 <-  as.numeric(gWidgets2::svalue(lCount3)) - 1
      gWidgets2::svalue(lCount3) <- count3
    })
    lCount3 <- gWidgets2::glabel(count3, container=gCount3, editable = TRUE)
    Spec3add <- gWidgets2::gbutton("+", cont=gCount3, handler = function(h,...){
      count3 <-  as.numeric(gWidgets2::svalue(lCount3)) + 1
      gWidgets2::svalue(lCount3) <- count3
    })

    # Keyboard Shortcut
    gKeyShrt3 <- gWidgets2::ggroup(cont = gSpec3, horizontal = TRUE)
    lKeyShrt3 <- gWidgets2::glabel("Shortcut Key:", cont = gKeyShrt3)
    lKeyPress3 <- gWidgets2::glabel(keys[3], container = gKeyShrt3, editable = TRUE)
    gWidgets2::addHandlerKeystroke(gKeyFocus, handler = function(h,...){
      if (h$key == tolower(gWidgets2::svalue(lKeyPress3))){
        count3 <-  as.numeric(gWidgets2::svalue(lCount3)) + 1
        gWidgets2::svalue(lCount3) <- count3
      }
    })
    gWidgets2::gseparator(cont=row1, horizontal = FALSE)
  }
  ### End Species 3 ###

  ### Species 4 ###
  if (isFALSE(is.na(data[4]))){
    gSpec4 <- gWidgets2::ggroup(cont=row1, horizontal=FALSE)
    Spec4Name <- gWidgets2::glabel(data[4], container=gSpec4, editable = FALSE)

    # Counter
    gCount4 <- gWidgets2::ggroup(cont=gSpec4, horizontal=TRUE)

    count4 <- 0
    Spec4sub <- gWidgets2::gbutton("-", cont=gCount4, handler = function(h,...){
      count4 <-  as.numeric(gWidgets2::svalue(lCount4)) - 1
      gWidgets2::svalue(lCount4) <- count4
    })
    lCount4 <- gWidgets2::glabel(count4, container=gCount4, editable = TRUE)
    Spec4add <- gWidgets2::gbutton("+", cont=gCount4, handler = function(h,...){
      count4 <-  as.numeric(gWidgets2::svalue(lCount4)) + 1
      gWidgets2::svalue(lCount4) <- count4
    })

    # Keyboard Shortcut
    gKeyShrt4 <- gWidgets2::ggroup(cont = gSpec4, horizontal = TRUE)
    lKeyShrt4 <- gWidgets2::glabel("Shortcut Key:", cont = gKeyShrt4)
    lKeyPress4 <- gWidgets2::glabel(keys[4], container = gKeyShrt4, editable = TRUE)
    gWidgets2::addHandlerKeystroke(gKeyFocus, handler = function(h,...){
      if (h$key == tolower(gWidgets2::svalue(lKeyPress4))){
        count4 <-  as.numeric(gWidgets2::svalue(lCount4)) + 1
        gWidgets2::svalue(lCount4) <- count4
      }
    })
    gWidgets2::gseparator(cont=row1, horizontal = FALSE)
  }
  ### End Species 4 ###

  ### Species 5 ###
  if (isFALSE(is.na(data[5]))){
    gSpec5 <- gWidgets2::ggroup(cont=row1, horizontal=FALSE)
    Spec5Name <- gWidgets2::glabel(data[5], container=gSpec5, editable = FALSE)

    # Counter
    gCount5 <- gWidgets2::ggroup(cont=gSpec5, horizontal=TRUE)

    count5 <- 0
    Spec5sub <- gWidgets2::gbutton("-", cont=gCount5, handler = function(h,...){
      count5 <-  as.numeric(gWidgets2::svalue(lCount5)) - 1
      gWidgets2::svalue(lCount5) <- count5
    })
    lCount5 <- gWidgets2::glabel(count5, container=gCount5, editable = TRUE)
    Spec5add <- gWidgets2::gbutton("+", cont=gCount5, handler = function(h,...){
      count5 <-  as.numeric(gWidgets2::svalue(lCount5)) + 1
      gWidgets2::svalue(lCount5) <- count5
    })

    # Keyboard Shortcut
    gKeyShrt5 <- gWidgets2::ggroup(cont = gSpec5, horizontal = TRUE)
    lKeyShrt5 <- gWidgets2::glabel("Shortcut Key:", cont = gKeyShrt5)
    lKeyPress5 <- gWidgets2::glabel(keys[5], container = gKeyShrt5, editable = TRUE)
    gWidgets2::addHandlerKeystroke(gKeyFocus, handler = function(h,...){
      if (h$key == tolower(gWidgets2::svalue(lKeyPress5))){
        count5 <-  as.numeric(gWidgets2::svalue(lCount5)) + 1
        gWidgets2::svalue(lCount5) <- count5
      }
    })
    gWidgets2::gseparator(cont=row1, horizontal = FALSE)
  }
  ### End Species 5 ###

  ### Species 6 ###
  if (isFALSE(is.na(data[6]))){
    gSpec6 <- gWidgets2::ggroup(cont=row1, horizontal=FALSE)
    Spec6Name <- gWidgets2::glabel(data[6], container=gSpec6, editable = FALSE)

    # Counter
    gCount6 <- gWidgets2::ggroup(cont=gSpec6, horizontal=TRUE)

    count6 <- 0
    Spec6sub <- gWidgets2::gbutton("-", cont=gCount6, handler = function(h,...){
      count6 <-  as.numeric(gWidgets2::svalue(lCount6)) - 1
      gWidgets2::svalue(lCount6) <- count6
    })
    lCount6 <- gWidgets2::glabel(count6, container=gCount6, editable = TRUE)
    Spec6add <- gWidgets2::gbutton("+", cont=gCount6, handler = function(h,...){
      count6 <-  as.numeric(gWidgets2::svalue(lCount6)) + 1
      gWidgets2::svalue(lCount6) <- count6
    })

    # Keyboard Shortcut
    gKeyShrt6 <- gWidgets2::ggroup(cont = gSpec6, horizontal = TRUE)
    lKeyShrt6 <- gWidgets2::glabel("Shortcut Key:", cont = gKeyShrt6)
    lKeyPress6 <- gWidgets2::glabel(keys[6], container = gKeyShrt6, editable = TRUE)
    gWidgets2::addHandlerKeystroke(gKeyFocus, handler = function(h,...){
      if (h$key == tolower(gWidgets2::svalue(lKeyPress6))){
        count6 <-  as.numeric(gWidgets2::svalue(lCount6)) + 1
        gWidgets2::svalue(lCount6) <- count6
      }
    })
  }
  ### End Species 6 ###

  ## Row 2 ##
  if (length(data) >= 6){
    gWidgets2::gseparator(cont=gmaster)
  }
  row2 <- gWidgets2::ggroup(container = gmaster, horizontal = TRUE)

  ### Species 7 ###
  if (isFALSE(is.na(data[7]))){
    gSpec7 <- gWidgets2::ggroup(cont=row2, horizontal=FALSE)
    Spec7Name <- gWidgets2::glabel(data[7], container=gSpec7, editable = FALSE)

    # Counter
    gCount7 <- gWidgets2::ggroup(cont=gSpec7, horizontal=TRUE)

    count7 <- 0
    Spec7sub <- gWidgets2::gbutton("-", cont=gCount7, handler = function(h,...){
      count7 <-  as.numeric(gWidgets2::svalue(lCount7)) - 1
      gWidgets2::svalue(lCount7) <- count7
    })
    lCount7 <- gWidgets2::glabel(count7, container=gCount7, editable = TRUE)
    Spec7add <- gWidgets2::gbutton("+", cont=gCount7, handler = function(h,...){
      count7 <-  as.numeric(gWidgets2::svalue(lCount7)) + 1
      gWidgets2::svalue(lCount7) <- count7
    })

    # Keyboard Shortcut
    gKeyShrt7 <- gWidgets2::ggroup(cont = gSpec7, horizontal = TRUE)
    lKeyShrt7 <- gWidgets2::glabel("Shortcut Key:", cont = gKeyShrt7)
    lKeyPress7 <- gWidgets2::glabel(keys[7], container = gKeyShrt7, editable = TRUE)
    gWidgets2::addHandlerKeystroke(gKeyFocus, handler = function(h,...){
      if (h$key == tolower(gWidgets2::svalue(lKeyPress7))){
        count7 <-  as.numeric(gWidgets2::svalue(lCount7)) + 1
        gWidgets2::svalue(lCount7) <- count7
      }
    })
    gWidgets2::gseparator(cont=row2, horizontal = FALSE)
  }
  ### End Species 7 ###

  ### Species 8 ###
  if (isFALSE(is.na(data[8]))){
    gSpec8 <- gWidgets2::ggroup(cont=row2, horizontal=FALSE)
    Spec8Name <- gWidgets2::glabel(data[8], container=gSpec8, editable = FALSE)

    # Counter
    gCount8 <- gWidgets2::ggroup(cont=gSpec8, horizontal=TRUE)

    count8 <- 0
    Spec8sub <- gWidgets2::gbutton("-", cont=gCount8, handler = function(h,...){
      count8 <-  as.numeric(gWidgets2::svalue(lCount8)) - 1
      gWidgets2::svalue(lCount8) <- count8
    })
    lCount8 <- gWidgets2::glabel(count8, container=gCount8, editable = TRUE)
    Spec8add <- gWidgets2::gbutton("+", cont=gCount8, handler = function(h,...){
      count8 <-  as.numeric(gWidgets2::svalue(lCount8)) + 1
      gWidgets2::svalue(lCount8) <- count8
    })

    # Keyboard Shortcut
    gKeyShrt8 <- gWidgets2::ggroup(cont = gSpec8, horizontal = TRUE)
    lKeyShrt8 <- gWidgets2::glabel("Shortcut Key:", cont = gKeyShrt8)
    lKeyPress8 <- gWidgets2::glabel(keys[8], container = gKeyShrt8, editable = TRUE)
    gWidgets2::addHandlerKeystroke(gKeyFocus, handler = function(h,...){
      if (h$key == tolower(gWidgets2::svalue(lKeyPress8))){
        count8 <-  as.numeric(gWidgets2::svalue(lCount8)) + 1
        gWidgets2::svalue(lCount8) <- count8
      }
    })
    gWidgets2::gseparator(cont=row2, horizontal = FALSE)
  }
  ### End Species 8 ###

  ### Species 9 ###
  if (isFALSE(is.na(data[9]))){
    gSpec9 <- gWidgets2::ggroup(cont=row2, horizontal=FALSE)
    Spec9Name <- gWidgets2::glabel(data[9], container=gSpec9, editable = FALSE)

    # Counter
    gCount9 <- gWidgets2::ggroup(cont=gSpec9, horizontal=TRUE)

    count9 <- 0
    Spec9sub <- gWidgets2::gbutton("-", cont=gCount9, handler = function(h,...){
      count9 <-  as.numeric(gWidgets2::svalue(lCount9)) - 1
      gWidgets2::svalue(lCount9) <- count9
    })
    lCount9 <- gWidgets2::glabel(count9, container=gCount9, editable = TRUE)
    Spec9add <- gWidgets2::gbutton("+", cont=gCount9, handler = function(h,...){
      count9 <-  as.numeric(gWidgets2::svalue(lCount9)) + 1
      gWidgets2::svalue(lCount9) <- count9
    })

    # Keyboard Shortcut
    gKeyShrt9 <- gWidgets2::ggroup(cont = gSpec9, horizontal = TRUE)
    lKeyShrt9 <- gWidgets2::glabel("Shortcut Key:", cont = gKeyShrt9)
    lKeyPress9 <- gWidgets2::glabel(keys[9], container = gKeyShrt9, editable = TRUE)
    gWidgets2::addHandlerKeystroke(gKeyFocus, handler = function(h,...){
      if (h$key == tolower(gWidgets2::svalue(lKeyPress9))){
        count9 <-  as.numeric(gWidgets2::svalue(lCount9)) + 1
        gWidgets2::svalue(lCount9) <- count9
      }
    })
    gWidgets2::gseparator(cont=row2, horizontal = FALSE)
  }
  ### End Species 9 ###

  ### Species 10 ###
  if (isFALSE(is.na(data[10]))){
    gSpec10 <- gWidgets2::ggroup(cont=row2, horizontal=FALSE)
    Spec10Name <- gWidgets2::glabel(data[10], container=gSpec10, editable = FALSE)

    # Counter
    gCount10 <- gWidgets2::ggroup(cont=gSpec10, horizontal=TRUE)

    count10 <- 0
    Spec10sub <- gWidgets2::gbutton("-", cont=gCount10, handler = function(h,...){
      count10 <-  as.numeric(gWidgets2::svalue(lCount10)) - 1
      gWidgets2::svalue(lCount10) <- count10
    })
    lCount10 <- gWidgets2::glabel(count10, container=gCount10, editable = TRUE)
    Spec10add <- gWidgets2::gbutton("+", cont=gCount10, handler = function(h,...){
      count10 <-  as.numeric(gWidgets2::svalue(lCount10)) + 1
      gWidgets2::svalue(lCount10) <- count10
    })

    # Keyboard Shortcut
    gKeyShrt10 <- gWidgets2::ggroup(cont = gSpec10, horizontal = TRUE)
    lKeyShrt10 <- gWidgets2::glabel("Shortcut Key:", cont = gKeyShrt10)
    lKeyPress10 <- gWidgets2::glabel(keys[10], container = gKeyShrt10, editable = TRUE)
    gWidgets2::addHandlerKeystroke(gKeyFocus, handler = function(h,...){
      if (h$key == tolower(gWidgets2::svalue(lKeyPress10))){
        count10 <-  as.numeric(gWidgets2::svalue(lCount10)) + 1
        gWidgets2::svalue(lCount10) <- count10
      }
    })
    gWidgets2::gseparator(cont=row2, horizontal = FALSE)
  }
  ### End Species 10 ###

  ### Species 11 ###
  if (isFALSE(is.na(data[11]))){
    gSpec11 <- gWidgets2::ggroup(cont=row2, horizontal=FALSE)
    Spec11Name <- gWidgets2::glabel(data[11], container=gSpec11, editable = FALSE)

    # Counter
    gCount11 <- gWidgets2::ggroup(cont=gSpec11, horizontal=TRUE)

    count11 <- 0
    Spec11sub <- gWidgets2::gbutton("-", cont=gCount11, handler = function(h,...){
      count11 <-  as.numeric(gWidgets2::svalue(lCount11)) - 1
      gWidgets2::svalue(lCount11) <- count11
    })
    lCount11 <- gWidgets2::glabel(count11, container=gCount11, editable = TRUE)
    Spec11add <- gWidgets2::gbutton("+", cont=gCount11, handler = function(h,...){
      count11 <-  as.numeric(gWidgets2::svalue(lCount11)) + 1
      gWidgets2::svalue(lCount11) <- count11
    })

    # Keyboard Shortcut
    gKeyShrt11 <- gWidgets2::ggroup(cont = gSpec11, horizontal = TRUE)
    lKeyShrt11 <- gWidgets2::glabel("Shortcut Key:", cont = gKeyShrt11)
    lKeyPress11 <- gWidgets2::glabel(keys[11], container = gKeyShrt11, editable = TRUE)
    gWidgets2::addHandlerKeystroke(gKeyFocus, handler = function(h,...){
      if (h$key == tolower(gWidgets2::svalue(lKeyPress11))){
        count11 <-  as.numeric(gWidgets2::svalue(lCount11)) + 1
        gWidgets2::svalue(lCount11) <- count11
      }
    })
    gWidgets2::gseparator(cont=row2, horizontal = FALSE)
  }
  ### End Species 11 ###

  ### Species 12 ###
  if (isFALSE(is.na(data[12]))){
    gSpec12 <- gWidgets2::ggroup(cont=row2, horizontal=FALSE)
    Spec12Name <- gWidgets2::glabel(data[12], container=gSpec12, editable = FALSE)

    # Counter
    gCount12 <- gWidgets2::ggroup(cont=gSpec12, horizontal=TRUE)

    count12 <- 0
    Spec12sub <- gWidgets2::gbutton("-", cont=gCount12, handler = function(h,...){
      count12 <-  as.numeric(gWidgets2::svalue(lCount12)) - 1
      gWidgets2::svalue(lCount12) <- count12
    })
    lCount12 <- gWidgets2::glabel(count12, container=gCount12, editable = TRUE)
    Spec12add <- gWidgets2::gbutton("+", cont=gCount12, handler = function(h,...){
      count12 <-  as.numeric(gWidgets2::svalue(lCount12)) + 1
      gWidgets2::svalue(lCount12) <- count12
    })

    # Keyboard Shortcut
    gKeyShrt12 <- gWidgets2::ggroup(cont = gSpec12, horizontal = TRUE)
    lKeyShrt12 <- gWidgets2::glabel("Shortcut Key:", cont = gKeyShrt12)
    lKeyPress12 <- gWidgets2::glabel(keys[12], container = gKeyShrt12, editable = TRUE)
    gWidgets2::addHandlerKeystroke(gKeyFocus, handler = function(h,...){
      if (h$key == tolower(gWidgets2::svalue(lKeyPress12))){
        count12 <-  as.numeric(gWidgets2::svalue(lCount12)) + 1
        gWidgets2::svalue(lCount12) <- count12
      }
    })
  }
  ### End Species 12 ###

  ## Row 3 ##
  if (length(data) >= 12){
    gWidgets2::gseparator(cont=gmaster)
  }
  row3 <- gWidgets2::ggroup(container = gmaster, horizontal = TRUE)

  ### Species 13 ###
  if (isFALSE(is.na(data[13]))){
    gSpec13 <- gWidgets2::ggroup(cont=row3, horizontal=FALSE)
    Spec13Name <- gWidgets2::glabel(data[13], container=gSpec13, editable = FALSE)

    # Counter
    gCount13 <- gWidgets2::ggroup(cont=gSpec13, horizontal=TRUE)

    count13 <- 0
    Spec13sub <- gWidgets2::gbutton("-", cont=gCount13, handler = function(h,...){
      count13 <-  as.numeric(gWidgets2::svalue(lCount13)) - 1
      gWidgets2::svalue(lCount13) <- count13
    })
    lCount13 <- gWidgets2::glabel(count13, container=gCount13, editable = TRUE)
    Spec13add <- gWidgets2::gbutton("+", cont=gCount13, handler = function(h,...){
      count13 <-  as.numeric(gWidgets2::svalue(lCount13)) + 1
      gWidgets2::svalue(lCount13) <- count13
    })

    # Keyboard Shortcut
    gKeyShrt13 <- gWidgets2::ggroup(cont = gSpec13, horizontal = TRUE)
    lKeyShrt13 <- gWidgets2::glabel("Shortcut Key:", cont = gKeyShrt13)
    lKeyPress13 <- gWidgets2::glabel(keys[13], container = gKeyShrt13, editable = TRUE)
    gWidgets2::addHandlerKeystroke(gKeyFocus, handler = function(h,...){
      if (h$key == tolower(gWidgets2::svalue(lKeyPress13))){
        count13 <-  as.numeric(gWidgets2::svalue(lCount13)) + 1
        gWidgets2::svalue(lCount13) <- count13
      }
    })
    gWidgets2::gseparator(cont=row3, horizontal = FALSE)
  }
  ### End Species 13 ###

  ### Species 14 ###
  if (isFALSE(is.na(data[14]))){
    gSpec14 <- gWidgets2::ggroup(cont=row3, horizontal=FALSE)
    Spec14Name <- gWidgets2::glabel(data[14], container=gSpec14, editable = FALSE)

    # Counter
    gCount14 <- gWidgets2::ggroup(cont=gSpec14, horizontal=TRUE)

    count14 <- 0
    Spec14sub <- gWidgets2::gbutton("-", cont=gCount14, handler = function(h,...){
      count14 <-  as.numeric(gWidgets2::svalue(lCount14)) - 1
      gWidgets2::svalue(lCount14) <- count14
    })
    lCount14 <- gWidgets2::glabel(count14, container=gCount14, editable = TRUE)
    Spec14add <- gWidgets2::gbutton("+", cont=gCount14, handler = function(h,...){
      count14 <-  as.numeric(gWidgets2::svalue(lCount14)) + 1
      gWidgets2::svalue(lCount14) <- count14
    })

    # Keyboard Shortcut
    gKeyShrt14 <- gWidgets2::ggroup(cont = gSpec14, horizontal = TRUE)
    lKeyShrt14 <- gWidgets2::glabel("Shortcut Key:", cont = gKeyShrt14)
    lKeyPress14 <- gWidgets2::glabel(keys[14], container = gKeyShrt14, editable = TRUE)
    gWidgets2::addHandlerKeystroke(gKeyFocus, handler = function(h,...){
      if (h$key == tolower(gWidgets2::svalue(lKeyPress14))){
        count14 <-  as.numeric(gWidgets2::svalue(lCount14)) + 1
        gWidgets2::svalue(lCount14) <- count14
      }
    })
    gWidgets2::gseparator(cont=row3, horizontal = FALSE)
  }
  ### End Species 14 ###

  ### Species 15 ###
  if (isFALSE(is.na(data[15]))){
    gSpec15 <- gWidgets2::ggroup(cont=row3, horizontal=FALSE)
    Spec15Name <- gWidgets2::glabel(data[15], container=gSpec15, editable = FALSE)

    # Counter
    gCount15 <- gWidgets2::ggroup(cont=gSpec15, horizontal=TRUE)

    count15 <- 0
    Spec15sub <- gWidgets2::gbutton("-", cont=gCount15, handler = function(h,...){
      count15 <-  as.numeric(gWidgets2::svalue(lCount15)) - 1
      gWidgets2::svalue(lCount15) <- count15
    })
    lCount15 <- gWidgets2::glabel(count15, container=gCount15, editable = TRUE)
    Spec15add <- gWidgets2::gbutton("+", cont=gCount15, handler = function(h,...){
      count15 <-  as.numeric(gWidgets2::svalue(lCount15)) + 1
      gWidgets2::svalue(lCount15) <- count15
    })

    # Keyboard Shortcut
    gKeyShrt15 <- gWidgets2::ggroup(cont = gSpec15, horizontal = TRUE)
    lKeyShrt15 <- gWidgets2::glabel("Shortcut Key:", cont = gKeyShrt15)
    lKeyPress15 <- gWidgets2::glabel(keys[15], container = gKeyShrt15, editable = TRUE)
    gWidgets2::addHandlerKeystroke(gKeyFocus, handler = function(h,...){
      if (h$key == tolower(gWidgets2::svalue(lKeyPress15))){
        count15 <-  as.numeric(gWidgets2::svalue(lCount15)) + 1
        gWidgets2::svalue(lCount15) <- count15
      }
    })
    gWidgets2::gseparator(cont=row3, horizontal = FALSE)
  }
  ### End Species 15 ###

  ### Species 16 ###
  if (isFALSE(is.na(data[16]))){
    gSpec16 <- gWidgets2::ggroup(cont=row3, horizontal=FALSE)
    Spec16Name <- gWidgets2::glabel(data[16], container=gSpec16, editable = FALSE)

    # Counter
    gCount16 <- gWidgets2::ggroup(cont=gSpec16, horizontal=TRUE)

    count16 <- 0
    Spec16sub <- gWidgets2::gbutton("-", cont=gCount16, handler = function(h,...){
      count16 <-  as.numeric(gWidgets2::svalue(lCount16)) - 1
      gWidgets2::svalue(lCount16) <- count16
    })
    lCount16 <- gWidgets2::glabel(count16, container=gCount16, editable = TRUE)
    Spec16add <- gWidgets2::gbutton("+", cont=gCount16, handler = function(h,...){
      count16 <-  as.numeric(gWidgets2::svalue(lCount16)) + 1
      gWidgets2::svalue(lCount16) <- count16
    })

    # Keyboard Shortcut
    gKeyShrt16 <- gWidgets2::ggroup(cont = gSpec16, horizontal = TRUE)
    lKeyShrt16 <- gWidgets2::glabel("Shortcut Key:", cont = gKeyShrt16)
    lKeyPress16 <- gWidgets2::glabel(keys[16], container = gKeyShrt16, editable = TRUE)
    gWidgets2::addHandlerKeystroke(gKeyFocus, handler = function(h,...){
      if (h$key == tolower(gWidgets2::svalue(lKeyPress16))){
        count16 <-  as.numeric(gWidgets2::svalue(lCount16)) + 1
        gWidgets2::svalue(lCount16) <- count16
      }
    })
    gWidgets2::gseparator(cont=row3, horizontal = FALSE)
  }
  ### End Species 16 ###

  ### Species 17 ###
  if (isFALSE(is.na(data[17]))){
    gSpec17 <- gWidgets2::ggroup(cont=row3, horizontal=FALSE)
    Spec17Name <- gWidgets2::glabel(data[17], container=gSpec17, editable = FALSE)

    # Counter
    gCount17 <- gWidgets2::ggroup(cont=gSpec17, horizontal=TRUE)

    count17 <- 0
    Spec17sub <- gWidgets2::gbutton("-", cont=gCount17, handler = function(h,...){
      count17 <-  as.numeric(gWidgets2::svalue(lCount17)) - 1
      gWidgets2::svalue(lCount17) <- count17
    })
    lCount17 <- gWidgets2::glabel(count17, container=gCount17, editable = TRUE)
    Spec17add <- gWidgets2::gbutton("+", cont=gCount17, handler = function(h,...){
      count17 <-  as.numeric(gWidgets2::svalue(lCount17)) + 1
      gWidgets2::svalue(lCount17) <- count17
    })

    # Keyboard Shortcut
    gKeyShrt17 <- gWidgets2::ggroup(cont = gSpec17, horizontal = TRUE)
    lKeyShrt17 <- gWidgets2::glabel("Shortcut Key:", cont = gKeyShrt17)
    lKeyPress17 <- gWidgets2::glabel(keys[17], container = gKeyShrt17, editable = TRUE)
    gWidgets2::addHandlerKeystroke(gKeyFocus, handler = function(h,...){
      if (h$key == tolower(gWidgets2::svalue(lKeyPress17))){
        count17 <-  as.numeric(gWidgets2::svalue(lCount17)) + 1
        gWidgets2::svalue(lCount17) <- count17
      }
    })
    gWidgets2::gseparator(cont=row3, horizontal = FALSE)
  }
  ### End Species 17 ###

  ### Species 18 ###
  if (isFALSE(is.na(data[18]))){
    gSpec18 <- gWidgets2::ggroup(cont=row3, horizontal=FALSE)
    Spec18Name <- gWidgets2::glabel(data[18], container=gSpec18, editable = FALSE)

    # Counter
    gCount18 <- gWidgets2::ggroup(cont=gSpec18, horizontal=TRUE)

    count18 <- 0
    Spec18sub <- gWidgets2::gbutton("-", cont=gCount18, handler = function(h,...){
      count18 <-  as.numeric(gWidgets2::svalue(lCount18)) - 1
      gWidgets2::svalue(lCount18) <- count18
    })
    lCount18 <- gWidgets2::glabel(count18, container=gCount18, editable = TRUE)
    Spec18add <- gWidgets2::gbutton("+", cont=gCount18, handler = function(h,...){
      count18 <-  as.numeric(gWidgets2::svalue(lCount18)) + 1
      gWidgets2::svalue(lCount18) <- count18
    })

    # Keyboard Shortcut
    gKeyShrt18 <- gWidgets2::ggroup(cont = gSpec18, horizontal = TRUE)
    lKeyShrt18 <- gWidgets2::glabel("Shortcut Key:", cont = gKeyShrt18)
    lKeyPress18 <- gWidgets2::glabel(keys[18], container = gKeyShrt18, editable = TRUE)
    gWidgets2::addHandlerKeystroke(gKeyFocus, handler = function(h,...){
      if (h$key == tolower(gWidgets2::svalue(lKeyPress18))){
        count18 <-  as.numeric(gWidgets2::svalue(lCount18)) + 1
        gWidgets2::svalue(lCount18) <- count18
      }
    })
  }
  ### End Species 18 ###

  ## Row 4 ##
  if (length(data) >= 18){
    gWidgets2::gseparator(cont=gmaster)
  }
  row4 <- gWidgets2::ggroup(container = gmaster, horizontal = TRUE)

  ### Species 19 ###
  if (isFALSE(is.na(data[19]))){
    gSpec19 <- gWidgets2::ggroup(cont=row4, horizontal=FALSE)
    Spec19Name <- gWidgets2::glabel(data[19], container=gSpec19, editable = FALSE)

    # Counter
    gCount19 <- gWidgets2::ggroup(cont=gSpec19, horizontal=TRUE)

    count19 <- 0
    Spec19sub <- gWidgets2::gbutton("-", cont=gCount19, handler = function(h,...){
      count19 <-  as.numeric(gWidgets2::svalue(lCount19)) - 1
      gWidgets2::svalue(lCount19) <- count19
    })
    lCount19 <- gWidgets2::glabel(count19, container=gCount19, editable = TRUE)
    Spec19add <- gWidgets2::gbutton("+", cont=gCount19, handler = function(h,...){
      count19 <-  as.numeric(gWidgets2::svalue(lCount19)) + 1
      gWidgets2::svalue(lCount19) <- count19
    })

    # Keyboard Shortcut
    gKeyShrt19 <- gWidgets2::ggroup(cont = gSpec19, horizontal = TRUE)
    lKeyShrt19 <- gWidgets2::glabel("Shortcut Key:", cont = gKeyShrt19)
    lKeyPress19 <- gWidgets2::glabel(keys[19], container = gKeyShrt19, editable = TRUE)
    gWidgets2::addHandlerKeystroke(gKeyFocus, handler = function(h,...){
      if (h$key == tolower(gWidgets2::svalue(lKeyPress19))){
        count19 <-  as.numeric(gWidgets2::svalue(lCount19)) + 1
        gWidgets2::svalue(lCount19) <- count19
      }
    })
    gWidgets2::gseparator(cont=row4, horizontal = FALSE)
  }
  ### End Species 19 ###

  ### Species 20 ###
  if (isFALSE(is.na(data[20]))){
    gSpec20 <- gWidgets2::ggroup(cont=row4, horizontal=FALSE)
    Spec20Name <- gWidgets2::glabel(data[20], container=gSpec20, editable = FALSE)

    # Counter
    gCount20 <- gWidgets2::ggroup(cont=gSpec20, horizontal=TRUE)

    count20 <- 0
    Spec20sub <- gWidgets2::gbutton("-", cont=gCount20, handler = function(h,...){
      count20 <-  as.numeric(gWidgets2::svalue(lCount20)) - 1
      gWidgets2::svalue(lCount20) <- count20
    })
    lCount20 <- gWidgets2::glabel(count20, container=gCount20, editable = TRUE)
    Spec20add <- gWidgets2::gbutton("+", cont=gCount20, handler = function(h,...){
      count20 <-  as.numeric(gWidgets2::svalue(lCount20)) + 1
      gWidgets2::svalue(lCount20) <- count20
    })

    # Keyboard Shortcut
    gKeyShrt20 <- gWidgets2::ggroup(cont = gSpec20, horizontal = TRUE)
    lKeyShrt20 <- gWidgets2::glabel("Shortcut Key:", cont = gKeyShrt20)
    lKeyPress20 <- gWidgets2::glabel(keys[20], container = gKeyShrt20, editable = TRUE)
    gWidgets2::addHandlerKeystroke(gKeyFocus, handler = function(h,...){
      if (h$key == tolower(gWidgets2::svalue(lKeyPress20))){
        count20 <-  as.numeric(gWidgets2::svalue(lCount20)) + 1
        gWidgets2::svalue(lCount20) <- count20
      }
    })
    gWidgets2::gseparator(cont=row4, horizontal = FALSE)
  }
  ### End Species 20 ###

  ### Species 21 ###
  if (isFALSE(is.na(data[21]))){
    gSpec21 <- gWidgets2::ggroup(cont=row4, horizontal=FALSE)
    Spec21Name <- gWidgets2::glabel(data[21], container=gSpec21, editable = FALSE)

    # Counter
    gCount21 <- gWidgets2::ggroup(cont=gSpec21, horizontal=TRUE)

    count21 <- 0
    Spec21sub <- gWidgets2::gbutton("-", cont=gCount21, handler = function(h,...){
      count21 <-  as.numeric(gWidgets2::svalue(lCount21)) - 1
      gWidgets2::svalue(lCount21) <- count21
    })
    lCount21 <- gWidgets2::glabel(count21, container=gCount21, editable = TRUE)
    Spec21add <- gWidgets2::gbutton("+", cont=gCount21, handler = function(h,...){
      count21 <-  as.numeric(gWidgets2::svalue(lCount21)) + 1
      gWidgets2::svalue(lCount21) <- count21
    })

    # Keyboard Shortcut
    gKeyShrt21 <- gWidgets2::ggroup(cont = gSpec21, horizontal = TRUE)
    lKeyShrt21 <- gWidgets2::glabel("Shortcut Key:", cont = gKeyShrt21)
    lKeyPress21 <- gWidgets2::glabel(keys[21], container = gKeyShrt21, editable = TRUE)
    gWidgets2::addHandlerKeystroke(gKeyFocus, handler = function(h,...){
      if (h$key == tolower(gWidgets2::svalue(lKeyPress21))){
        count21 <-  as.numeric(gWidgets2::svalue(lCount21)) + 1
        gWidgets2::svalue(lCount21) <- count21
      }
    })
    gWidgets2::gseparator(cont=row4, horizontal = FALSE)
  }
  ### End Species 21 ###

  ### Species 22 ###
  if (isFALSE(is.na(data[22]))){
    gSpec22 <- gWidgets2::ggroup(cont=row4, horizontal=FALSE)
    Spec22Name <- gWidgets2::glabel(data[22], container=gSpec22, editable = FALSE)

    # Counter
    gCount22 <- gWidgets2::ggroup(cont=gSpec22, horizontal=TRUE)

    count22 <- 0
    Spec22sub <- gWidgets2::gbutton("-", cont=gCount22, handler = function(h,...){
      count22 <-  as.numeric(gWidgets2::svalue(lCount22)) - 1
      gWidgets2::svalue(lCount22) <- count22
    })
    lCount22 <- gWidgets2::glabel(count22, container=gCount22, editable = TRUE)
    Spec22add <- gWidgets2::gbutton("+", cont=gCount22, handler = function(h,...){
      count22 <-  as.numeric(gWidgets2::svalue(lCount22)) + 1
      gWidgets2::svalue(lCount22) <- count22
    })

    # Keyboard Shortcut
    gKeyShrt22 <- gWidgets2::ggroup(cont = gSpec22, horizontal = TRUE)
    lKeyShrt22 <- gWidgets2::glabel("Shortcut Key:", cont = gKeyShrt22)
    lKeyPress22 <- gWidgets2::glabel(keys[22], container = gKeyShrt22, editable = TRUE)
    gWidgets2::addHandlerKeystroke(gKeyFocus, handler = function(h,...){
      if (h$key == tolower(gWidgets2::svalue(lKeyPress22))){
        count22 <-  as.numeric(gWidgets2::svalue(lCount22)) + 1
        gWidgets2::svalue(lCount22) <- count22
      }
    })
    gWidgets2::gseparator(cont=row4, horizontal = FALSE)
  }
  ### End Species 22 ###

  ### Species 23 ###
  if (isFALSE(is.na(data[23]))){
    gSpec23 <- gWidgets2::ggroup(cont=row4, horizontal=FALSE)
    Spec23Name <- gWidgets2::glabel(data[23], container=gSpec23, editable = FALSE)

    # Counter
    gCount23 <- gWidgets2::ggroup(cont=gSpec23, horizontal=TRUE)

    count23 <- 0
    Spec23sub <- gWidgets2::gbutton("-", cont=gCount23, handler = function(h,...){
      count23 <-  as.numeric(gWidgets2::svalue(lCount23)) - 1
      gWidgets2::svalue(lCount23) <- count23
    })
    lCount23 <- gWidgets2::glabel(count23, container=gCount23, editable = TRUE)
    Spec23add <- gWidgets2::gbutton("+", cont=gCount23, handler = function(h,...){
      count23 <-  as.numeric(gWidgets2::svalue(lCount23)) + 1
      gWidgets2::svalue(lCount23) <- count23
    })

    # Keyboard Shortcut
    gKeyShrt23 <- gWidgets2::ggroup(cont = gSpec23, horizontal = TRUE)
    lKeyShrt23 <- gWidgets2::glabel("Shortcut Key:", cont = gKeyShrt23)
    lKeyPress23 <- gWidgets2::glabel(keys[23], container = gKeyShrt23, editable = TRUE)
    gWidgets2::addHandlerKeystroke(gKeyFocus, handler = function(h,...){
      if (h$key == tolower(gWidgets2::svalue(lKeyPress23))){
        count23 <-  as.numeric(gWidgets2::svalue(lCount23)) + 1
        gWidgets2::svalue(lCount23) <- count23
      }
    })
    gWidgets2::gseparator(cont=row4, horizontal = FALSE)
  }
  ### End Species 23 ###

  ### Species 24 ###
  if (isFALSE(is.na(data[24]))){
    gSpec24 <- gWidgets2::ggroup(cont=row4, horizontal=FALSE)
    Spec24Name <- gWidgets2::glabel(data[24], container=gSpec24, editable = FALSE)

    # Counter
    gCount24 <- gWidgets2::ggroup(cont=gSpec24, horizontal=TRUE)

    count24 <- 0
    Spec24sub <- gWidgets2::gbutton("-", cont=gCount24, handler = function(h,...){
      count24 <-  as.numeric(gWidgets2::svalue(lCount24)) - 1
      gWidgets2::svalue(lCount24) <- count24
    })
    lCount24 <- gWidgets2::glabel(count24, container=gCount24, editable = TRUE)
    Spec24add <- gWidgets2::gbutton("+", cont=gCount24, handler = function(h,...){
      count24 <-  as.numeric(gWidgets2::svalue(lCount24)) + 1
      gWidgets2::svalue(lCount24) <- count24
    })

    # Keyboard Shortcut
    gKeyShrt24 <- gWidgets2::ggroup(cont = gSpec24, horizontal = TRUE)
    lKeyShrt24 <- gWidgets2::glabel("Shortcut Key:", cont = gKeyShrt24)
    lKeyPress24 <- gWidgets2::glabel(keys[24], container = gKeyShrt24, editable = TRUE)
    gWidgets2::addHandlerKeystroke(gKeyFocus, handler = function(h,...){
      if (h$key == tolower(gWidgets2::svalue(lKeyPress24))){
        count24 <-  as.numeric(gWidgets2::svalue(lCount24)) + 1
        gWidgets2::svalue(lCount24) <- count24
      }
    })
  }
  ### End Species 24 ###

  ## Row 5 ##
  if (length(data) >= 24){
    gWidgets2::gseparator(cont=gmaster)
  }
  row5 <- gWidgets2::ggroup(container = gmaster, horizontal = TRUE)

  ### Species 25 ###
  if (isFALSE(is.na(data[25]))){
    gSpec25 <- gWidgets2::ggroup(cont=row5, horizontal=FALSE)
    Spec25Name <- gWidgets2::glabel(data[25], container=gSpec25, editable = FALSE)

    # Counter
    gCount25 <- gWidgets2::ggroup(cont=gSpec25, horizontal=TRUE)

    count25 <- 0
    Spec25sub <- gWidgets2::gbutton("-", cont=gCount25, handler = function(h,...){
      count25 <-  as.numeric(gWidgets2::svalue(lCount25)) - 1
      gWidgets2::svalue(lCount25) <- count25
    })
    lCount25 <- gWidgets2::glabel(count25, container=gCount25, editable = TRUE)
    Spec25add <- gWidgets2::gbutton("+", cont=gCount25, handler = function(h,...){
      count25 <-  as.numeric(gWidgets2::svalue(lCount25)) + 1
      gWidgets2::svalue(lCount25) <- count25
    })

    # Keyboard Shortcut
    gKeyShrt25 <- gWidgets2::ggroup(cont = gSpec25, horizontal = TRUE)
    lKeyShrt25 <- gWidgets2::glabel("Shortcut Key:", cont = gKeyShrt25)
    lKeyPress25 <- gWidgets2::glabel(keys[25], container = gKeyShrt25, editable = TRUE)
    gWidgets2::addHandlerKeystroke(gKeyFocus, handler = function(h,...){
      if (h$key == tolower(gWidgets2::svalue(lKeyPress25))){
        count25 <-  as.numeric(gWidgets2::svalue(lCount25)) + 1
        gWidgets2::svalue(lCount25) <- count25
      }
    })
    gWidgets2::gseparator(cont=row5, horizontal = FALSE)
  }
  ### End Species 25 ###

  ### Species 26 ###
  if (isFALSE(is.na(data[26]))){
    gSpec26 <- gWidgets2::ggroup(cont=row5, horizontal=FALSE)
    Spec26Name <- gWidgets2::glabel(data[26], container=gSpec26, editable = FALSE)

    # Counter
    gCount26 <- gWidgets2::ggroup(cont=gSpec26, horizontal=TRUE)

    count26 <- 0
    Spec26sub <- gWidgets2::gbutton("-", cont=gCount26, handler = function(h,...){
      count26 <-  as.numeric(gWidgets2::svalue(lCount26)) - 1
      gWidgets2::svalue(lCount26) <- count26
    })
    lCount26 <- gWidgets2::glabel(count26, container=gCount26, editable = TRUE)
    Spec26add <- gWidgets2::gbutton("+", cont=gCount26, handler = function(h,...){
      count26 <-  as.numeric(gWidgets2::svalue(lCount26)) + 1
      gWidgets2::svalue(lCount26) <- count26
    })

    # Keyboard Shortcut
    gKeyShrt26 <- gWidgets2::ggroup(cont = gSpec26, horizontal = TRUE)
    lKeyShrt26 <- gWidgets2::glabel("Shortcut Key:", cont = gKeyShrt26)
    lKeyPress26 <- gWidgets2::glabel(keys[26], container = gKeyShrt26, editable = TRUE)
    gWidgets2::addHandlerKeystroke(gKeyFocus, handler = function(h,...){
      if (h$key == tolower(gWidgets2::svalue(lKeyPress26))){
        count26 <-  as.numeric(gWidgets2::svalue(lCount26)) + 1
        gWidgets2::svalue(lCount26) <- count26
      }
    })
    gWidgets2::gseparator(cont=row5, horizontal = FALSE)
  }
  ### End Species 26 ###

  ### Species 27 ###
  if (isFALSE(is.na(data[27]))){
    gSpec27 <- gWidgets2::ggroup(cont=row5, horizontal=FALSE)
    Spec27Name <- gWidgets2::glabel(data[27], container=gSpec27, editable = FALSE)

    # Counter
    gCount27 <- gWidgets2::ggroup(cont=gSpec27, horizontal=TRUE)

    count27 <- 0
    Spec27sub <- gWidgets2::gbutton("-", cont=gCount27, handler = function(h,...){
      count27 <-  as.numeric(gWidgets2::svalue(lCount27)) - 1
      gWidgets2::svalue(lCount27) <- count27
    })
    lCount27 <- gWidgets2::glabel(count27, container=gCount27, editable = TRUE)
    Spec27add <- gWidgets2::gbutton("+", cont=gCount27, handler = function(h,...){
      count27 <-  as.numeric(gWidgets2::svalue(lCount27)) + 1
      gWidgets2::svalue(lCount27) <- count27
    })

    # Keyboard Shortcut
    gKeyShrt27 <- gWidgets2::ggroup(cont = gSpec27, horizontal = TRUE)
    lKeyShrt27 <- gWidgets2::glabel("Shortcut Key:", cont = gKeyShrt27)
    lKeyPress27 <- gWidgets2::glabel(keys[27], container = gKeyShrt27, editable = TRUE)
    gWidgets2::addHandlerKeystroke(gKeyFocus, handler = function(h,...){
      if (h$key == tolower(gWidgets2::svalue(lKeyPress27))){
        count27 <-  as.numeric(gWidgets2::svalue(lCount27)) + 1
        gWidgets2::svalue(lCount27) <- count27
      }
    })
    gWidgets2::gseparator(cont=row5, horizontal = FALSE)
  }
  ### End Species 27 ###

  ### Species 28 ###
  if (isFALSE(is.na(data[28]))){
    gSpec28 <- gWidgets2::ggroup(cont=row5, horizontal=FALSE)
    Spec28Name <- gWidgets2::glabel(data[28], container=gSpec28, editable = FALSE)

    # Counter
    gCount28 <- gWidgets2::ggroup(cont=gSpec28, horizontal=TRUE)

    count28 <- 0
    Spec28sub <- gWidgets2::gbutton("-", cont=gCount28, handler = function(h,...){
      count28 <-  as.numeric(gWidgets2::svalue(lCount28)) - 1
      gWidgets2::svalue(lCount28) <- count28
    })
    lCount28 <- gWidgets2::glabel(count28, container=gCount28, editable = TRUE)
    Spec28add <- gWidgets2::gbutton("+", cont=gCount28, handler = function(h,...){
      count28 <-  as.numeric(gWidgets2::svalue(lCount28)) + 1
      gWidgets2::svalue(lCount28) <- count28
    })

    # Keyboard Shortcut
    gKeyShrt28 <- gWidgets2::ggroup(cont = gSpec28, horizontal = TRUE)
    lKeyShrt28 <- gWidgets2::glabel("Shortcut Key:", cont = gKeyShrt28)
    lKeyPress28 <- gWidgets2::glabel(keys[28], container = gKeyShrt28, editable = TRUE)
    gWidgets2::addHandlerKeystroke(gKeyFocus, handler = function(h,...){
      if (h$key == tolower(gWidgets2::svalue(lKeyPress28))){
        count28 <-  as.numeric(gWidgets2::svalue(lCount28)) + 1
        gWidgets2::svalue(lCount28) <- count28
      }
    })
    gWidgets2::gseparator(cont=row5, horizontal = FALSE)
  }
  ### End Species 28 ###

  ### Species 29 ###
  if (isFALSE(is.na(data[29]))){
    gSpec29 <- gWidgets2::ggroup(cont=row5, horizontal=FALSE)
    Spec29Name <- gWidgets2::glabel(data[29], container=gSpec29, editable = FALSE)

    # Counter
    gCount29 <- gWidgets2::ggroup(cont=gSpec29, horizontal=TRUE)

    count29 <- 0
    Spec29sub <- gWidgets2::gbutton("-", cont=gCount29, handler = function(h,...){
      count29 <-  as.numeric(gWidgets2::svalue(lCount29)) - 1
      gWidgets2::svalue(lCount29) <- count29
    })
    lCount29 <- gWidgets2::glabel(count29, container=gCount29, editable = TRUE)
    Spec29add <- gWidgets2::gbutton("+", cont=gCount29, handler = function(h,...){
      count29 <-  as.numeric(gWidgets2::svalue(lCount29)) + 1
      gWidgets2::svalue(lCount29) <- count29
    })

    # Keyboard Shortcut
    gKeyShrt29 <- gWidgets2::ggroup(cont = gSpec29, horizontal = TRUE)
    lKeyShrt29 <- gWidgets2::glabel("Shortcut Key:", cont = gKeyShrt29)
    lKeyPress29 <- gWidgets2::glabel(keys[29], container = gKeyShrt29, editable = TRUE)
    gWidgets2::addHandlerKeystroke(gKeyFocus, handler = function(h,...){
      if (h$key == tolower(gWidgets2::svalue(lKeyPress29))){
        count29 <-  as.numeric(gWidgets2::svalue(lCount29)) + 1
        gWidgets2::svalue(lCount29) <- count29
      }
    })
    gWidgets2::gseparator(cont=row5, horizontal = FALSE)
  }
  ### End Species 29 ###

  ### Species 30 ###
  if (isFALSE(is.na(data[30]))){
    gSpec30 <- gWidgets2::ggroup(cont=row5, horizontal=FALSE)
    Spec30Name <- gWidgets2::glabel(data[30], container=gSpec30, editable = FALSE)

    # Counter
    gCount30 <- gWidgets2::ggroup(cont=gSpec30, horizontal=TRUE)

    count30 <- 0
    Spec30sub <- gWidgets2::gbutton("-", cont=gCount30, handler = function(h,...){
      count30 <-  as.numeric(gWidgets2::svalue(lCount30)) - 1
      gWidgets2::svalue(lCount30) <- count30
    })
    lCount30 <- gWidgets2::glabel(count30, container=gCount30, editable = TRUE)
    Spec30add <- gWidgets2::gbutton("+", cont=gCount30, handler = function(h,...){
      count30 <-  as.numeric(gWidgets2::svalue(lCount30)) + 1
      gWidgets2::svalue(lCount30) <- count30
    })

    # Keyboard Shortcut
    gKeyShrt30 <- gWidgets2::ggroup(cont = gSpec30, horizontal = TRUE)
    lKeyShrt30 <- gWidgets2::glabel("Shortcut Key:", cont = gKeyShrt30)
    lKeyPress30 <- gWidgets2::glabel(keys[30], container = gKeyShrt30, editable = TRUE)
    gWidgets2::addHandlerKeystroke(gKeyFocus, handler = function(h,...){
      if (h$key == tolower(gWidgets2::svalue(lKeyPress30))){
        count30 <-  as.numeric(gWidgets2::svalue(lCount30)) + 1
        gWidgets2::svalue(lCount30) <- count30
      }
    })
  }
  ### End Species 30 ###

  # Export Button

  exportbutton <- gWidgets2::gbutton("Export to CSV", cont=gmaster, handler = function(h,...){
    Count <- c()
    for (i in 1:length(data)){
      Count <- append(Count, gWidgets2::svalue(get(paste0("lCount", i))))
    }
    df <- cbind(Species = data, Count = Count)
    filepath <- gWidgets2::gfile(text = "Select filepath", type = "save", initial.filename = paste0("Zooplankton Counts ", Sys.Date(), ".csv"))
    if (stringr::str_sub(filepath,-4,-1) != ".csv"){
      filepath <- paste0(filepath, ".csv")
    }
    utils::write.csv(df, file.path(filepath), row.names = FALSE)
  })




  # Making the window visible - RUN LAST!

  gWidgets2::visible(w) <- TRUE

}
