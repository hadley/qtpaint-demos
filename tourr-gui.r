# source("~/Documents/cranvas/demos/tourr-gui.r"); gui_xy()

library(qtbase)
library(qtpaint)
library(ggplot2, warn.conflicts = FALSE)
library(tourr, warn.conflicts = FALSE)
library(colorspace)
library(RGtk2)
library(gWidgets)

gui_xy <- function(data = flea, ...) {
  num <- sapply(data, is.numeric)
  
  tour <- NULL
  tour_anim <- NULL
  update_tour <- function(...) {
    tour <<- create_tour(data, 
      var_selected = svalue(Variables), 
      cat_selected = svalue(Class), 
      axes_location = svalue(dl),
      tour_type = svalue(TourType),
      aps = svalue(sl)
    )
    tour_anim <<- with(tour, tourer(data, tour_path, proj = cur_proj, velocity = aps / 33))
    TRUE
  }

  data_proj <- NULL
  cur_proj <- NULL
  step_tour <- function(...) {
    # if there's no tour, don't draw anything
    if (is.null(tour)) return(FALSE)

    tour_step <- tour_anim$step2(svalue(sl) / 33)
    if (is.null(tour_step$proj)) return(FALSE)

    cur_proj <<- tour_step$proj
    
    data_proj <<- tour$data %*% tour_step$proj
    qupdate(points)
    
    return(TRUE)
  }

  render_tour <- function(item, painter, exposed) {
    col <- alpha(tour$colour, svalue(sl_alpha))
    size <- svalue(sl_size)
    if (size == 1) {
      qdrawPoint(painter, data_proj[, 1], data_proj[,2], stroke = col)      
    } else {
      circle <- qpathCircle(0, 0, size)
      qstrokeColor(painter) <- NA
      qdrawGlyph(painter, circle, data_proj[, 1], data_proj[,2], fill = col)
    }
    
    # Draw axes
    if (!is.null(cur_proj)) {
      pos <- cur_proj * 2
      labels <- abbreviate(colnames(tour$data))
       
      qstrokeColor(painter) <- "grey50"
      qdrawSegment(painter, 0, 0, pos[, 1], pos[, 2])
      theta <- seq(0, 2 * pi, length = 50)
      qdrawLine(painter, cos(theta) * 2, sin(theta) * 2)
      qdrawText(painter, labels, pos[, 1], pos[, 2])
      
    }
    
  }
  
  # ==================Controls==========================
  w <- gwindow("2D Tour plot example", visible = FALSE)
  vbox <- glayout(cont = w)

  # Variable selection column
  vbox[1, 1, anchor = c(-1, 0)] <- "Variable Selection"
  vbox[2, 1] <- Variables <- gcheckboxgroup(names(data[num]), 
    checked = TRUE, horizontal = FALSE)
    
  class_box <- ggroup(hor = F)
  add(class_box, glabel("Colour by"))
  add(class_box, Class <- gtable(c("None", names(data)[!num]), multiple = TRUE), 
    expand = TRUE)
  vbox[5, 4, expand = TRUE] <- class_box
  
  # Tour selection column
  vbox[1, 3, anchor=c(-1, 0)] <- "Tour Type"
  tour_types <- c("Grand", "Little", "Guided(holes)", "Guided(cm)", "Guided(lda_pp)", "Local")
  vbox[2, 3] <- TourType <- gradio(tour_types)

  # control aesthetics
  aes_box <- glayout()
  
  aes_box[1,1, anchor = c(1, -1)] <- "Speed"
  aes_box[1,2, expand = TRUE] <- sl <- 
    gslider(from = 0, to = 5, by = 0.1, value = 1)
  aes_box[2,1, anchor = c(1, -1)] <- "Transparency"
  aes_box[2,2, expand = TRUE] <- sl_alpha <- 
    gslider(from = 0, to = 1, by = 0.01, value = 1)
  aes_box[3,1, anchor = c(1, -1)] <- "Size"
  aes_box[3,2, expand = TRUE] <- sl_size <- 
    gslider(from = 1, to = 8, by = 0.5, value = 2)
  
  vbox[5, 1:3] <- aes_box

  # buttons control
  buttonGroup <- ggroup(horizontal = F, cont=vbox)  
  
  gbutton("Apply", cont = buttonGroup, handler = function(...) {
    pause(FALSE)
    update_tour()
  })
  gbutton("Quit",cont=buttonGroup, handler = function(...) {
    pause(TRUE)
    qclose(view)
    dispose(w)
  })
  timer <- qtimer(30, step_tour)
  pause <- function(paused) {
    svalue(chk_pause) <- paused
    if (paused) {
      timer$stop()
    } else {
      timer$start()
    }
  }
  chk_pause <- gcheckbox("Pause", cont = buttonGroup,
    handler = function(h, ...) pause(svalue(h$obj)))


  vbox[2, 4, anchor = c(0, 1)] <- buttonGroup
  
  # Create canvas for displaying tour
  scene <- qgraphicsScene()
  root <- qlayer(scene)

  points <- qlayer(root, render_tour)
  qlimits(points) <- qrect(c(-3, 3), c(-3, 3))
  
  update_tour()
  pause(FALSE)
  visible(w) <- TRUE

  view <- qplotView(scene = scene, opengl = F)
  print(view)
  
  invisible()
}


create_tour <- function(data, var_selected, cat_selected, axes_location, tour_type, aps) {
  if (length(var_selected) < 3) {
    gmessage("Please select at least three variables", icon = "warning")
    return()
  }
  
  # Work out point colours
  if (length(cat_selected) > 0  && cat_selected[1] != "None" ) {
    cat <- data[cat_selected]
    # collapse to single variable if multiple selected
    int <- interaction(cat, drop = TRUE)
    pal <- rainbow_hcl(length(levels(int)))
    col <- pal[as.numeric(int)]
  } else {
    col <- "black"
  }
  
  # Work out which type of tour to use
  tour <- switch(tour_type,
    "Grand" = grand_tour(), 
    "Little" = little_tour(), 
    "Guided(holes)" = guided_tour(holes), 
    "Guided(cm)" = guided_tour(cm), 
    "Guided(lda_pp)" = guided_tour(lda_pp(data[,cat_selected])),
    "Local" = local_tour()
  )
  
  
  sel <- data[var_selected]
  # Sphere the data if we're using a guided tour
  if (length(grep(tour_type, "Guided")) > 0) {
    sel <- sphere(sel)
  }
  
  list(
    data = rescale(sel),
    tour_path = tour,
    colour = col,
    aps = aps
  )
}
