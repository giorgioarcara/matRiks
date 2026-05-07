#' Draw (Method)
#'
#' Draws single figures, matrices with 9 or 4 cells, or response list of a matriks
#'
#' @param obj The figure/matriks/response list to be drawn
#' @param main logical, print the title of the drawing. Default is FALSE
#' @param canvas logical, draw the figure on a new canvas. Default is TRUE
#' @param hide logical, hide the cell corresponding to the correct response. Default is FALSE
#' @param bg character, define the color background. Default is white
#' @param mar numeric vector, change margins of the canvas
#' @param xlim numeric, change the length of the x axis
#' @param oma numeric vector, change the outer margins of the whole figure. Default is c(4,4,0.2,0.2)
#' @param ... other arguments
#'
#' @return A graphic
#' @import DescTools
#' @importFrom graphics par
#' @importFrom utils read.csv
#' @export draw
#' @export
#'
#' @examples
#' # draw a circle
#' draw(circle())
#' # draw a circle inside the first circle
#' draw(size(circle(), 2), canvas = FALSE)
draw <- function(obj, main = NULL, canvas = TRUE,
                 hide = FALSE,
                 bg = "white", mar=c(1,1,1,1), xlim=16,
                 oma = c(4,4,0.2,0.2), ...) {
  UseMethod("draw")
}

#' @describeIn draw Draw figure
#'
#' Draw a figure
#'
#' @param obj The figure to be drawn
#' @param main logical, print the title of the drawing. Default is FALSE
#' @param canvas logical, draw the figure on a new canvas. Default is TRUE
#' @param hide logical, hide the cell corresponding to the correct response. Default is FALSE
#' @param bg character, define the color background. Default is white
#' @param mar numeric vector, change margins of the canvas
#' @param xlim numeric, change the length of the x axis
#' @param oma numeric vector, change the outer margins of the whole figure. Default is c(4,4,0.2,0.2)
#' @param ... other arguments
#'
#' @return A graphic of the figure
#' @export draw.figure
#' @export
#'
#'
#' @examples
#' # draw a circle
#' draw.figure(circle())
#'
#' # draw a circle inside the other
#' draw.figure(size(circle(), 2), canvas = FALSE)
draw.figure <- function(obj, main = NULL, canvas = TRUE,
                        hide = FALSE,
                        bg = "white", mar=c(1,1,1,1), xlim=16,
                        oma = c(4,4,0.2,0.2), ...) {
  if (canvas == TRUE)
  {
    DescTools::Canvas(xlim=xlim, mar=mar, main = main, bg = bg, ...)
  }
  for(j in 1:length(obj$shape))
  {
    if(obj$visible[[j]]==1)
    {
      if(obj$num[[j]][1]==1){
        DescTools::DrawRegPolygon(x = obj$pos.x[[j]], y = obj$pos.y[[j]], rot = obj$rotation[[j]],
                                  radius.x = obj$size.x[[j]], radius.y = obj$size.y[[j]], nv = obj$nv[[j]],
                                  lty=obj$lty[[j]], lwd=obj$lwd[[j]], col = obj$shade[[j]])
        
      }else{
        DescTools::DrawCircle(x = obj$pos.x[[j]], y = obj$pos.y[[j]],
                              r.out = obj$size.x[[j]], r.in= obj$size.y[[j]],
                              theta.1=obj$theta.1[[j]],
                              theta.2=obj$theta.2[[j]], nv = obj$nv[[j]],
                              lty=obj$lty[[j]], lwd=obj$lwd[[j]], col = obj$shade[[j]])
      }
    }
  }
  
}

#' @describeIn draw Draw Matriks
#'
#' Draw a matriks
#'
#' @param obj The matriks to be drawn
#' @param main logical, print the title of the drawing. Default is FALSE
#' @param canvas logical, draw the figure on a new canvas. Default is TRUE
#' @param hide logical, hide the cell corresponding to the correct response. Default is FALSE
#' @param bg character, define the color background. Default is white
#' @param mar numeric vector, change margins of the canvas
#' @param xlim numeric, change the length of the x axis
#' @param oma numeric vector, change the outer margins of the whole figure. Default is c(4,4,0.2,0.2)
#' @param ... other arguments
#'
#' @return A graphic of the matriks
#' @export draw.matriks
#' @export
#'
#' @examples
#' # draw a matriks
#' my_mat <- mat_apply(cof(circle(), luck(), pacman()), "shade", "shape")
#' draw(my_mat)
draw.matriks <- function(obj, main = NULL, canvas = TRUE,
                         hide = FALSE,
                         bg = "white", mar=c(1,1,1,1), xlim=16,
                         oma = c(4,4,0.2,0.2), ...) {
  oldpar <- par(no.readonly = TRUE)
  oldpar <- oldpar[!names(oldpar) %in% c("pin", "fin", "plt", "pty")]
  on.exit(par(oldpar))
  # ti prego non arrabbiarti
  # come non fungeva più per ragioni che boh, ho capito che aveva bisogno di nuove funzioni (cof.double e cof.numeric)
  # quando commi delle matrici, mat.type prende classe double ed ha lunghezza del numero di matrici
  # che sono state concatenate (questa può essere una info importante per il futuro)
  # sicuramente tu hai una soluzione più elegante ma ora la solzuzione qui and dirty è:
  # il probklema di questa soluzion eè che è stata estesa a tutto
  n.cell <- obj$mat.type
  
  squares <- paste0("Sq", 1:9)
  if (n.cell == 9) {
    par(
      mfrow = c(3, 3),
      mar = c(0.5, 6, 0.5, 2) + .1,
      mai = c(.1, .1, .1, .1),
      oma = oma
    )
    
  } else if (n.cell == 4) {
    par(
      mfrow = c(2, 2),
      mar = c(0.5, 6, 0.5, 2) + .1,
      mai = c(.1, .1, .1, .1),
      oma = oma
    )
  }
  
  if (hide == TRUE){n.cell <- n.cell-1}
  for (i in 1:n.cell)
  {
    DescTools::Canvas(xlim=16,mar=c(1,1,1,1), main = main, bg = bg)
    draw(obj[[squares[[i]]]],canvas = FALSE)
  }
  
}


#' @describeIn draw Draw response list
#'
#' Draw the response list of a matriks
#'
#' @param obj The figure/matriks/response list to be drawn
#' @param main logical, print the title of the drawing. Default is FALSE
#' @param canvas logical, draw the figure on a new canvas. Default is TRUE
#' @param hide logical, hide the cell corresponding to the correct response. Default is FALSE
#' @param bg character, define the color background. Default is white
#' @param mar numeric vector, change margins of the canvas
#' @param xlim numeric, change the length of the x axis
#' @param oma numeric vector, change the outer margins of the whole figure. Default is c(4,4,0.2,0.2)
#' @param distractors integer, default length of the response list. Works also with a character vector with the labels of the desired distractors.
#' @param labels character, alternative labels to be printed as the main title of the distractors (main = TRUE). The labels must have the same length as the vector of distractors.
#' @param print logical, print all the distractors together (default, FALSE) or one by one (TRUE)
#' @param frow numeric, vector of length 2 (nrow, ncol), response options will be drawn in a nrow x ncol array. Further details in `par()` documentation
#' @param ... other arguments
#'
#' @return A graphic of the matriks
#' @export draw.responses
#' @export
#'
#' @examples
#' # generate a matriks
#' my_mat1 <- mat_apply(cof(s_axe(), luck(), pacman()), "rotate", "shape")
#' my_mat2 <- mat_apply(dot(), "shade", "shade")
#' my_mat <- com(my_mat1, my_mat2)
#' # generate a response list
#' my_resp <- response_list(my_mat)
#' # draw response list
#' draw(my_resp)
draw.responses <- function(obj, main = FALSE, canvas = TRUE,
                           hide = FALSE,
                           bg = "white", mar=c(1,1,1,1), xlim=16,
                           oma = c(4,4,0.2,0.2),
                           distractors = NULL, labels = NULL,
                           print = FALSE, frow = NULL,
                           ...) {
  oldpar <- par(no.readonly = TRUE)
  oldpar <- oldpar[!names(oldpar) %in% c("pin", "fin", "plt", "pty")]
  on.exit(par(oldpar))
  if (is.null(distractors)) distractors <- 1:length(obj)
  if (is.null(frow)) frow = c(2, round(length(distractors)/2 + 0.2))
  if (prod(frow) < length(distractors)) stop("There's not enough space for all the selected response options, increase frow")
  if (is.null(labels) == TRUE) {
    labels <- names(obj)
  } else if (is.null(labels) == FALSE) {
    labels <- labels
    main <- TRUE
  }
  
  if (print == FALSE) {
    par(mfrow = frow,
        mar = c(0.5, 6, 0.5, 2) + .1,
        mai = c(.1, .1, .1, .1),
        oma = oma)
    
  } else {
    par(mfrow = c(1, 1),
        mar = c(0.5, 6, 0.5, 2) + .1,
        mai = c(.1, .1, .1, .1),
        oma = oma)
  }
  
  if (main == TRUE) {
    for (i in 1:length(distractors)) {
      draw(obj[[distractors[i]]], main = labels[i])
    }
  } else {
    for (i in 1:length(distractors)) {
      draw(obj[[distractors[i]]])
    }
  }
  
}


#' Draw a matriks and its response list on the same device
#'
#' Draws a matriks in the upper portion of the device and its response list
#' in the lower portion. Uses \code{cowplot} to capture each base R plot
#' independently and compose them, avoiding layout conflicts between the two
#' draw calls.
#'
#' @param mat A matriks object to be drawn in the upper panel
#' @param resp A responses object to be drawn in the lower panel
#' @param resp_height numeric, proportion of the device height allocated to the
#'   response list. Must be between 0 and 1. Default is 0.3.
#' @param frow numeric vector of length 2 (nrow, ncol), layout of the response
#'   options panel. Default is c(1, 4).
#' @param mat_args list, additional arguments passed to \code{draw(mat, ...), default hide=TRUE}
#' @param resp_args list, additional arguments passed to \code{draw(resp, ...)}
#'
#' @return A ggplot/cowplot object rendered to the current device
#' @import cowplot
#' @export
#'
#' @examples
#' my_mat <- mat_apply(cof(circle(), luck(), pacman()), "shade", "shape")
#' my_resp <- response_list(my_mat)
#' draw_combined(my_mat, my_resp, resp_height = 0.3)
draw_combined <- function(mat, resp,
                          resp_height = 0.3,
                          frow = NULL,
                          mat_args = list(hide=TRUE),
                          resp_args = list()) {
  
  g_mat <- cowplot::as_grob(function() {
    do.call(draw, c(list(mat), mat_args))
  })
  
  g_resp <- cowplot::as_grob(function() {
    resp_call <- c(list(resp), resp_args)
    if (!is.null(frow)) resp_call <- c(resp_call, list(frow = frow))
    do.call(draw, resp_call)
  })
  
  cowplot::plot_grid(g_mat, g_resp,
                     ncol = 1,
                     rel_heights = c(1 - resp_height, resp_height))
}
