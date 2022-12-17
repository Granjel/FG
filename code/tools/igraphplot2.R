#modified igraph plotting code to allow for changes in edge.arrow.width. Edge.arrow.size still not supported

plot.igraph2=function (x, axes = FALSE, add = FALSE, xlim = c(-1, 1), ylim = c(-1, 
                                                                               1), mark.groups = list(), mark.shape = 1/2, mark.col = rainbow(length(mark.groups), 
                                                                                                                                              alpha = 0.3), mark.border = rainbow(length(mark.groups), 
                                                                                                                                                                                  alpha = 1), mark.expand = 15, ...) 
{
  graph <- x
  if (!is_igraph(graph)) {
    stop("Not a graph object")
  }
  params <- i.parse.plot.params(graph, list(...))
  vertex.size <- 1/200 * params("vertex", "size")
  label.family <- params("vertex", "label.family")
  label.font <- params("vertex", "label.font")
  label.cex <- params("vertex", "label.cex")
  label.degree <- params("vertex", "label.degree")
  label.color <- params("vertex", "label.color")
  label.dist <- params("vertex", "label.dist")
  labels <- params("vertex", "label")
  shape <- igraph.check.shapes(params("vertex", "shape"))
  edge.color <- params("edge", "color")
  edge.width <- params("edge", "width")
  edge.lty <- params("edge", "lty")
  arrow.mode <- params("edge", "arrow.mode")
  edge.labels <- params("edge", "label")
  loop.angle <- params("edge", "loop.angle")
  edge.label.font <- params("edge", "label.font")
  edge.label.family <- params("edge", "label.family")
  edge.label.cex <- params("edge", "label.cex")
  edge.label.color <- params("edge", "label.color")
  elab.x <- params("edge", "label.x")
  elab.y <- params("edge", "label.y")
  arrow.size <- params("edge", "arrow.size")
  arrow.width <- params("edge", "arrow.width")
  curved <- params("edge", "curved")
  if (is.function(curved)) {
    curved <- curved(graph)
  }
  layout <- params("plot", "layout")
  margin <- params("plot", "margin")
  margin <- rep(margin, length = 4)
  rescale <- params("plot", "rescale")
  asp <- params("plot", "asp")
  frame <- params("plot", "frame")
  main <- params("plot", "main")
  sub <- params("plot", "sub")
  xlab <- params("plot", "xlab")
  ylab <- params("plot", "ylab")
  palette <- params("plot", "palette")
  if (!is.null(palette)) {
    old_palette <- palette(palette)
    on.exit(palette(old_palette), add = TRUE)
  }
  arrow.mode <- i.get.arrow.mode(graph, arrow.mode)
  maxv <- max(vertex.size)
  if (rescale) {
    layout <- norm_coords(layout, -1, 1, -1, 1)
    xlim <- c(xlim[1] - margin[2] - maxv, xlim[2] + margin[4] + 
                maxv)
    ylim <- c(ylim[1] - margin[1] - maxv, ylim[2] + margin[3] + 
                maxv)
  }
  if (!add) {
    plot(0, 0, type = "n", xlab = xlab, ylab = ylab, xlim = xlim, 
         ylim = ylim, axes = axes, frame = frame, asp = asp, 
         main = main, sub = sub)
  }
  if (!is.list(mark.groups) && is.numeric(mark.groups)) {
    mark.groups <- list(mark.groups)
  }
  mark.shape <- rep(mark.shape, length = length(mark.groups))
  mark.border <- rep(mark.border, length = length(mark.groups))
  mark.col <- rep(mark.col, length = length(mark.groups))
  mark.expand <- rep(mark.expand, length = length(mark.groups))
  for (g in seq_along(mark.groups)) {
    v <- V(graph)[mark.groups[[g]]]
    if (length(vertex.size) == 1) {
      vs <- vertex.size
    }
    else {
      vs <- rep(vertex.size, length = vcount(graph))[v]
    }
    igraph.polygon(layout[v, , drop = FALSE], vertex.size = vs, 
                   expand.by = mark.expand[g]/200, shape = mark.shape[g], 
                   col = mark.col[g], border = mark.border[g])
  }
  el <- as_edgelist(graph, names = FALSE)
  loops.e <- which(el[, 1] == el[, 2])
  nonloops.e <- which(el[, 1] != el[, 2])
  loops.v <- el[, 1][loops.e]
  loop.labels <- edge.labels[loops.e]
  loop.labx <- if (is.null(elab.x)) {
    rep(NA, length(loops.e))
  }
  else {
    elab.x[loops.e]
  }
  loop.laby <- if (is.null(elab.y)) {
    rep(NA, length(loops.e))
  }
  else {
    elab.y[loops.e]
  }
  edge.labels <- edge.labels[nonloops.e]
  elab.x <- if (is.null(elab.x)) 
    NULL
  else elab.x[nonloops.e]
  elab.y <- if (is.null(elab.y)) 
    NULL
  else elab.y[nonloops.e]
  el <- el[nonloops.e, , drop = FALSE]
  edge.coords <- matrix(0, nrow = nrow(el), ncol = 4)
  edge.coords[, 1] <- layout[, 1][el[, 1]]
  edge.coords[, 2] <- layout[, 2][el[, 1]]
  edge.coords[, 3] <- layout[, 1][el[, 2]]
  edge.coords[, 4] <- layout[, 2][el[, 2]]
  if (length(unique(shape)) == 1) {
    ec <- .igraph.shapes[[shape[1]]]$clip(edge.coords, el, 
                                          params = params, end = "both")
  }
  else {
    shape <- rep(shape, length = vcount(graph))
    ec <- edge.coords
    ec[, 1:2] <- t(sapply(seq(length = nrow(el)), function(x) {
      .igraph.shapes[[shape[el[x, 1]]]]$clip(edge.coords[x, 
                                                         , drop = FALSE], el[x, , drop = FALSE], params = params, 
                                             end = "from")
    }))
    ec[, 3:4] <- t(sapply(seq(length = nrow(el)), function(x) {
      .igraph.shapes[[shape[el[x, 2]]]]$clip(edge.coords[x, 
                                                         , drop = FALSE], el[x, , drop = FALSE], params = params, 
                                             end = "to")
    }))
  }
  x0 <- ec[, 1]
  y0 <- ec[, 2]
  x1 <- ec[, 3]
  y1 <- ec[, 4]
  if (length(loops.e) > 0) {
    ec <- edge.color
    if (length(ec) > 1) {
      ec <- ec[loops.e]
    }
    point.on.cubic.bezier <- function(cp, t) {
      c <- 3 * (cp[2, ] - cp[1, ])
      b <- 3 * (cp[3, ] - cp[2, ]) - c
      a <- cp[4, ] - cp[1, ] - c - b
      t2 <- t * t
      t3 <- t * t * t
      a * t3 + b * t2 + c * t + cp[1, ]
    }
    compute.bezier <- function(cp, points) {
      dt <- seq(0, 1, by = 1/(points - 1))
      sapply(dt, function(t) point.on.cubic.bezier(cp, 
                                                   t))
    }
    plot.bezier <- function(cp, points, color, width, arr, 
                            lty, arrow.size, arr.w) {
      p <- compute.bezier(cp, points)
      polygon(p[1, ], p[2, ], border = color, lwd = width, 
              lty = lty)
      if (arr == 1 || arr == 3) {
        igraph.Arrows(p[1, ncol(p) - 1], p[2, ncol(p) - 
                                             1], p[1, ncol(p)], p[2, ncol(p)], sh.col = color, 
                      h.col = color, size = arrow.size, sh.lwd = width, 
                      h.lwd = width, open = FALSE, code = 2, width = arr.w)
      }
      if (arr == 2 || arr == 3) {
        igraph.Arrows(p[1, 2], p[2, 2], p[1, 1], p[2, 
                                                   1], sh.col = color, h.col = color, size = arrow.size, 
                      sh.lwd = width, h.lwd = width, open = FALSE, 
                      code = 2, width = arr.w)
      }
    }
    loop <- function(x0, y0, cx = x0, cy = y0, color, angle = 0, 
                     label = NA, width = 1, arr = 2, lty = 1, arrow.size = arrow.size, 
                     arr.w = arr.w, lab.x, lab.y) {
      rad <- angle
      center <- c(cx, cy)
      cp <- matrix(c(x0, y0, x0 + 0.4, y0 + 0.2, x0 + 0.4, 
                     y0 - 0.2, x0, y0), ncol = 2, byrow = TRUE)
      phi <- atan2(cp[, 2] - center[2], cp[, 1] - center[1])
      r <- sqrt((cp[, 1] - center[1])^2 + (cp[, 2] - center[2])^2)
      phi <- phi + rad
      cp[, 1] <- cx + r * cos(phi)
      cp[, 2] <- cy + r * sin(phi)
      plot.bezier(cp, 50, color, width, arr = arr, lty = lty, 
                  arrow.size = arrow.size, arr.w = arr.w)
      if (is.language(label) || !is.na(label)) {
        lx <- x0 + 0.3
        ly <- y0
        phi <- atan2(ly - center[2], lx - center[1])
        r <- sqrt((lx - center[1])^2 + (ly - center[2])^2)
        phi <- phi + rad
        lx <- cx + r * cos(phi)
        ly <- cy + r * sin(phi)
        if (!is.na(lab.x)) {
          lx <- lab.x
        }
        if (!is.na(lab.y)) {
          ly <- lab.y
        }
        text(lx, ly, label, col = edge.label.color, font = edge.label.font, 
             family = edge.label.family, cex = edge.label.cex)
      }
    }
    ec <- edge.color
    if (length(ec) > 1) {
      ec <- ec[loops.e]
    }
    vs <- vertex.size
    if (length(vertex.size) > 1) {
      vs <- vs[loops.v]
    }
    ew <- edge.width
    if (length(edge.width) > 1) {
      ew <- ew[loops.e]
    }
    la <- loop.angle
    if (length(loop.angle) > 1) {
      la <- la[loops.e]
    }
    lty <- edge.lty
    if (length(edge.lty) > 1) {
      lty <- lty[loops.e]
    }
    arr <- arrow.mode
    if (length(arrow.mode) > 1) {
      arr <- arrow.mode[loops.e]
    }
    asize <- arrow.size
    if (length(arrow.size) > 1) {
      asize <- arrow.size[loops.e]
    }
    xx0 <- layout[loops.v, 1] + cos(la) * vs
    yy0 <- layout[loops.v, 2] - sin(la) * vs
    mapply(loop, xx0, yy0, color = ec, angle = -la, label = loop.labels, 
           lty = lty, width = ew, arr = arr, arrow.size = asize, 
           arr.w = arrow.width, lab.x = loop.labx, lab.y = loop.laby)
  }
  if (length(x0) != 0) {
    if (length(edge.color) > 1) {
      edge.color <- edge.color[nonloops.e]
    }
    if (length(edge.width) > 1) {
      edge.width <- edge.width[nonloops.e]
    }
    if (length(edge.lty) > 1) {
      edge.lty <- edge.lty[nonloops.e]
    }
    if (length(arrow.mode) > 1) {
      arrow.mode <- arrow.mode[nonloops.e]
    }
    if (length(arrow.size) > 1) {
      arrow.size <- arrow.size[nonloops.e]
      #modify here for multple arrow sizes - will pad out vector inside arrow function
      arrow.size = arrow.size[!is.na(arrow.size)]
    }
    if (length(curved) > 1) {
      curved <- curved[nonloops.e]
    }
    if (length(unique(arrow.mode)) == 1) {
      lc <- igraph.Arrows2(x0, y0, x1, y1, h.col = edge.color, 
                           sh.col = edge.color, sh.lwd = edge.width, h.lwd = 1, 
                           open = FALSE, code = arrow.mode[1], sh.lty = edge.lty, 
                           h.lty = 1, size = arrow.size, width = arrow.width, 
                           curved = curved)
      lc.x <- lc$lab.x
      lc.y <- lc$lab.y
    }
    else {
      curved <- rep(curved, length = ecount(graph))[nonloops.e]
      lc.x <- lc.y <- numeric(length(curved))
      for (code in 0:3) {
        valid <- arrow.mode == code
        if (!any(valid)) {
          next
        }
        ec <- edge.color
        if (length(ec) > 1) {
          ec <- ec[valid]
        }
        ew <- edge.width
        if (length(ew) > 1) {
          ew <- ew[valid]
        }
        el <- edge.lty
        if (length(el) > 1) {
          el <- el[valid]
        }
        lc <- igraph.Arrows(x0[valid], y0[valid], x1[valid], 
                            y1[valid], code = code, sh.col = ec, h.col = ec, 
                            sh.lwd = ew, h.lwd = 1, h.lty = 1, sh.lty = el, 
                            open = FALSE, size = arrow.size, width = arrow.width, 
                            curved = curved[valid])
        lc.x[valid] <- lc$lab.x
        lc.y[valid] <- lc$lab.y
      }
    }
    if (!is.null(elab.x)) {
      lc.x <- ifelse(is.na(elab.x), lc.x, elab.x)
    }
    if (!is.null(elab.y)) {
      lc.y <- ifelse(is.na(elab.y), lc.y, elab.y)
    }
    text(lc.x, lc.y, labels = edge.labels, col = edge.label.color, 
         family = edge.label.family, font = edge.label.font, 
         cex = edge.label.cex)
  }
  rm(x0, y0, x1, y1)
  if (length(unique(shape)) == 1) {
    .igraph.shapes[[shape[1]]]$plot(layout, params = params)
  }
  else {
    sapply(seq(length = vcount(graph)), function(x) {
      .igraph.shapes[[shape[x]]]$plot(layout[x, , drop = FALSE], 
                                      v = x, params = params)
    })
  }
  par(xpd = TRUE)
  x <- layout[, 1] + label.dist * cos(-label.degree) * (vertex.size + 
                                                          6 * 8 * log10(2))/200
  y <- layout[, 2] + label.dist * sin(-label.degree) * (vertex.size + 
                                                          6 * 8 * log10(2))/200
  if (length(label.family) == 1) {
    text(x, y, labels = labels, col = label.color, family = label.family, 
         font = label.font, cex = label.cex)
  }
  else {
    if1 <- function(vect, idx) if (length(vect) == 1) 
      vect
    else vect[idx]
    sapply(seq_len(vcount(graph)), function(v) {
      text(x[v], y[v], labels = if1(labels, v), col = if1(label.color, 
                                                          v), family = if1(label.family, v), font = if1(label.font, 
                                                                                                        v), cex = if1(label.cex, v))
    })
  }
  rm(x, y)
  invisible(NULL)
}

igraph.Arrows2=function (x1, y1, x2, y2, code = 2, size = 1, width = 1.2/4/cin, 
                         open = TRUE, sh.adj = 0.1, sh.lwd = 1, sh.col = if (is.R()) par("fg") else 1, 
                         sh.lty = 1, h.col = sh.col, h.col.bo = sh.col, h.lwd = sh.lwd, 
                         h.lty = sh.lty, curved = FALSE) 
{
  cin <- size * par("cin")[2]
  
  lx <- length(x1)
  
  uin <- if (is.R()) 
    1/xyinch()
  else par("uin")
  
  delta <- sqrt(h.lwd) * par("cin")[2] * 0.005
  
  #modify for multiple sizes here
  arrlist=lapply(1:length(size),function(w){
    x <- sqrt(seq(0, cin[w]^2, length = floor(35 * cin[w]) + 2))
    x.arr <- c(-rev(x), -x)
    return(list(x=x,x.arr=x.arr))
  })
  x=lapply(arrlist,function(w) w$x)
  x.arr=lapply(arrlist,function(w) w$x.arr)
  #pad size to same length as edges
  wx=lx/length(x)
  if(wx>1){
    x=rep(x,ceiling(wx))
    x.arr=rep(x.arr,ceiling(wx))
    cin=rep(cin,ceiling(wx))
  }
  wx=lx/length(width)
  if(wx>1){
    width=rep(width,ceiling(wx))
  }
  width <- width * (1.2/4/cin)
  
  #modify for multiple widths here
  arrlist=lapply(1:length(width),function(w){
    
    wx2<-width[w]*x[[w]]^2
    y.arr <- c(-rev(wx2 + delta), wx2 + delta)#repeat it backwards
    deg.arr <- c(atan2(y.arr, x.arr[[w]]), NA)#atan2 of y array and x array
    r.arr <- c(sqrt(x.arr[[w]]^2 + y.arr^2), NA) #square root of x array and y array
    return(list(deg.arr=deg.arr,r.arr=r.arr))
  })
  deg.arr=do.call(c,lapply(arrlist,function(w) w$deg.arr))
  r.arr=do.call(c,lapply(arrlist,function(w) w$r.arr))
  deg.arr2=lapply(arrlist,function(w) w$deg.arr)
  
  bx1 <- x1
  bx2 <- x2
  by1 <- y1
  by2 <- y2
  
  #modify for multiple arrow sizes
  
  if(length(cin)==1){
    r.seg <- rep(cin * sh.adj, lx)
    theta1 <- atan2((y1 - y2) * uin[2], (x1 - x2) * uin[1])
    th.seg1 <- theta1 + rep(atan2(0, -cin), lx)
    theta2 <- atan2((y2 - y1) * uin[2], (x2 - x1) * uin[1])
    th.seg2 <- theta2 + rep(atan2(0, -cin), lx)
  }else{
    r.seg <- cin * sh.adj
    theta1 <- atan2((y1 - y2) * uin[2], (x1 - x2) * uin[1])
    th.seg1 <- theta1 + (atan2(0, -cin))
    theta2 <- atan2((y2 - y1) * uin[2], (x2 - x1) * uin[1])
    th.seg2 <- theta2 + (atan2(0, -cin))		
  }
  
  
  x1d <- y1d <- x2d <- y2d <- 0
  if (code %in% c(1, 3)) {
    x2d <- r.seg * cos(th.seg2)/uin[1]
    y2d <- r.seg * sin(th.seg2)/uin[2]
  }
  if (code %in% c(2, 3)) {
    x1d <- r.seg * cos(th.seg1)/uin[1]
    y1d <- r.seg * sin(th.seg1)/uin[2]
  }
  if (is.logical(curved) && all(!curved) || is.numeric(curved) && 
      all(!curved)) {
    segments(x1 + x1d, y1 + y1d, x2 + x2d, y2 + y2d, lwd = sh.lwd, 
             col = sh.col, lty = sh.lty)
    phi <- atan2(y1 - y2, x1 - x2)
    r <- sqrt((x1 - x2)^2 + (y1 - y2)^2)
    lc.x <- x2 + 2/3 * r * cos(phi)
    lc.y <- y2 + 2/3 * r * sin(phi)
  }
  else {
    if (is.numeric(curved)) {
      lambda <- curved
    }
    else {
      lambda <- as.logical(curved) * 0.5
    }
    lambda <- rep(lambda, length.out = length(x1))
    c.x1 <- x1 + x1d
    c.y1 <- y1 + y1d
    c.x2 <- x2 + x2d
    c.y2 <- y2 + y2d
    midx <- (x1 + x2)/2
    midy <- (y1 + y2)/2
    spx <- midx - lambda * 1/2 * (c.y2 - c.y1)
    spy <- midy + lambda * 1/2 * (c.x2 - c.x1)
    sh.col <- rep(sh.col, length = length(c.x1))
    sh.lty <- rep(sh.lty, length = length(c.x1))
    sh.lwd <- rep(sh.lwd, length = length(c.x1))
    lc.x <- lc.y <- numeric(length(c.x1))
    for (i in seq_len(length(c.x1))) {
      if (lambda[i] == 0) {
        segments(c.x1[i], c.y1[i], c.x2[i], c.y2[i], 
                 lwd = sh.lwd[i], col = sh.col[i], lty = sh.lty[i])
        phi <- atan2(y1[i] - y2[i], x1[i] - x2[i])
        r <- sqrt((x1[i] - x2[i])^2 + (y1[i] - y2[i])^2)
        lc.x[i] <- x2[i] + 2/3 * r * cos(phi)
        lc.y[i] <- y2[i] + 2/3 * r * sin(phi)
      }
      else {
        spl <- xspline(x = c(c.x1[i], spx[i], c.x2[i]), 
                       y = c(c.y1[i], spy[i], c.y2[i]), shape = 1, 
                       draw = FALSE)
        lines(spl, lwd = sh.lwd[i], col = sh.col[i], 
              lty = sh.lty[i])
        if (code %in% c(2, 3)) {
          x1[i] <- spl$x[3 * length(spl$x)/4]
          y1[i] <- spl$y[3 * length(spl$y)/4]
        }
        if (code %in% c(1, 3)) {
          x2[i] <- spl$x[length(spl$x)/4]
          y2[i] <- spl$y[length(spl$y)/4]
        }
        lc.x[i] <- spl$x[2/3 * length(spl$x)]
        lc.y[i] <- spl$y[2/3 * length(spl$y)]
      }
    }
  }
  if (code %in% c(2, 3)) {
    theta <- atan2((by2 - y1) * uin[2], (bx2 - x1) * uin[1])
    
    #alter here for multiple arrow widths/size
    if(length(width)==1&length(size)==1){
      Rep <- rep(length(deg.arr), lx)
    } else {
      Rep <- sapply(deg.arr2,length)
    }
    p.x2 <- rep(bx2, Rep)
    p.y2 <- rep(by2, Rep)
    if(length(width)==1&length(size)==1){
      ttheta <- rep(theta, Rep) + rep(deg.arr, lx)
      r.arr <- rep(r.arr, lx) 
    } else {#repping not neccesary
      ttheta <- rep(theta, Rep) + deg.arr
    }
    if (open) 
      lines((p.x2 + r.arr * cos(ttheta)/uin[1]), (p.y2 + 
                                                    r.arr * sin(ttheta)/uin[2]), lwd = h.lwd, col = h.col.bo, 
            lty = h.lty)
    else polygon(p.x2 + r.arr * cos(ttheta)/uin[1], p.y2 + 
                   r.arr * sin(ttheta)/uin[2], col = h.col, lwd = h.lwd, 
                 border = h.col.bo, lty = h.lty)
  }
  if (code %in% c(1, 3)) {
    x1 <- bx1
    y1 <- by1
    tmp <- x1
    x1 <- x2
    x2 <- tmp
    tmp <- y1
    y1 <- y2
    y2 <- tmp
    theta <- atan2((y2 - y1) * uin[2], (x2 - x1) * uin[1])
    lx <- length(x1)
    Rep <- rep(length(deg.arr), lx)
    p.x2 <- rep(x2, Rep)
    p.y2 <- rep(y2, Rep)
    ttheta <- rep(theta, Rep) + rep(deg.arr, lx)
    r.arr <- rep(r.arr, lx)
    if (open) 
      lines((p.x2 + r.arr * cos(ttheta)/uin[1]), (p.y2 + 
                                                    r.arr * sin(ttheta)/uin[2]), lwd = h.lwd, col = h.col.bo, 
            lty = h.lty)
    else polygon(p.x2 + r.arr * cos(ttheta)/uin[1], p.y2 + 
                   r.arr * sin(ttheta)/uin[2], col = h.col, lwd = h.lwd, 
                 border = h.col.bo, lty = h.lty)
  }
  list(lab.x = lc.x, lab.y = lc.y)
}