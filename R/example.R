#### start ####

#load libraries
library(png)

#set working directory to this repo
setwd("~/repos/text-in-img/")

#read in functions
source("R/functions.R")

#specify high-level parameters
n_rounds <- 5
cex_history <- matrix(NA, nrow = n_rounds, ncol= 2)
flanking <- F
wiggle <- F
background_cols <- c("black","white", "grey50")
render_image <- T
adjust_ws_kerning <- T
adjust_colors <- F

#get text
txt_name <- c("lorem-ipsum", "alice_chapter-1", "alice_full-text")[2]
path_to_txt <- paste0("input/txt/", txt_name, ".txt")
txt <- readLines(path_to_txt, warn = F)

#get image
img_name <- c("alice-in-wonderland")
img_name <- img_name[1]
path_to_image <- paste0("input/img/", img_name, ".png")
img <- readPNG(source = path_to_image)

####

#resize image for easier processing
# img <- scale_img(300, img)
# 


# col_res = 0.01; kde_thresh = 0.95;
# PCA_thresh = 0.95; ncol_range = 3:10; grid_n = 50; use_kd_for_cluster = F;
# n_top = 100; offset_k = 0; transparency_thresh = 0.9; use_HSL = F;
# use_Lab = F; inverse_normal = F; weight_pow = 0.5;
# 
# ncol_range; n_passes = 0; method = "tSNE"; col_res = 0.01; offset_k = 0;
# kde_thresh = 0.95; use_kd_for_cluster = T;
# transparency_thresh = 0.99; use_HSL = F;
# PCA_thresh = 0.95; use_Lab = F; weight_pow = 0.5; dr_method = "tSNE"; inverse_normal = F
# 
# n_passes = 0; n_top = 100;
# kde_thresh = 0.95; use_kd_for_cluster = T;
# transparency_thresh = 0.99; use_HSL = F;
# PCA_thresh = 0.95; use_Lab = F; weight_pow = 0.5; dr_method = "tSNE"

# ncol_range = 10; n_passes = 0; col_res = 0.05;
# kde_thresh = 0.99; use_kd_for_cluster = F;
# transparency_thresh = 0.99; use_HSL = F; n_top = 200;
# PCA_thresh = 0.95; use_Lab = F; weight_pow = 0.75; dr_method = c("tSNE")

#perform color clustering on image
ncol_range <- 15
pimg <- preprocess_image(img, ncol_range, n_passes = 0, col_res = 0.02,
                        kde_thresh = 0.99, use_kd_for_cluster = F,
                        transparency_thresh = 0.99, use_HSL = F, n_top = 200,
                        PCA_thresh = 0.95, use_Lab = T, weight_pow = 0.75, dr_method = c("tSNE"), 
                        min_dist_q = 0.1, dist_w = 0)

#rotate image
rotation_angle <- 0
pimg <- abind::abind(apply(pimg, 3, rotate_grid, theta = -rotation_angle, simplify = F), along = 3)

#compute matrix for pixel locations
hits_indlocs <- fill_hits_matrix(pimg)
hits <- hits_indlocs$hits
indlocs <- hits_indlocs$indlocs
uniq_indlocsy <- hits_indlocs$uniq_indlocsy
pxlw <- hits_indlocs$pxlw

#see if we have preprocessed correctly
png::writePNG(pimg, target = paste0("output/img/", img_name, "_processed-img.png"))
png(paste0("output/img/", img_name, "_test-indlocs.png"), width = ncol(hits) * 5, height = nrow(hits) * 5, units = "px")
par(mar = c(0,0,0,0))
plot(NA, NA, xlim = c(0,1), ylim = c(0,1), frame.plot = F, xaxt = "n", yaxt = "n", xlab = "", ylab = "")
rect(-1,-1,3,3,col = "grey20", xpd = NA)
points(indlocs$x, indlocs$y, col = indlocs$col, pch = 15, cex = 0.5)
dev.off()

#### process text ####
txt <- paste0(txt, collapse = " ")
txt <- gsub(txt, pattern = "\t", replacement = " ")
txt <- gsub(txt, pattern = "\\s+", replacement = " ")
txt <- strsplit(txt, "")[[1]]
max_ind <- length(txt)

#check text for illegal characters & evaluate fill proportion
font = 2
family = "Arial"
uniqchars <- unique(txt)
w2h <- c(strwidth(paste0(uniqchars, collapse = ""), font = font, family = family, units = "inches"),
         strheight(paste0(uniqchars, collapse = ""), font = font, family = family, units = "inches"))
w2h <- w2h / min(w2h) * 100
png(paste0("~/uniqchars-", txt_name,".png"), width = w2h[1]*0.5, height = w2h[2], units = "px")
par(mar = c(0,0,0,0))
plot(NA, NA, xlim = c(0,w2h[1] / 100), ylim = c(-0.5,1.5), frame.plot = F, xaxt = "n", yaxt = "n", xlab = "", ylab = "")
uniqcharcex <- 1 / strheight(uniqchars[1], units = "user", font = font, family = family)
uniqcharwidths <- strwidth(uniqchars, units = "user", cex = uniqcharcex, font = font, family = family)
cs_uniqcharwidths <- cumsum(c(0,uniqcharwidths))

for(i in 1:length(uniqchars)){
  text(labels = uniqchars[i], x = cs_uniqcharwidths[i], y = 0.5,
       col = 1, cex = uniqcharcex, font = font, pos = 4, family = family)
  rect(xleft = cs_uniqcharwidths[i], ybottom = 0, ytop = 1, xright = cs_uniqcharwidths[i] + uniqcharwidths[i], border = 1)
}
dev.off()

uniqchars <- unique(txt)


#### start preprocessing drawing ####
npixels <-c(nrow(hits) * 5, ncol(hits) * 5)
# npixels <- c(12, 12) * 300
png(paste0("output/img/", img_name, "_", txt_name, "_textfill_bcol-1.png"), width = npixels[2], height = npixels[1], units = "px")
par(mar = c(0,0,0,0))
plot(NA, NA, xlim = c(0,1), ylim = c(0,1), frame.plot = F, xaxt = "n", yaxt = "n", xlab = "", ylab = "")


#initialize parameters
paintable_area <- sum(hits) / length(hits)
charh_multiplier <- 1.21
txta <- strwidth(paste0(txt, collapse = ""), units = "user", font = font, family = family) *
  strheight(paste0(txt, collapse = ""), units = "user", font = font, family = family) * charh_multiplier
cex <- sqrt(paintable_area / txta)
prev_subline_i <- 1

#start iterative optimization of cex
for(round in 1:n_rounds){

  toolong <- F
  cat(paste0("\n", round, ": "))
  charwidths <- setNames(strwidth(uniqchars, units = "user", cex = cex, font = font, family = family), uniqchars)
  charheights <- setNames(strheight(uniqchars, units = "user", cex = cex, font = font, family = family) * charh_multiplier, uniqchars)
  charheight <- as.numeric(charheights[1])
  txtw <- cumsum(charwidths[txt])

  #figure out charater locations
  ind_indlocs <- 1
  # ind_indlocs <- min(which(indlocs$tw > 0.5)) #if you want to start on a more encompassing line
  charlocx <- indlocs$x[ind_indlocs]
  charlocy <- indlocs$y[ind_indlocs]
  tw <- indlocs$tw[ind_indlocs]
  tsw <- indlocs$tsw[ind_indlocs]
  twtd <- 0
  tswtd <- 0
  charmat <- matrix(NA, ncol = 6, nrow = length(txt))
  charmat <- as.data.frame(charmat)
  colnames(charmat) <- c("x", "y", "col", "line", "block", "char")
  charmat$char <- txt
  line_i <- 1
  curr_block <- 1
  go_to_next_line <- F

  for(i in 1:max_ind){
    if(i %% round(length(txt) / 10) == 0) cat(i / round(length(txt) / 10) * 10, " ")

    charmat[i,1:5] <- c(charlocx, charlocy, NA, line_i, curr_block)
    charlocx <- charlocx + charwidths[txt[i]]

    twtd <- twtd + charwidths[txt[i]]
    tswtd <- tswtd + charwidths[txt[i]]
    # print(paste0(i, ": ", paste0(charmat[i,], collapse =" ")))
    if((tswtd > tsw) & !(twtd > tw)){
      twtd <- twtd + tsw - tswtd #to not falsely accumulate extra subwidths when they are small

      closest_indlocy <- indlocs$y[which.min(abs(charlocy - indlocs$y))]
      valid_indlocs <- indlocs[which(abs(closest_indlocy - indlocs$y) < 1E-9),]
      # valid_indlocs <- indlocs[which(abs(charlocy - indlocs$y) < 1E-9),]
      if(min(which(charlocx < valid_indlocs$x)) == Inf){
        go_to_next_line <- T
        # print("inf val"); break
      } else {
        block_inds <- which(charmat$block == curr_block & charmat$line == line_i)
        # new_ind_indlocs <- which(charmat[,5] == (curr_block+1) & charmat[,4] == line_i)
        new_ind_indlocs <- min(which((charlocx + 0.1*pxlw) < valid_indlocs$x)) #machine precision hack
        charlocx <- valid_indlocs$x[new_ind_indlocs]

        #spread whitespace so that you're on the edges of the current area
        if(round == n_rounds & adjust_ws_kerning){
          block_center <- charmat[block_inds[1],1] + tsw / 2

          if(length(block_inds) > 1){
            if(txt[max(block_inds)] == " "){
              # charmat[max(block_inds),1] <- charmat[max(block_inds)-1,1]
              block_inds <- block_inds[-length(block_inds)]
            }
            if(txt[min(block_inds)] == " "){
              # charmat[min(block_inds),1] <- charmat[min(block_inds)+1,1]
              block_inds <- block_inds[-1]
            }
            if(sum(txt[block_inds] != " ") == 1){
              # charmat[block_inds,1] <- charmat[block_inds,1] + (tsw - tswtd) / 2 #centers single character
              non_ws_ind <- block_inds[which(txt[block_inds] != " ")]
              charmat[non_ws_ind,1] <- block_center - charwidths[txt[non_ws_ind]] / 1.5 #to accommodate pos = 4
            } else {
              missing_space <- tsw - (tswtd)
              which_ws <- which(txt[block_inds] == " ")
              per_ws_incr <- missing_space / length(which_ws)
              per_ws_incr <- max(per_ws_incr, -charwidths[" "] / 2) #never remove more than half a space
              incr_vec <- rep(0, length(block_inds))
              incr_vec[which_ws] <- per_ws_incr
              incr_vec <- cumsum(incr_vec)

              #not centering characters properly -- find block center and char center and recenter chars in block
              char_center <- charmat[block_inds[1],1] + (diff(range(charmat[block_inds,1] + incr_vec)) + charwidths[charmat$char[block_inds[length(block_inds)]]]) / 2
              center_disp <- block_center - char_center

              #finally, apply the adjustment
              charmat[block_inds,1] <- charmat[block_inds,1] + incr_vec + center_disp
            }
          }
        }

        prev_subline_i <- i + 1
        curr_block <- curr_block + 1
        tswtd <- 0
        tsw <- valid_indlocs$tsw[new_ind_indlocs]

      }
    }
    if(twtd > tw | go_to_next_line){
      go_to_next_line <- F
      block_inds <- which(charmat$block == curr_block & charmat$line == line_i)

      #spread whitespace so that you're on the edges of the current area
      if(round == n_rounds & adjust_ws_kerning){
        block_center <- charmat[block_inds[1],1] + tsw / 2

        if(length(block_inds) > 1){
          if(txt[max(block_inds)] == " "){
            # charmat[max(block_inds),1] <- charmat[max(block_inds)-1,1]
            block_inds <- block_inds[-length(block_inds)]
          }
          if(txt[min(block_inds)] == " "){
            # charmat[min(block_inds),1] <- charmat[min(block_inds)+1,1]
            block_inds <- block_inds[-1]
          }
          if(sum(txt[block_inds] != " ") == 1){
            # charmat[block_inds,1] <- charmat[block_inds,1] + (tsw - tswtd) / 2 #centers single character
            non_ws_ind <- block_inds[which(txt[block_inds] != " ")]
            charmat[non_ws_ind,1] <- block_center - charwidths[txt[non_ws_ind]] / 1.5 #to accommodate pos = 4
          } else {
            missing_space <- tsw - (tswtd)
            which_ws <- which(txt[block_inds] == " ")
            per_ws_incr <- missing_space / length(which_ws)
            per_ws_incr <- max(per_ws_incr, -charwidths[" "] / 2) #never remove more than half a space
            incr_vec <- rep(0, length(block_inds))
            incr_vec[which_ws] <- per_ws_incr
            incr_vec <- cumsum(incr_vec)

            #not centering characters properly -- find block center and char center and recenter chars in block
            char_center <- charmat[block_inds[1],1] + (diff(range(charmat[block_inds,1] + incr_vec)) +
                                                         charwidths[charmat$char[block_inds[length(block_inds)]]]) / 2
            center_disp <- block_center - char_center

            #finally, apply the adjustment
            charmat[block_inds,1] <- charmat[block_inds,1] + incr_vec + center_disp
          }
        }
      }


      twtd <- 0
      tswtd <- 0
      curr_block <- 1
      line_i <- line_i + 1

      #problem --  we increment by strheight and then find the closest valid line for info about width
      #but if there are not pixels there, we still find the closest line! need to skip ahead to the next closest
      #so need to check if there are indlocs with height between old and new charlocy
      #and if not, skip it down to the next available indloc
      next_indlocy_down <- max(uniq_indlocsy[uniq_indlocsy < (indlocs$y[ind_indlocs] - charheight / 100)])
      prop_charlocy <- charlocy - charheight
      charlocy <- min(prop_charlocy, next_indlocy_down)
      ind_indlocs <- which.min(abs(charlocy - indlocs$y))
      tw <- indlocs$tw[ind_indlocs]
      tsw <- indlocs$tsw[ind_indlocs]
      charlocx <- indlocs$x[ind_indlocs]
      if(charlocy > min(indlocs$y)){
        # charlocy <- indlocs$y[ind_indlocs] #this is why I am getting final line palimpset
      } else {
        toolong <- T
        got_through_prop <- i / length(txt)
        cat(paste0("ending at ", round(got_through_prop, 3) * 100, "% "))
        break
      }
    }

    #end of character loop
    # charmat[i,1:5] <- c(charlocx, charlocy, NA, line_i, curr_block)
    # charlocx <- charlocx + charwidths[txt[i]]

  }

  #find relation between last character and last pixel
  closest_ind <- which.min(apply(t(t(indlocs[,1:2]) - as.numeric(tail(charmat, 1)[1:2])), 1, pythag))
  prop_through <- closest_ind / nrow(indlocs)

  #adjust cex
  newcexprop <- 0.5
  if(!toolong){
    newcex <- cex / prop_through^0.5 + cex / 400
    print(newcex / cex)
    cex_history[round,] <- c(cex, newcex / cex)
  } else {
    newcex <- cex * got_through_prop^0.5 - cex / 400
    print(newcex / cex)
    cex_history[round,] <- c(cex, newcex / cex)
  }

  flanking <- (!all(cex_history[1:round,2] > 1) & !all(cex_history[1:round,2] < 1)) & round != 1

  if(!flanking) {
    cex <- newcex * newcexprop + cex * (1-newcexprop)
  } else {
    curr_cex_history <- cex_history[1:round,]
    cvs <- curr_cex_history[,1]
    rats <- curr_cex_history[,2]
    #too much text
    closest_below <- min(cvs[which(rats < 1)])
    #too little text
    closest_above <- max(cvs[which(rats > 1)])
    # cex_weights <- (1 / abs(1-cex_history[1:round,2]))^0.5
    # cex_weights <- cex_weights / sum(cex_weights)
    # cex <- sum(cex_history[1:round,1] * cex_weights) * 0.9 + propcex * 0.1
    if(round != n_rounds){
      cex <- (closest_above * 0.5 + closest_below * 0.5)
      # print(paste0("above: ", closest_above, ",\nbelow: ", closest_below, ",\nnew cex: ", cex))
      # cex <- cex * ifelse(abs(rats[round] - rats[round-1]) < 1E-4 & wiggle, exp(rnorm(1, 0, 0.01)), 1)
    }
    if(round == (n_rounds-1)){
        cex <- curr_cex_history[which.min(abs(curr_cex_history[,2]-1)),1]

        #get just the ones where all the text is there
        cvs <- curr_cex_history[,1]
        rats <- curr_cex_history[,2]
        closest_above <- which(rats > 1)
        cex <- cvs[closest_above[which.min(rats[closest_above])]]
    }
  }


}

dev.off()

#current error -- can't skip lines!

#figure out color info
nlines <- length(unique(charmat[,4]))
rlelines <- rle(charmat[,4])
line_inds <- cumsum(c(0,rlelines$lengths))
background_col_rgb <- t(col2rgb(background_cols[1]))
for(i in 1:nlines){
  cmatinds <- (line_inds[i]+1):(line_inds[i+1])
  subcm <- charmat[cmatinds,]
  yloc <- as.numeric(subcm[1,2])
  closest_indlocy <- indlocs$y[which.min(abs(yloc - indlocs$y))]
  subindlocs <- indlocs[abs(indlocs$y - closest_indlocy) < 1E-9,]
  xlocs <- subcm$x + charwidths[subcm$char] / 2

  #this finds the closest color
  nearest_matches <- findInterval(xlocs, c(-Inf, subindlocs$x[-1]-diff(subindlocs$x)/2, Inf))
  native_cols <- subindlocs$col[nearest_matches]
  
  #this finds the modal color in row
  match_inds <- c(0, nearest_matches[-length(nearest_matches)] + floor(diff(nearest_matches) / 2), length(subindlocs$x))
  native_cols <- sapply(seq_along(match_inds)[-1], function(j){
    silcols <- subindlocs$col[(match_inds[j-1]+1):(match_inds[j])]
    tsilcols <- table(silcols)
    names(tsilcols)[which.max(tsilcols)]
  })
  

  if(adjust_colors){
    charprops <- propspace_uniqchar[charmat$char[cmatinds]]^0.5
    native_col_rgb <- t(col2rgb(native_cols))
    needed_cols <- data.frame(red = (native_col_rgb[,1] -  background_col_rgb[1] * (1-charprops)) / charprops,
                         blue = (native_col_rgb[,2] -  background_col_rgb[2] * (1-charprops)) / charprops,
                         green = (native_col_rgb[,3] -  background_col_rgb[3] * (1-charprops)) / charprops)
    needed_cols <- needed_cols / 255
    needed_cols <- needed_cols / apply(cbind(needed_cols, 1), 1, max)
    charmat[cmatinds,3] <- sapply(1:nrow(needed_cols), function(coli) ifelse(!any(is.na(needed_cols[coli,])),
                                                      rgb(needed_cols$red[coli], needed_cols$blue[coli], needed_cols$green[coli], alpha = 1),
                                                      NA))

  } else {
    charmat[cmatinds,3] <- native_cols
  }


}

#### draw the characters ####

if(abs(rotation_angle) > 1E-3){
  rotmat <- rotmat_00(rotation_angle)
  new_pixcoords <- t(rotmat %*% t(cbind(charmat$x - 0.5, charmat$y - 0.5)))
  charmat$x <- new_pixcoords[,1] + 0.5
  charmat$y <- new_pixcoords[,2] + 0.5
}


for(bcoli in 1:length(background_cols)){
  if(render_image){
    png(paste0("output/img/", img_name, "_", txt_name, "_textfill_bcol-", bcoli, ".png"), width = npixels[2], height = npixels[1], units = "px")
    par(mar = c(0,0,0,0))
    plot(NA, NA, xlim = c(0,1), ylim = c(0,1), frame.plot = F, xaxt = "n", yaxt = "n", xlab = "", ylab = "")
    rect(-10, -10, 10, 10, col = background_cols[bcoli], border = background_cols[bcoli], xpd = NA)

    for(i in (1:max_ind)){
      if(i %% round(length(txt) / 10) == 0) cat(i / round(length(txt) / 10) * 10, " ")
      text(labels = charmat$char[i], x = charmat$x[i] - ifelse(txt[i] == "Rethink Priorities", 0.02, 0), y = charmat$y[i],
           col = charmat$col[i], cex = cex, font = font, pos = 4, family = family, srt = rotation_angle)
      # abline(h = charmat$y[i] - charheight / 4, lwd = 0.01)

    }
  }
  dev.off()
}

