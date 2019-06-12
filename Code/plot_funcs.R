### Plotting functions for travel time plots ###

plot_tt <- function(infected_sf, access_raster, bg_adm0, bg_adm1, lakes){

  # Convert raster to data frame, minutes -> hours and cap at 12 hours
  acc_df <- na.omit(as.data.frame(access_raster, xy=TRUE))
  names(acc_df)[3] <- 'layer'
  acc_df$layer[which(acc_df$layer > 12)] <- 12

  as_df <- fortify(infected_sf)
  bg_adm0 <- fortify(bg_adm0)
  bg_adm1 <- fortify(bg_adm1)
  lakes <- lakes[which(lakes$SQKM > 100),]

  df_labels <- data.table(rbind(c('Democratic Republic\nof the Congo', 27, 1.5),
                                c('Uganda', 31, 0),
                                c('Rwanda', 30, -2),
                                c('Burundi', 30, -3),
                                c('Tanzania', 31.3, -1.4)))
  names(df_labels) <- c('country', 'x', 'y')
  df_labels[, x := as.numeric(x)]
  df_labels[, y := as.numeric(y)]



  ttmap <-
    ggplot() +
    geom_raster(data = acc_df, aes(x=x, y=y, fill = layer)) +
    geom_polygon(data = bg_adm1, aes(x = long, y = lat, group = group), fill = 'transparent', color = 'gray33', size = .15) +
    geom_polygon(data = bg_adm0, aes(x = long, y = lat, group = group), fill = 'transparent', color = 'gray47', size = .75) +
    geom_polygon(data = as_df, aes(x=long, y = lat, group=group, colour = 'Affected health\nareas'), fill = 'transparent')+
    geom_sf(data = lakes, fill = 'lightblue', color = 'darkblue', size = .15) +
    scale_fill_viridis(name  = 'Travel time to\nnearest case\nof Ebola (hours)\n'  ,option = 'magma', direction = -1,
                       labels = c('0', '3','6', '9', '>=12'), breaks = c(0,3,6,9,12)) +
    scale_color_manual(name = NULL, values = c('Affected health\nareas' = 'black')) +
    coord_sf(xlim = c(25, 33), ylim = c(-3.3, 3), expand = FALSE) +
    geom_label(data = df_labels, aes(x=x, y = y, label = country), fill='grey', alpha = 0.4, fontface = 'bold') +
    theme(axis.text = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          legend.background=element_blank(),
          legend.key = element_blank(),
          legend.text = element_text(size = 10),
          legend.title = element_text(size = 12),
          plot.caption = element_text(hjust = 0, size = 7),
          plot.title = element_text(face = 'bold', size = 14),
          panel.background = element_blank())


  return(ttmap)

}

plot_rel_map <- function(infected_sf, access_raster, bg_adm0, lakes, co){


  if(co == 'RWA'){
    df_labels <- data.table(rbind(c('Democratic Republic\nof the Congo', 29, -1.25),
                     c('Uganda', 30, -1),
                     c('Rwanda', 30, -1.7),
                     c('Burundi', 30.15, -2.6)))

    names(df_labels) <- c('country', 'x', 'y')
    df_labels[, x := as.numeric(x)]
    df_labels[, y := as.numeric(y)]

    xlims <- c(28.5, 31)
    ylims <- c(-3, -0.8)

  } else if(co == 'UGA'){
    df_labels <- data.table(rbind(c('Democratic Republic\nof the Congo', 29.9, 2.15),
                                  c('Uganda', 32, 1.2),
                                  c('Tanzania', 31.3, -1.35)))

    names(df_labels) <- c('country', 'x', 'y')
    df_labels[, x := as.numeric(x)]
    df_labels[, y := as.numeric(y)]

    xlims <- c(29, 35.5)
    ylims <- c(-1.8, 4.35)

  }

  co_shp <- bg_adm0[which(bg_adm0@data$iso == co),]
  access_raster <- mask(access_raster, co_shp)
  acc_df <- na.omit(as.data.frame(access_raster, xy = TRUE))


  as_df <- fortify(infected_sf)
  bg_adm0 <- fortify(bg_adm0)
  lakes <- lakes[which(lakes$SQKM > 100),]
  names(acc_df)[3] <- 'layer'

  quantile.interval <- quantile(acc_df$layer, probs=seq(0, 1, by = 1/5))
  acc_df$quant <- cut(acc_df$layer, breaks=quantile.interval, include.lowest = TRUE)


  rel_map <-
    ggplot() +
    geom_polygon(data = as_df, aes(x=long, y = lat, group=group, colour = 'Affected health\nareas'), fill = 'transparent', show.legend = TRUE)+
    geom_raster(data = acc_df, aes(x=x, y=y, fill = quant), show.legend = TRUE) +
    geom_polygon(data = bg_adm0, aes(x=long, y=lat, group=group), fill = 'transparent', color = 'gray33', size = 1) +
    geom_sf(data = lakes, fill = 'lightblue', color = 'darkblue', size = .05) +
    scale_fill_manual(name = 'Quintile of Travel Time\nto Nearest Case of Ebola',
                      values = c('#F3E51E', '#44BE70', '#218C8D', '#3A538B', '#440357'),
                      labels = rev(c('80 - 100', '60 - 80' , '40 - 60','20 - 40' , '0 - 20'))) +
    scale_color_manual(name = NULL, values = c('Affected health\nareas' = 'black')) +
    guides(fill = guide_legend( order = 1), color = guide_legend(order = 2)) +
    coord_sf(xlim = xlims, ylim = ylims, expand = FALSE) +
    geom_label(data = df_labels, aes(x=x, y = y, label = country), fill='grey', alpha = 0.4, fontface = 'bold', size = 3) +
    theme(axis.text = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          legend.key = element_blank(),
          legend.text = element_text(size = 10),
          legend.title = element_text(size = 12),
          plot.caption = element_text(hjust = 0, size = 7),
          plot.title = element_text(face = 'bold', size = 14, hjust = 0),
          panel.background = element_blank(),
          panel.grid = element_line(color = 'white'),
          panel.border = element_rect(fill = 'transparent', color = 'black'))

  return(rel_map)

}
