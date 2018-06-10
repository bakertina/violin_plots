split_violin <- function(data_new_cleaned, split_col, group_col, labels=NULL, xlab = "X_",
                          ylab = "Y_", nums_y =NULL, levels =NULL, levels_split=NULL,
                          theme=FALSE, colours , position = "bottom", 
                          include_counts = TRUE, main = "", base_size = 12, base_family =NULL, 
                          ylim =NULL, xvalue){
  require(ggplot2)
  require(dplyr)
  require(broom)
  
  colnames(data_new_cleaned) <- gsub(xvalue, "x_value", x = colnames(data_new_cleaned))
  theme.size = base_size
  geom.text.size = theme.size / 14 * 5
  # geom txt size comes up bigger so use this conversion to keep ratio which is useful in some plots
  colnames(data_new_cleaned)[colnames(data_new_cleaned) == split_col] <-
    "split_col_"
  colnames(data_new_cleaned)[colnames(data_new_cleaned) == group_col] <-
    "inter_"
  if (is.null(levels)) {colours =c("#0057e7", "#d62d20")}
  if (is.null(nums_y)) {nums_y =max(data_new_cleaned$x_value)*1.2}
  if (!is.null(levels)) {
    data_new_cleaned$inter_ <-
      factor(data_new_cleaned$inter_, levels = levels)
  }
  if (!is.null(levels_split)) {
    data_new_cleaned$split_col_ <-
      factor(data_new_cleaned$split_col_, levels = levels_split)
  }
  ##############################################################################
  #http://stackoverflow.com/questions/35732845/split-violin-plot-with-quartiles
  # The split plots are done by calculating the densities yourself beforehand, and then plotting polygons. 

  pdat = data_new_cleaned %>%
    group_by(inter_, split_col_) %>%
    do(tidy(density(.[['x_value']]))) %>%
    rename(loc = x, dens = y) %>%
    mutate(dens = 0.5 * dens / max(dens)) %>%
    ungroup()
  #     Calculate summary statistics in a separate dataframe;
  # sums -  quants and mean
  sums = data_new_cleaned %>%
    group_by(inter_, split_col_) %>%
    summarise(
      lquart_loc = quantile(x_value)[[2]],
      uquart_loc = quantile(x_value)[[4]],
      med = mean(x_value)
    ) %>%
    ungroup() %>%
    gather(segment, loc_sum, lquart_loc, uquart_loc, med)
  # sums2 -  indervidual lines
  data_new_cleaned %>%
    select(inter_, split_col_, x_value) -> sums2
  #     Calculate the corresponding points on each group's density curve
  # left join sums
  sums = left_join(pdat, sums, by = c('inter_', 'split_col_')) %>%
    group_by(inter_, split_col_) %>%
    do(data.frame(
      loc = unique(.$loc_sum),
      dens = approx(.$loc, .$dens, unique(.$loc_sum))[['y']]
    )) %>%
    ungroup()
  # Left join suns2
  sums2 = left_join(pdat, sums2, by = c('inter_', 'split_col_')) %>%
    group_by(inter_, split_col_) %>%
    do(data.frame(
      loc = unique(.$x_value),
      dens = approx(.$loc, .$dens, unique(.$x_value))[['y']]
    )) %>%
    ungroup()
  #     Change the number of offsest for the data
  offsets = unique(data_new_cleaned[['inter_']]) %>% {
    setNames(0:(length(.) - 1), .)
  } #number of groups
  splits = unique(data_new_cleaned[['split_col_']]) # what value to split on
  
  #     Offset the densities and summary statistics
  # mutate pdat with dens
  pdat = pdat %>%
    mutate(offset_dens = offsets[.[['inter_']]] + ifelse(.[['split_col_']] == splits[1], -dens, dens))
  # mutate sums with dens
  sums = sums %>%
    mutate(offset = offsets[.[['inter_']]],
           offset_dens = offset + ifelse(.[['split_col_']] == splits[1], -dens, dens)) ## Q1,Q3 MEAN
  # ### SUMS MEAN
  a <- 1:nrow(sums); b <- a[seq(3, length(a), 3)] # multiples of 3
  droplevels(sums) -> sums
  sums[b,] %>% droplevels() -> sums_mean # select the means
  
  # mutate summs2 with dens
  sums2 = sums2 %>%
    mutate(offset = offsets[.[['inter_']]],
           offset_dens = offset + ifelse(.[['split_col_']] == splits[1], -0.1, +0.1)) # change the +/- 0.01 for making lines wider
  # ### START THE PLOT
  if (!is.null(labels)) {
    labels_ <- labels
  }
  else{
    labels_ <- names(offsets)
  }
  print(table(data_new_cleaned$split_col_, data_new_cleaned$inter_))
  nums <- table(data_new_cleaned$split_col_, data_new_cleaned$inter_)
  
  q <- ggplot(pdat,
              aes(
                offset_dens,
                loc,
                color = split_col_,
                group = interaction(pdat[['inter_']], pdat[['split_col_']])
              ),
              show.legend = TRUE) +
    geom_polygon(aes(fill = split_col_, color = split_col_),
                 alpha = 0.1,
                 show.legend = TRUE) +
    geom_segment( # Q1 Q3
      data = sums,
      aes(
        x = offset,
        y = loc,
        xend = offset_dens,
        yend = loc,
        color = split_col_
      ),
      size = 1.0,
      inherit.aes = FALSE
    ) +
    geom_segment( # SUMS MEAN
      data = sums_mean,
      aes(
        x = offset,
        y = loc,
        xend = offset_dens,
        yend = loc,
        color = split_col_
      ),
      size = 1.5,
      colour = "black",
      linetype =1, # solid line
      inherit.aes = FALSE
    ) +
    geom_segment(
      data = sums2,
      aes(
        x = offset,
        y = loc,
        xend = offset_dens,
        yend = loc,
        color = split_col_
      ),
      inherit.aes = FALSE
    ) +
    scale_x_continuous(name = xlab,
                       breaks = unname(offsets),
                       labels = labels_) +
    scale_y_continuous(name = ylab) +
    ggtitle(label = main) +
    coord_cartesian(xlim = NULL, ylim = ylim) +
    scale_color_manual(values = colours) +
    scale_fill_manual(values = colours);q
  #xlab(xlab) +
  
  guides(colour = guide_legend(override.aes = list(alpha = 0.1)))
  if (include_counts == TRUE) {
    for (i in  unname(offsets)) {
      q <-
        q + annotate(
          "text",
          x = i,
          y = nums_y,
          label = paste0(nums[1, (i + 1)], " : ", nums[2, (i + 1)]),
          size = geom.text.size,
          color = "black",
          family = "ArialMT"
        )
    }
  }
  ## adds some final touches
  if (theme == "prism_style") {
    q <- q + theme_bw() +
      theme(
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line.x = element_line(color = "black", size = 0.5),
        axis.line.y = element_line(color = "black", size = 0.5),
        #axis.text.x = element_blank(),
        #axis.text.y=element_text(size=14, family ="ArialMT"),
        #axis.title = element_text(size = 14, family ="ArialMT"),
        #axis.text = element_text(size = 14, family ="ArialMT"),
        legend.position = position,
        # taken this out for plots
        #legend.key.size = unit(0.4, "cm"),
        #legend.key = element_blank(),
        legend.title = element_blank()
      )
  }
  else if (theme == "minimal") {
    q <- q + theme_minimal(base_size = base_size) +
      theme(legend.title = element_blank(),
            legend.position = position)
  }
  else if (theme == "bw") {
    q <- q + theme_bw(base_size = base_size) +
      theme(legend.title = element_blank(),
            legend.position = position)
  }
  else if (theme == "classic") {
    q <- q + theme_classic(base_size = base_size) +
      theme(legend.title = element_blank(),
            legend.position = position)
  }
  else if (theme == "light") {
    q <- q + theme_light(base_size = base_size) +
      theme(legend.title = element_blank(),
            legend.position = position)
  }
  return(q)
}
