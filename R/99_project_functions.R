# Data distribution functions ------------------------------------------------

# Basic count plot of 1 variable, stratified on another, eg condition or tumor
count_plot <- function(data, col_name, x_label_str, stratify_col, legend_str) {
  title_string <- str_c("Distribution of", x_label_str, "stratified on", legend_str , sep = " ")
  plot <- data %>%
    ggplot(mapping = aes(x = {{col_name}},
                         y = (..count..)/sum(..count..)*100,
                         fill = {{stratify_col}})) +
    geom_bar(alpha=0.5, position = position_dodge(width = 0.95)) +
    labs(x = x_label_str,
         y = "Percentage",
         fill = legend_str) +
    ggtitle(title_string) +
    theme_minimal(base_family = "Avenir") 
  
  return(plot)
}

# Basic box plot of 1 variable, stratified on another, eg condition or tumor
box_plot <- function(data, col_name, x_label_str, stratify_col, legend_str) {
  title_string <- str_c("Boxplot of", x_label_str, "stratified on", legend_str , sep = " ")
  plot <- data %>%
    ggplot(mapping = aes(x = {{col_name}}, 
                         fill = {{stratify_col}})) +
    geom_boxplot(alpha=0.5) +
    labs(x = x_label_str, 
         fill = legend_str) +
    ggtitle(title_string) +
    theme_minimal(base_family = "Avenir") 
  
  return(plot)
}

# Basic violin plot of 1 variable, stratified on another, eg condition or tumor
violin_plot <- function(data, col_name, y_label_str, stratify_col, stratify_str) {
  title_string <- str_c("Violinplot of", y_label_str, "stratified on", stratify_str , sep = " ")
  plot <- data %>%
    ggplot(mapping = aes(x = {{stratify_col}},
                         y = {{col_name}},
                         fill = {{stratify_col}})) +
    geom_violin(alpha=0.5) +
    labs(x = stratify_str,
         y = y_label_str) +
    ggtitle(title_string) +
    theme_minimal(base_family = "Avenir") +
    theme(legend.position = "none",
          axis.text.y = element_text(size = 13, hjust = 1, vjust = 1),
          axis.title = element_text(size = 13, hjust = 0.5, vjust = 1),
          axis.text.x = element_text(size = 12, hjust = 1, vjust = 1))
  
  return(plot)
}


# Densitogram function for common plots
densitogram_plot <- function(data, col_name, x_label_str, stratify_col, legend_str) {
  plot <- data %>%
    ggplot(mapping = aes(x = {{col_name}}, 
                         fill = {{stratify_col}})) +
    geom_density(alpha=0.5) +
    labs(x = x_label_str, 
         fill = legend_str) +
    theme_minimal(base_family = "Avenir") 
  
  return(plot)
}

# Basic histogram of 1 variable, stratified on another, eg condition or tumor
hist_plot <- function(data, col_name, x_label_str, stratify_col, legend_str) {
  title_string <- legend_str
  plot <- data %>%
    ggplot(mapping = aes(x = {{col_name}}, 
                         fill = {{stratify_col}})) +
    geom_histogram(binwidth = 0.1, alpha = 0.5) +
    ggtitle(title_string) +
    theme_minimal(base_family = "Avenir") +
    theme(legend.title = element_text(size = 7), legend.position = "bottom", legend.box = "horizontal") +
    labs(fill=legend_str)
  
  return(plot)
}


  # MCA analysis function -----------------------------------------------
comb_countour_plot <- function(df, x, y, grouping, legend_str){
  
  p_main <-  ggplot(df, 
                    aes_string(x = x, 
                               y = y, 
                               col = grouping )) +
    geom_point(alpha = 0.5) +
    geom_density_2d() +
    labs(x = "MCA 1",
         y = "MCA 2",
         col = legend_str) +
    theme_minimal() +
    theme(legend.position = "bottom", 
          axis.text.x= element_text(size = 9, 
                                    hjust = 1, 
                                    vjust = 1), 
          axis.text.y= element_text(size = 9, 
                                    hjust = 1, 
                                    vjust = 1),
          legend.text = element_text(size = 9.5), 
          axis.title= element_text(size = 11, 
                                   hjust = 0.5, 
                                   vjust = 1), 
          plot.title = element_text(size = 13, 
                                    hjust = 0.5),
          plot.subtitle = element_text(size = 11, 
                                       hjust = 0.5))
  
  p_den1 <- ggplot(df, aes_string(x = x, fill = grouping ))+
    geom_density(alpha = 0.5)+
    theme_void() +
    theme(legend.position = "none")
  
  p_den2 <- ggplot(df, aes_string(x = y, fill = grouping ))+
    geom_density(alpha = 0.5)+
    theme_void() +
    theme(legend.position = "none")+
    coord_flip()
  
  p_den1 + plot_spacer() + p_main + p_den2 + 
    plot_layout(ncol = 2, 
                nrow = 2, 
                widths = c(4, 1), 
                heights = c(1, 4))
}




