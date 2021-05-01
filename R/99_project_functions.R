# Data distribution functions ------------------------------------------------

# Basic count plot of 1 variable, stratified on another, eg condition or tumor
count_plot <- function(data, col_name, x_label_str, stratify_col, legend_str) {
  title_string <- str_c("Distribution of", x_label_str, "stratified on", legend_str , sep = " ")
  plot <- data %>%
    ggplot(mapping = aes(x = {{col_name}}, 
                         fill = {{stratify_col}})) +
    geom_bar(alpha=0.5, position = position_dodge(width = 0.95)) +
    labs(x = x_label_str,
         y = "Count",
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

# Densitogram function
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



# XX functions ------------------------------------------------




