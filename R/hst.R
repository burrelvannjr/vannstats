#' Simplified Histogram
#'
#' This function plots a histogram (hst) on a given data frame, and uses simplified calls within the function to parse the histogram by up to variables.
#' @import ggplot2 dplyr purrr
#' @importFrom stats IQR density
#' @param df data frame to read in.
#' @param var1 the variable of interest that should be plotted.
#' @param by1 a grouping variable by which the histogram for \code{var1} should be parsed.
#' @param by2 a potential second grouping variable by which the histogram for \code{var1} (already parsed by \code{by1}) should be parsed.
#' @return This function returns the histogram.
#' @examples
#' data <- mtcars
#'
#' hst(data,mpg,cyl)
#' @export


hst <- function(df, var1, by1, by2){
  v1 <- NULL #necessary for removing the "undefined global function" warning
  density1 <- NULL
  bygroups <- length(match.call())-3
  n1 <- deparse(substitute(var1))
  n1 <- as.character(n1)
  if(bygroups==-1) {
    title <- paste0("Histogram of '", deparse(substitute(df)), "'")
    labx <- deparse(substitute(df))
    df <- as.data.frame(df)
    df <- df %>%
      mutate(group = "group")
    names(df) <- c("v1","group")
    n0 <- "v1"
    n0 <- as.character(n0)
    dens = split(df, df$group) %>%
      map_df(~ tibble(v1=seq(min(.x[[n0]],na.rm=T), max(.x[[n0]],na.rm=T), length=1000),
                      density1=dnorm(x=v1, mean=mean(.x[[n0]],na.rm=T), sd=sd(.x[[n0]],na.rm=T))),
             .id="group")
    b1 <- df
    b1 <- b1[,1]
    #bins <- ((2 * (IQR(b1, na.rm=T))) / (length(b1)^(1/(length(b1)))))
    bins <- diff(range(b1, na.rm=T)) / (2 * IQR(b1, na.rm=T) / length(b1)^(1/3))
    bw <- (2 * IQR(b1, na.rm=T) / length(b1)^(1/3))
    dens <- dens %>% mutate(density=(density1*bw*nrow(df))) #newheight is yheight * bw * length(df)
    p <- ggplot2::ggplot(data = df, aes(x=v1)) +
      geom_histogram(color="black", fill="lightgrey", binwidth = bw) +
      facet_null() +
      geom_line(data=dens, aes(x=v1, y=(density)), colour="black") +
      ggtitle(title) + xlab(labx) +
      theme_classic() +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),
            axis.line = element_line(colour = "black"), axis.text.x = element_text(vjust=0.5, colour="#000000"),
            axis.text.y = element_text(face="bold", colour="#000000"), plot.title = element_text(hjust = 0.5, lineheight=1.5, face="bold"))
  }
  if(bygroups==0) {
    title <- paste0("Histogram of '", deparse(substitute(var1)), "'")
    df <- df %>%
      mutate(group = "group")
    dens = split(df, df$group) %>%
      map_df(~ tibble(var1=seq(min(.x[[n1]],na.rm=T), max(.x[[n1]],na.rm=T), length=1000),
                      density1=dnorm(x=var1, mean=mean(.x[[n1]],na.rm=T), sd=sd(.x[[n1]],na.rm=T))),
             .id="group")
    b1 <- df %>% dplyr::select({{ var1 }})
    b1 <- b1[,1]
    #bins <- ((2 * (IQR(b1, na.rm=T))) / (length(b1)^(1/(length(b1)))))
    bins <- diff(range(b1, na.rm=T)) / (2 * IQR(b1, na.rm=T) / length(b1)^(1/3))
    bw <- (2 * IQR(b1, na.rm=T) / length(b1)^(1/3))
    dens <- dens %>% mutate(density=(density1*bw*nrow(df))) #newheight is yheight * bw * length(df)
    p <- ggplot2::ggplot(data = df, aes(x={{ var1 }})) +
      geom_histogram(color="black", fill="lightgrey", binwidth = bw) +
      facet_null() +
      geom_line(data=dens, aes(x=var1, y=(density)), colour="black") +
      ggtitle(title) +
      theme_classic() +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),
            axis.line = element_line(colour = "black"), axis.text.x = element_text(vjust=0.5, colour="#000000"),
            axis.text.y = element_text(face="bold", colour="#000000"), plot.title = element_text(hjust = 0.5, lineheight=1.5, face="bold"))
  }

  if(bygroups==1) {
    df <- df %>%
      mutate(group = {{ by1 }})
    title <- paste0("Histogram of '", deparse(substitute(var1)),"' by '", deparse(substitute(by1)), "'")
    #print(bygroups)
    dens = split(df, df$group) %>%
      map_df(~ tibble(var1=seq(min(.x[[n1]],na.rm=T), max(.x[[n1]],na.rm=T), length=1000),
                      density1=dnorm(x=var1, mean=mean(.x[[n1]],na.rm=T), sd=sd(.x[[n1]],na.rm=T))),
             .id="group")
    b1 <- df %>% dplyr::select({{ var1 }})
    b1 <- b1[,1]
    #bins <- ((2 * (IQR(b1, na.rm=T))) / (length(b1)^(1/(length(b1)))))
    bins <- diff(range(b1, na.rm=T)) / (2 * IQR(b1, na.rm=T) / length(b1)^(1/3))
    bw <- (2 * IQR(b1, na.rm=T) / length(b1)^(1/3))
    dens <- dens %>% mutate(density=(density1*bw*nrow(df))) #newheight is yheight * bw * length(df)
    p <- ggplot2::ggplot(data = df, aes(x={{ var1 }})) +
      geom_histogram(color="black", fill="lightgrey", binwidth = bw) +
      facet_wrap(~group) +
      geom_line(data=dens, aes(x=var1, y=(density)), colour="black") +
      ggtitle(title) +
      theme_classic() +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),
            axis.line = element_line(colour = "black"), axis.text.x = element_text(vjust=0.5, colour="#000000"),
            axis.text.y = element_text(face="bold", colour="#000000"), plot.title = element_text(hjust = 0.5, lineheight=1.5, face="bold"))
  }

  if(bygroups==2) {
    df <- df %>%
      mutate(group = paste0({{ by1 }},", ",{{ by2}}))
    title <- paste0("Histogram of '", deparse(substitute(var1)),"' by '", deparse(substitute(by1)),"' and '", deparse(substitute(by2)), "'")
    #print(bygroups)
    dens = split(df, df$group) %>%
      map_df(~ tibble(var1=seq(min(.x[[n1]],na.rm=T), max(.x[[n1]],na.rm=T), length=1000),
                      density1=dnorm(x=var1, mean=mean(.x[[n1]],na.rm=T), sd=sd(.x[[n1]],na.rm=T))),
             .id="group")
    b1 <- df %>% dplyr::select({{ var1 }})
    b1 <- b1[,1]
    #bins <- ((2 * (IQR(b1, na.rm=T))) / (length(b1)^(1/(length(b1)))))
    bins <- diff(range(b1, na.rm=T)) / (2 * IQR(b1, na.rm=T) / length(b1)^(1/3))
    bw <- (2 * IQR(b1, na.rm=T) / length(b1)^(1/3))
    dens <- dens %>% mutate(density=(density1*bw*nrow(df))) #newheight is yheight * bw * length(df)
    p <- ggplot2::ggplot(data = df, aes(x={{ var1 }})) +
      geom_histogram(color="black", fill="lightgrey", binwidth = bw) +
      facet_wrap(~group) +
      geom_line(data=dens, aes(x=var1, y=(density)), colour="black") +
      ggtitle(title) +
      theme_classic() +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),
            axis.line = element_line(colour = "black"), axis.text.x = element_text(vjust=0.5, colour="#000000"),
            axis.text.y = element_text(face="bold", colour="#000000"), plot.title = element_text(hjust = 0.5, lineheight=1.5, face="bold"))
  }
  #df$group
  #print(df)
  return(p)
}



