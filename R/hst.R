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
  bygroups <- length(match.call())-3
  n1 <- deparse(substitute(var1))
  n1 <- as.character(n1)
  if(bygroups==0) {
    title <- paste0("Histogram of '", deparse(substitute(var1)), "'")
    df <- df %>%
      mutate(group = "group")
    dens = split(df, df$group) %>%
      map_df(~ tibble(var1=seq(.00001*min(.x[[n1]],na.rm=T), 1.96*max(.x[[n1]],na.rm=T), length=100),
                      density=dnorm(x=var1, mean=mean(.x[[n1]],na.rm=T), sd=sd(.x[[n1]],na.rm=T))),
             .id="group")
    b1 <- df %>% dplyr::select({{ var1 }})
    b1 <- b1[,1]
    bins <- ((2 * (IQR(b1, na.rm=T))) / (length(b1)^(1/(length(b1)))))
    #bw <- ((2 * (IQR(b1, na.rm=T))) / (length(b1)^(1/3)))
    #print(((2 * (IQR(b1, na.rm=T)))))
    #print((length(b1)^(1/8)))
    p <- ggplot2::ggplot(data = df, aes(x={{ var1 }}), na.rm=T) +
      geom_histogram(color="black", fill="lightgrey", bins = bins) +
      facet_null() +
      geom_line(data=dens, aes(x=var1, y=(density*20)), colour="black") +
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
      map_df(~ tibble(var1=seq(.00001*min(.x[[n1]],na.rm=T), 1.96*max(.x[[n1]],na.rm=T), length=100),
                      density=dnorm(x=var1, mean=mean(.x[[n1]],na.rm=T), sd=sd(.x[[n1]],na.rm=T))),
             .id="group")
    b1 <- df %>% dplyr::select({{ var1 }})
    b1 <- b1[,1]
    bins <- ((2 * (IQR(b1, na.rm=T))) / (length(b1)^(1/(length(b1)))))
    p <- ggplot2::ggplot(data = df, aes(x={{ var1 }}), na.rm=T) +
      geom_histogram(color="black", fill="lightgrey", bins = bins) +
      facet_wrap(~group) +
      geom_line(data=dens, aes(x=var1, y=(density*20)), colour="black") +
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
      map_df(~ tibble(var1=seq(.00001*min(.x[[n1]],na.rm=T), 1.96*max(.x[[n1]],na.rm=T), length=100),
                      density=dnorm(x=var1, mean=mean(.x[[n1]],na.rm=T), sd=sd(.x[[n1]],na.rm=T))),
             .id="group")
    b1 <- df %>% dplyr::select({{ var1 }})
    b1 <- b1[,1]
    bins <- ((2 * (IQR(b1, na.rm=T))) / (length(b1)^(1/(length(b1)))))
    p <- ggplot2::ggplot(data = df, aes(x={{ var1 }}), na.rm=T) +
      geom_histogram(color="black", fill="lightgrey", bins = bins) +
      facet_wrap(~group) +
      geom_line(data=dens, aes(x=var1, y=(density*20)), colour="black") +
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

