library(dplyr)
library(ggplot2)
library(gridExtra)
library(extrafont)
loadfonts(device = "win")
library(patchwork)
library(ggtext)



summary.function <- function(object, penalty, K) { 
  newtable <- object %>%
    dplyr::group_by(!!rlang::sym(penalty)) %>%
    dplyr::summarise(
      APE = mean(PEtotal, na.rm = TRUE),
      SD_PE = sd(PEtotal, na.rm = TRUE),
      SEPE = SD_PE / sqrt(K)
    ) %>%
    dplyr::mutate(
      APE_formatted = format(APE, digits = 3, nsmall = 2)
    ) %>%
    dplyr::rename(lambda = !!rlang::sym(penalty))
  return(newtable)
}



# New Version for having lambda min 
find_lambda <- function(summary_table) {
  APE <- as.numeric(summary_table$APE)
  SEPE <- as.numeric(summary_table$SEPE)
  lambda <- summary_table$lambda
  
  lambda_min <- lambda[which.min(APE)]
  threshold <- min(APE, na.rm = TRUE) + SEPE[which.min(APE)]
  lambda_1se <- max(lambda[APE <= threshold])
  # return(lambda_min)
return(list(lambda_min = lambda_min, lambda_1se = lambda_1se))
}


#########
# Extending for lambda2se

find_lambda <- function(summary_table) {
  APE <- as.numeric(summary_table$APE)
  SEPE <- as.numeric(summary_table$SEPE)
  lambda <- summary_table$lambda
  
  idx_min <- which.min(APE)
  lambda_min <- lambda[idx_min]
  threshold_1se <- APE[idx_min] + SEPE[idx_min]
  threshold_2se <- APE[idx_min] + 2 * SEPE[idx_min]
  
  lambda_1se <- max(lambda[APE <= threshold_1se])
  lambda_2se <- max(lambda[APE <= threshold_2se])
  
  return(list(
    lambda_min = lambda_min,
    lambda_1se = lambda_1se,
    lambda_2se = lambda_2se
  ))
}


plotxval <- function(object, s_label = NULL, show_y = TRUE,
                     lambda1se = NULL, lambda2se = NULL, lambda3se = NULL,
                     ylim = NULL) {
  object$APE  <- as.numeric(object$APE)
  object$SEPE <- as.numeric(object$SEPE)
  
  lambda_min <- object$lambda[which.min(object$APE)]
  print(paste("lambda_min: ", lambda_min))
  if (!is.null(lambda1se)) print(paste("lambda1se:", lambda1se))
  if (!is.null(lambda2se)) print(paste("lambda2se:", lambda2se))
  if (!is.null(lambda3se)) print(paste("lambda3se:", lambda3se))
  
  y_max <- if (!is.null(ylim)) ylim[2] else max(object$APE + object$SEPE, na.rm = TRUE)
  label_y_main    <- y_max - 0.02
  label_y_lambda1 <- y_max - 0.06
  label_y_lambda2 <- y_max - 0.10
  label_y_lambda3 <- y_max - 0.14
  
  p <- ggplot2::ggplot(object, ggplot2::aes(x = lambda, y = APE)) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = APE - SEPE, ymax = APE + SEPE),
      color = "gray", linewidth = 0.4
    ) +
    ggplot2::geom_point(color = "red", size = 2) +
    ggplot2::labs(
      x = expression(lambda),
      y = if (show_y) "Average Prediction Error" else NULL
    ) +
    ggplot2::theme_minimal(base_family = "Calibri") +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_line(
        linewidth = 0.4, linetype = "dashed", color = "gray80"
      ),
      panel.grid.minor = ggplot2::element_blank(),
      text = ggplot2::element_text(size = 13),
      axis.title.x = ggplot2::element_text(size = 14, face = "plain"),
      axis.title.y = ggplot2::element_text(size = 14, face = "plain"),
      axis.text.x  = ggplot2::element_text(angle = 45, hjust = 1)
    )
  
  if (!is.null(ylim)) {
    p <- p + ggplot2::coord_cartesian(ylim = ylim)
  }
  
  ## λ_min 
  p <- p +
    ggplot2::geom_vline(
      xintercept = lambda_min, linetype = "dotted",
      color = "black", linewidth = 0.8
    ) +
    ggplot2::annotate(
      "text",
      x = lambda_min, y = label_y_main,
      label = paste0("lambda[min]==", round(lambda_min, 2)),
      parse = TRUE,
      size = 3.2, vjust = 1.1, hjust = 0.5,
      fontface = "italic", color = "black"
    )
  
  ## λ_1SE 
  if (!is.null(lambda1se) && is.finite(lambda1se)) {
    p <- p +
      ggplot2::geom_vline(
        xintercept = lambda1se, linetype = "dotted",
        color = "#d62728", linewidth = 1
      ) +
      ggplot2::annotate(
        "text",
        x = lambda1se, y = label_y_lambda1,
        label = paste0("lambda[1*SE]==", round(lambda1se, 2)),
        parse = TRUE,
        size = 3.2, vjust = 1.1, hjust = 0.5,
        fontface = "italic", color = "#d62728"
      )
  }
  
  ## λ_2SE 
  if (!is.null(lambda2se) && is.finite(lambda2se)) {
    p <- p +
      ggplot2::geom_vline(
        xintercept = lambda2se, linetype = "dotted",
        color = "#1f77b4", linewidth = 1
      ) +
      ggplot2::annotate(
        "text",
        x = lambda2se, y = label_y_lambda2,
        label = paste0("lambda[2*SE]==", round(lambda2se, 2)),
        parse = TRUE,
        size = 3.2, vjust = 1.1, hjust = 0.5,
        fontface = "italic", color = "#1f77b4"
      )
  }
  
  ## λ_3SE 
  if (!is.null(lambda3se) && is.finite(lambda3se)) {
    p <- p +
      ggplot2::geom_vline(
        xintercept = lambda3se, linetype = "dotted",
        color = "#2ca02c", linewidth = 1
      ) +
      ggplot2::annotate(
        "text",
        x = lambda3se, y = label_y_lambda3,
        label = paste0("lambda[3*SE]==", round(lambda3se, 2)),
        parse = TRUE,
        size = 3.2, vjust = 1.1, hjust = 0.5,
        fontface = "italic", color = "#2ca02c"
      )
  }
  
  
  if (!is.null(s_label)) {
    p <- p +
      ggplot2::labs(title = s_label) +
      ggplot2::theme(
        plot.title = ggtext::element_textbox_simple(
          size = 14, color = "black", fill = "grey90", box.color = "grey80",
          halign = 0.5, linewidth = 0.5, lineheight = 1.2,
          padding = ggplot2::margin(5, 5, 5, 5),
          margin  = ggplot2::margin(0, 0, 5, 0)
        )
      )
  }
  
  print(p)
}


