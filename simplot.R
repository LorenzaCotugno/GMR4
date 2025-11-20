library(dplyr)
library(ggplot2)
library(tidyr)
library(knitr)
library(kableExtra)

# First plot

# R = 6
res_list <- mget(paste0("result_", c(1,3,5,7,9,11,13,15,17)))
combined <- bind_rows(res_list, .id = "Study") %>%
  filter(ResponseSize == 6) %>%
  mutate(
    SampleSize = factor(SampleSize, levels = c(250, 500, 1000)),
    NoiseLevel = factor(NoiseLevel, levels = c(10, 50, 200))
  )

fdr_long_all <- combined %>%
  select(SampleSize, NoiseLevel, FDR_Min, FDR_1se, FDR_2se, FDR_3se) %>%
  pivot_longer(
    cols      = starts_with("FDR_"),
    names_to  = "Cutoff",
    values_to = "FDR"
  ) %>%
  mutate(
    Cutoff = factor(
      Cutoff,
      levels = c("FDR_Min", "FDR_1se", "FDR_2se", "FDR_3se"),
      labels = c("Min", "1SE", "2SE", "3SE")
    )
  )

ggplot(fdr_long_all, aes(x = Cutoff, y = FDR, fill = Cutoff)) +
  geom_boxplot() +
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
  facet_grid(
    rows = vars(SampleSize),
    cols = vars(NoiseLevel),
    labeller = labeller(
      SampleSize = function(x) paste0("n = ", x),
      NoiseLevel  = function(x) paste0("noise = ", x)
    )
  ) +
  scale_fill_discrete(name = expression(lambda)) +
  labs(
    title = expression(paste("Distribuzione di FDR per ", lambda, " (R = 6)")),
    x     = expression(lambda),
    y     = "FDR"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.background = element_rect(fill = "gray94", color = "white", size = 0.6),
    panel.grid.major = element_line(color = "white", size = 0.6),
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "gray94", color = NA),
    strip.text       = element_text(face = "plain"),
    panel.spacing.y  = unit(1, "cm"),
    legend.position  = "bottom"
  )

# R = 12
res_list <- mget(paste0("result_", c(2,4,6,8,10,12,14,16,18)))
combined <- bind_rows(res_list, .id = "Study") %>%
  filter(ResponseSize == 12) %>%
  mutate(
    SampleSize = factor(SampleSize, levels = c(250, 500, 1000)),
    NoiseLevel = factor(NoiseLevel, levels = c(10, 50, 200))
  )

fdr_long_all <- combined %>%
  select(SampleSize, NoiseLevel, FDR_Min, FDR_1se, FDR_2se, FDR_3se) %>%
  pivot_longer(
    cols      = starts_with("FDR_"),
    names_to  = "Cutoff",
    values_to = "FDR"
  ) %>%
  mutate(
    Cutoff = factor(
      Cutoff,
      levels = c("FDR_Min", "FDR_1se", "FDR_2se", "FDR_3se"),
      labels = c("Min", "1SE", "2SE", "3SE")
    )
  )

ggplot(fdr_long_all, aes(x = Cutoff, y = FDR, fill = Cutoff)) +
  geom_boxplot() +
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
  facet_grid(
    rows = vars(SampleSize),
    cols = vars(NoiseLevel),
    labeller = labeller(
      SampleSize = function(x) paste0("n = ", x),
      NoiseLevel  = function(x) paste0("noise = ", x)
    )
  ) +
  scale_fill_discrete(name = expression(lambda)) +
  labs(
    title = expression(paste("Distribuzione di FDR per ", lambda, " (R = 12)")),
    x     = expression(lambda),
    y     = "FDR"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.background = element_rect(fill = "gray94", color = "white", size = 0.6),
    panel.grid.major = element_line(color = "white", size = 0.6),
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "gray94", color = NA),
    strip.text       = element_text(face = "plain"),
    panel.spacing.y  = unit(1, "cm"),
    legend.position  = "bottom"
  ) 


res_list <- mget(paste0("result_", 1:18), inherits = TRUE)

combined <- bind_rows(res_list, .id = "Study") %>%
  filter(ResponseSize %in% c(6, 12)) %>%
  mutate(
    Study      = as.factor(Study),
    SampleSize = factor(SampleSize, levels = c(250, 500, 1000)),
    NoiseLevel = factor(NoiseLevel, levels = c(10, 50, 200))
  )

noise_r_levels <- c(
  "noise = 10 & R = 6",
  "noise = 10 & R = 12",
  "noise = 50 & R = 6",
  "noise = 50 & R = 12",
  "noise = 200 & R = 6",
  "noise = 200 & R = 12"
)

combined <- combined %>%
  mutate(
    Noise_R = factor(
      paste0("noise = ", NoiseLevel, " & R = ", ResponseSize),
      levels = noise_r_levels
    )
  )

fdr_long_all <- combined %>%
  select(SampleSize, Noise_R, FDR_Min, FDR_1se, FDR_2se, FDR_3se) %>%
  pivot_longer(
    cols      = starts_with("FDR_"),
    names_to  = "Cutoff",
    values_to = "FDR"
  ) %>%
  mutate(
    Cutoff = factor(
      Cutoff,
      levels = c("FDR_Min", "FDR_1se", "FDR_2se", "FDR_3se"),
      labels = c("Min", "1SE", "2SE", "3SE")
    )
  )

p_fdr <- ggplot(fdr_long_all, aes(x = Cutoff, y = FDR, fill = Cutoff)) +
  geom_boxplot() +
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
  facet_grid(
    rows = vars(SampleSize),
    cols = vars(Noise_R),
    labeller = labeller(
      SampleSize = function(x) paste0("n = ", x)
    )
  ) +
  scale_fill_discrete(name = expression(lambda)) +
  labs(
    # nessun titolo per restare compatti
    x = expression(lambda),
    y = "FDR"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.background = element_rect(fill = "gray94", color = "white", size = 0.6),
    panel.grid.major = element_line(color = "white", size = 0.6),
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "gray94", color = NA),
    strip.text       = element_text(face = "plain"),
    panel.spacing.y  = unit(1, "cm"),
    panel.spacing.x  = unit(0.6, "cm"),
    legend.position  = "bottom"
  )

p_fdr

#---


# Second Plot

# R = 6

res_list_6 <- mget(paste0("result_", c(1,3,5,7,9,11,13,15,17)))
dat6 <- bind_rows(res_list_6, .id = "Study") %>%
  filter(ResponseSize == 6) %>%
  mutate(
    SampleSize = factor(SampleSize, levels = c(250, 500, 1000)),
    NoiseLevel = factor(NoiseLevel, levels = c(10, 50, 200))
  )


fdr_comp6 <- dat6 %>%
  select(SampleSize, NoiseLevel, starts_with("FDR_N_"), starts_with("FDR_O_")) %>%
  pivot_longer(
    cols          = starts_with("FDR_"),
    names_pattern = "FDR_([NO])_(.*)",
    names_to      = c("Type", "Cutoff"),
    values_to     = "FDR"
  ) %>%
  mutate(
    # map N/O in "Numerical"/"Ordinal"
    Type   = recode(Type, N = "Numerical", O = "Ordinal"),
    Cutoff = factor(
      toupper(Cutoff),
      levels = c("MIN", "1SE", "2SE", "3SE"),
      labels = c("Min", "1SE", "2SE", "3SE")
    )
  )

ggplot(fdr_comp6, aes(x = Cutoff, y = FDR, fill = Type)) +
  geom_boxplot(position = position_dodge(width = 0.8), color = "black") +
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
  facet_grid(
    rows = vars(SampleSize),
    cols = vars(NoiseLevel),
    labeller = labeller(
      SampleSize = function(x) paste0("n = ", x),
      NoiseLevel = function(x) paste0("noise = ", x)
    )
  ) +
  scale_fill_manual(
    name   = "Variable Type",
    values = c("Numerical" = "#E41A1C", "Ordinal"   = "#377EB8")
  ) +
  labs(
    x = expression(lambda),
    y = "FDR"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.background   = element_rect(fill = "gray94", color = NA),
    panel.grid.major   = element_line(color = "white"),
    panel.grid.minor   = element_blank(),
    plot.background    = element_rect(fill = "white", color = NA),
    strip.background   = element_rect(fill = "gray94", color = NA),
    strip.text         = element_text(face = "plain"),
    panel.spacing.y    = unit(1, "cm"),
    legend.position    = "bottom"
  )


# R = 12

res_list_12 <- mget(paste0("result_", c(2,4,6,8,10,12,14,16,18)))
dat12 <- bind_rows(res_list_12, .id = "Study") %>%
  filter(ResponseSize == 12) %>%
  mutate(
    SampleSize = factor(SampleSize, levels = c(250, 500, 1000)),
    NoiseLevel = factor(NoiseLevel, levels = c(10, 50, 200))
  )

fdr_comp12 <- dat12 %>%
  select(SampleSize, NoiseLevel, starts_with("FDR_N_"), starts_with("FDR_O_")) %>%
  pivot_longer(
    cols          = starts_with("FDR_"),
    names_pattern = "FDR_([NO])_(.*)",
    names_to      = c("Type", "Cutoff"),
    values_to     = "FDR"
  ) %>%
  mutate(
    # map N/O in "Numerical"/"Ordinal"
    Type   = recode(Type, N = "Numerical", O = "Ordinal"),
    Cutoff = factor(
      toupper(Cutoff),
      levels = c("MIN", "1SE", "2SE", "3SE"),
      labels = c("Min", "1SE", "2SE", "3SE")
    )
  )

ggplot(fdr_comp12, aes(x = Cutoff, y = FDR, fill = Type)) +
  geom_boxplot(position = position_dodge(width = 0.8), color = "black") +
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
  facet_grid(
    rows = vars(SampleSize),
    cols = vars(NoiseLevel),
    labeller = labeller(
      SampleSize = function(x) paste0("n = ", x),
      NoiseLevel  = function(x) paste0("noise = ", x)
    )
  ) +
  scale_fill_manual(
    name   = "Variable Type",
    values = c("Numerical" = "#E41A1C", "Ordinal" = "#377EB8")
  ) +
  labs(
    x = expression(lambda),
    y = "FDR"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.background = element_rect(fill  = "gray94", color = "white", size = 0.6),
    panel.grid.major = element_line(color = "white", size = 0.6),
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill  = "gray94", color = NA),
    strip.text       = element_text(face  = "plain"),
    panel.spacing    = unit(1, "cm"),
    legend.position  = "bottom"
  )


#--

res_list_6  <- mget(paste0("result_", c(1,3,5,7,9,11,13,15,17)))
dat6 <- bind_rows(res_list_6, .id = "Study") %>%
  filter(ResponseSize == 6) %>%
  mutate(
    SampleSize = factor(SampleSize, levels = c(250, 500, 1000)),
    NoiseLevel = factor(NoiseLevel, levels = c(10, 50, 200))
  )

res_list_12 <- mget(paste0("result_", c(2,4,6,8,10,12,14,16,18)))
dat12 <- bind_rows(res_list_12, .id = "Study") %>%
  filter(ResponseSize == 12) %>%
  mutate(
    SampleSize = factor(SampleSize, levels = c(250, 500, 1000)),
    NoiseLevel = factor(NoiseLevel, levels = c(10, 50, 200))
  )

pivot_fdr <- function(dat) {
  dat %>%
    select(SampleSize, NoiseLevel, ResponseSize, starts_with("FDR_N_"), starts_with("FDR_O_")) %>%
    pivot_longer(
      cols          = starts_with("FDR_"),
      names_pattern = "FDR_([NO])_(.*)",
      names_to      = c("Type", "Cutoff"),
      values_to     = "FDR"
    ) %>%
    mutate(
      Type   = recode(Type, N = "Numerical", O = "Ordinal"),
      Cutoff = factor(
        toupper(Cutoff),
        levels = c("MIN", "1SE", "2SE", "3SE"),
        labels = c("Min", "1SE", "2SE", "3SE")
      ),
      ResponseSize = factor(ResponseSize, levels = c(6, 12))
    )
}

fdr_comp6  <- pivot_fdr(dat6)
fdr_comp12 <- pivot_fdr(dat12)

fdr_all <- bind_rows(fdr_comp6, fdr_comp12) %>%
  mutate(
    Col = factor(
      paste0("noise = ", NoiseLevel, " & R = ", ResponseSize),
      levels = c(
        "noise = 10 & R = 6", "noise = 10 & R = 12",
        "noise = 50 & R = 6", "noise = 50 & R = 12",
        "noise = 200 & R = 6","noise = 200 & R = 12"
      )
    )
  )

p_all <- ggplot(fdr_all, aes(x = Cutoff, y = FDR, fill = Type)) +
  geom_boxplot(position = position_dodge(width = 0.8), color = "black") +
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
  facet_grid(
    rows = vars(SampleSize),   # 3 righe: n = 250, 500, 1000
    cols = vars(Col)           # 6 colonne combinate: noise & R
  ) +
  scale_fill_manual(
    name   = "Variable Type",
    values = c("Numerical" = "#E41A1C", "Ordinal" = "#377EB8")
  ) +
  labs(
    x = expression(lambda),
    y = "FDR"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.background = element_rect(fill = "gray94", color = "white", size = 0.6),
    panel.grid.major = element_line(color = "white", size = 0.6),
    panel.grid.minor = element_blank(),
    plot.background  = element_rect(fill = "white", color = NA),
    strip.background = element_rect(fill = "gray94", color = NA),
    strip.text       = element_text(face = "plain"),
    panel.spacing.y  = unit(1, "cm"),
    panel.spacing.x  = unit(0.6, "cm"),
    legend.position  = "bottom"
  )

p_all

#---

# TDR

res_list <- mget(paste0("result_", 1:18), inherits = TRUE)

combined <- bind_rows(res_list, .id = "Study") %>%
  filter(ResponseSize %in% c(6, 12)) %>%
  mutate(
    Study        = as.factor(Study),
    SampleSize   = factor(SampleSize, levels = c(250, 500, 1000)),
    NoiseLevel   = factor(NoiseLevel, levels = c(10, 50, 200)),
    ResponseSize = factor(ResponseSize, levels = c(6, 12))
  )

ensure_tdr <- function(df) {
  has_tdr <- any(grepl("^TDR_", names(df)))
  if (!has_tdr) {
    fdr_cols <- grep("^FDR_", names(df), value = TRUE)
    for (nm in fdr_cols) {
      tdr_nm <- sub("^FDR", "TDR", nm)
      df[[tdr_nm]] <- 1 - df[[nm]]
    }
  }
  df
}
combined <- ensure_tdr(combined)

tdr_wide <- combined %>%
  select(ResponseSize, SampleSize, NoiseLevel, starts_with("TDR_")) %>%
  pivot_longer(
    cols      = starts_with("TDR_"),
    names_to  = "Metric",
    values_to = "TDR"
  ) %>%
  mutate(
    Cutoff = toupper(sub("^TDR_(?:[A-Z]_)?", "", Metric)),
    Cutoff = factor(Cutoff,
                    levels = c("MIN", "1SE", "2SE", "3SE"),
                    labels = c("Min", "1SE", "2SE", "3SE"))
  ) %>%
  filter(!is.na(Cutoff)) %>%
  group_by(ResponseSize, SampleSize, NoiseLevel, Cutoff) %>%
  summarise(
    Mean = mean(TDR, na.rm = TRUE),
    SD   = sd(TDR,   na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(MeanSD = sprintf("%.2f (%.2f)", Mean, SD)) %>%
  select(-Mean, -SD) %>%
  tidyr::pivot_wider(
    names_from  = Cutoff,
    values_from = MeanSD
  ) %>%
  transmute(
    `n`     = SampleSize,
    `noise` = NoiseLevel,
    `R`     = ResponseSize,
    Min, `1SE`, `2SE`, `3SE`
  ) %>%
  arrange(R, noise, n)

kable(tdr_wide,
      format  = "latex",
      booktabs= TRUE,
      caption = "True Discovery Rate (TDR) — media (sd) per n, noise, R e cutoff",
      align   = c("c","c","c","c","c","c","c"))


